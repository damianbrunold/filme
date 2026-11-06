;; Tests that need ffmpeg/ffprobe on the PATH: probing, fast-start
;; remuxing, thumbnails, and a complete chunked upload through the app.
;; Run: scm tests/test_media.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scm crypto)
        (scm crypto password)
        (scm fs)
        (scm log)
        (scm module)
        (scm net http request)
        (scm net http response)
        (scm random access)
        (scm string)
        (scm system)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app media)
        (app store)
        (app users)
        (app videos)
        (app web))

(test-runner-factory scm-test-runner)
(test-begin "filme media")

(define tmp (mktempdir '(prefix . "filme-media")))
(define clip (string-append tmp "/clip.mp4"))

;; 4 s test pattern with a tone; ffmpeg writes the moov box at the end
;; unless asked otherwise, which is what processing must fix.
(test-equal 0
            (run-program (list "ffmpeg" "-v" "error" "-y" "-f" "lavfi" "-i"
                               "testsrc=duration=4:size=320x240:rate=25" "-f"
                               "lavfi" "-i" "sine=frequency=440:duration=4"
                               "-c:v" "libx264" "-pix_fmt" "yuv420p" "-c:a"
                               "aac" "-shortest" clip)))

(test-group "probe"
  (test-assert (sniff-mp4? clip))
  (test-assert "moov at the end before processing" (not (moov-first? clip)))
  (let ((info (probe-video clip)))
    (test-equal 320 (alist-ref info 'width))
    (test-equal "h264" (alist-ref info 'video-codec))
    (test-equal "aac" (alist-ref info 'audio-codec))
    (test-assert (< 3.5 (alist-ref info 'duration) 4.5)))
  (test-assert (browser-playable? "h264" "aac"))
  (test-assert (not (browser-playable? "hevc" "aac")))
  (test-error (probe-video (string-append root "/config.example.scm"))))

(define (utf8->string* b) (if (bytevector? b) (utf8->string b) b))

(define lib (make-library tmp))

(test-group "process"
  (let* ((m (video-create! lib '((title . "clip"))))
         (id (alist-ref m 'id)))
    (copy-file clip (video-file lib id "video.mp4"))
    (process-video! lib id)
    (let ((m (video-ref lib id)))
      (test-equal 'ready (alist-ref m 'status))
      (test-equal #f (alist-ref m 'warning))
      (test-equal 240 (alist-ref m 'height))
      (test-assert "moov moved to the front"
                   (moov-first? (video-file lib id "video.mp4")))
      (test-assert (file-exists? (video-file lib id "thumb.jpg")))
      (test-assert (file-exists? (video-file lib id "thumb-sm.jpg")))
      (test-assert "no temp files"
                   (not (file-exists? (video-file lib id "video.tmp.mp4")))))
    (extract-thumbnail! lib id 3.0)
    (test-assert (file-exists? (video-file lib id "thumb.jpg"))))
  (let* ((m (video-create! lib '((title . "broken"))))
         (id (alist-ref m 'id)))
    (call-with-output-file (video-file lib id "video.mp4")
      (lambda (p) (write-string "not a video" p)))
    (process-video! lib id)
    (test-equal 'failed (alist-ref (video-ref lib id) 'status))
    (test-assert (string? (alist-ref (video-ref lib id) 'error)))))

(test-group "chunked upload through the app"
  (parameterize ((password-pbkdf2-iterations 1000))
    (save-user! (string-append tmp "/users.scm")
                "ed"
                (password-hash "ed password1")
                'editor))
  (let* ((app (make-app `((url-prefix . "")
                          (static-dir . ,(string-append root "/static"))
                          (data-dir . ,tmp)
                          (session-secret .
                                          ,(base64-encode (random-bytes 32)))
                          (session-max-age . 3600)
                          (secure-cookies? . #f)
                          (process? . #f)
                          (min-free-bytes . 0))))
         (lib (app-library))
         (send (lambda (method url cookie body . headers)
                 (parameterize ((log-port (open-output-string))
                                (password-pbkdf2-iterations 1000))
                   (app (make-http-request
                         method
                         url
                         (append (if cookie (list (cons "Cookie" cookie)) '())
                                 headers)
                         body)))))
         (cookie (car (string-split-char
                       (http-response-header
                        (send "POST" "/login" #f
                              "username=ed&password=ed+password1"
                              '("Content-Type" .
                                "application/x-www-form-urlencoded"))
                        "Set-Cookie")
                       #\;)))
         (bytes (call-with-random-access-file clip
                  'read
                  (lambda (f)
                    (random-access-file-read f 0 (file-size clip)))))
         (size (bytevector-length bytes))
         (half (quotient size 2))
         (r (send "POST" "/api/uploads" cookie
                  (string-append "{\"filename\":\"Mein Clip.mp4\",\"size\":"
                                 (number->string size)
                                 "}")
                  '("Content-Type" . "application/json")))
         (id (substring (utf8->string* (http-response-body r)) 7 18)))
    (test-equal 201 (http-response-status r))
    (test-equal 200
                (http-response-status (send
                                       "PUT"
                                       (string-append "/api/uploads/" id
                                                      "?offset=0")
                                       cookie
                                       (bytevector-copy bytes 0 half))))
    (test-equal 200
                (http-response-status (send
                                       "PUT"
                                       (string-append "/api/uploads/" id
                                                      "?offset="
                                                      (number->string half))
                                       cookie
                                       (bytevector-copy bytes half size))))
    (let
        ((done (send "POST"
                     (string-append "/api/uploads/" id "/complete")
                     cookie
                     #f)))
      (test-equal 200 (http-response-status done))
      (let* ((vid (substring (utf8->string* (http-response-body done)) 10 21))
             (m (video-ref lib vid)))
        (test-equal "Mein Clip" (alist-ref m 'title))
        (test-equal 'ready (alist-ref m 'status))
        (test-equal 'draft (alist-ref m 'visibility))
        (test-equal "ed" (alist-ref m 'created-by))
        (test-assert "upload directory removed"
                     (not (directory-exists?
                           (string-append tmp "/uploads/" id))))))))

(rm tmp 'recursive 'force)
(test-end "filme media")
