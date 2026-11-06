;; (app media) — ffprobe / ffmpeg and the background processing queue.
;;
;; After an upload, a video is processed once, in a single worker thread:
;;   1. ffprobe reads duration, size and codecs (no video stream -> failed);
;;   2. if the moov atom is not at the front, ffmpeg remuxes the file with
;;      -c copy -movflags +faststart (lossless; playback starts at once);
;;   3. a poster frame at 10% becomes thumb.jpg (1280 px) and thumb-sm.jpg.
;; Every ffmpeg run has a timeout and is killed when it overruns.
;; Commands are argument lists, never shell strings.

(define-library (app media)
  (import (scheme base)
          (scheme file)
          (srfi 18)
          (scm fs)
          (scm io)
          (scm json simple)
          (scm log)
          (scm random access)
          (scm system)
          (app store)
          (app videos))
  (export browser-playable?
          enqueue-video!
          extract-thumbnail!
          ffmpeg-program
          ffprobe-program
          image->thumbnail!
          make-processor
          moov-first?
          probe-video
          process-video!
          processor-busy?
          sniff-image
          sniff-mp4?)
  (begin

    (define ffmpeg-program (make-parameter "ffmpeg"))
    (define ffprobe-program (make-parameter "ffprobe"))

    ;; --- file signatures -------------------------------------------

    (define (file-head path n)
      (call-with-random-access-file path
        'read
        (lambda (f) (random-access-file-read f 0 n))))

    (define (bytes=? bv off s)
      (and (>= (bytevector-length bv) (+ off (string-length s)))
           (let loop ((i 0))
             (or (= i (string-length s))
                 (and (= (bytevector-u8-ref bv (+ off i))
                         (char->integer (string-ref s i)))
                      (loop (+ i 1)))))))

    (define (sniff-mp4? path)
      "Syntax: (sniff-mp4? path)
Library: (app media)
Description: True if path starts like an ISO media file (an ftyp box)."
      (and (file-exists? path) (bytes=? (file-head path 12) 4 "ftyp")))

    (define (sniff-image bv)
      "Syntax: (sniff-image bytevector)
Library: (app media)
Description: 'jpeg, 'png or 'webp by the first bytes, else #f."
      (cond ((and (>= (bytevector-length bv) 3)
                  (= (bytevector-u8-ref bv 0) #xff)
                  (= (bytevector-u8-ref bv 1) #xd8)
                  (= (bytevector-u8-ref bv 2) #xff))
             'jpeg)
            ((and (>= (bytevector-length bv) 8)
                  (= (bytevector-u8-ref bv 0) #x89)
                  (bytes=? bv 1 "PNG"))
             'png)
            ((and (bytes=? bv 0 "RIFF") (bytes=? bv 8 "WEBP")) 'webp)
            (else #f)))

    (define (u32 bv i)
      (+ (* (bytevector-u8-ref bv i) 16777216)
         (* (bytevector-u8-ref bv (+ i 1)) 65536)
         (* (bytevector-u8-ref bv (+ i 2)) 256)
         (bytevector-u8-ref bv (+ i 3))))

    (define (moov-first? path)
      "Syntax: (moov-first? path)
Library: (app media)
Description: True if the top-level moov box comes before mdat, so a
  browser can start playing before it has the whole file."
      (call-with-random-access-file path
        'read
        (lambda (f)
          (let ((size (random-access-file-size f)))
            (let loop ((off 0) (n 0))
              (if (or (> (+ off 8) size) (> n 1000))
                  #f
                  (let* ((h (random-access-file-read f off 16))
                         (len32 (u32 h 0))
                         (len (cond ((= len32 1)
                                     (+ (* (u32 h 8) 4294967296) (u32 h 12)))
                                    ((= len32 0) (- size off))
                                    (else len32))))
                    (cond ((bytes=? h 4 "moov") #t)
                          ((bytes=? h 4 "mdat") #f)
                          ((< len 8) #f)
                          (else (loop (+ off len) (+ n 1)))))))))))

    ;; --- running ffmpeg ----------------------------------------------

    ;; Runs cmd with a timeout. Returns #t, or raises with the end of the
    ;; program's output as the message.
    (define (run-with-timeout cmd timeout-seconds)
      (let* ((log (string-append (mktemp '(prefix . "filme-ffmpeg")) ""))
             (h (start-program cmd `((log-file ,log))))
             (code (process-wait h (* 1000 timeout-seconds))))
        (unless code
          (process-kill h #t)
          (process-wait h 5000))
        (let ((out (if (file-exists? log) (read-file-string log) "")))
          (when (file-exists? log) (delete-file log))
          (cond ((not code) (error "ffmpeg timed out" timeout-seconds))
                ((not (= code 0))
                 (error (string-append "ffmpeg failed: " (tail out 400))))
                (else #t)))))

    (define (tail s n)
      (if (> (string-length s) n)
          (substring s (- (string-length s) n) (string-length s))
          s))

    (define (num x)
      (cond ((number? x) x)
            ((string? x) (or (string->number x) 0))
            (else 0)))

    (define (probe-video path)
      "Syntax: (probe-video path)
Library: (app media)
Description: An alist (duration width height video-codec audio-codec)
  read with ffprobe. Raises if ffprobe fails or there is no video stream."
      (let
          ((r (run-program/capture (list (ffprobe-program)
                                         "-v"
                                         "error"
                                         "-of"
                                         "json"
                                         "-show_format"
                                         "-show_streams"
                                         path))))
        (unless (and r (= (car r) 0))
          (error (string-append "ffprobe failed: "
                                (if r
                                    (tail (caddr r) 300)
                                    "not installed?"))))
        (let* ((j (json-parse (cadr r)))
               (streams (vector->list (json-ref j "streams" #())))
               (of-type (lambda (t)
                          (let loop ((ss streams))
                            (cond ((null? ss) #f)
                                  ((equal? (json-ref (car ss) "codec_type" "")
                                           t)
                                   (car ss))
                                  (else (loop (cdr ss)))))))
               (v (of-type "video"))
               (a (of-type "audio"))
               (fmt (json-ref j "format" '())))
          (unless v (error "no video stream"))
          `((duration . ,(num (json-ref fmt "duration" 0)))
            (width . ,(num (json-ref v "width" 0)))
            (height . ,(num (json-ref v "height" 0)))
            (video-codec . ,(json-ref v "codec_name" ""))
            (audio-codec . ,(if a (json-ref a "codec_name" "") ""))))))

    (define (browser-playable? video-codec audio-codec)
      "Syntax: (browser-playable? video-codec audio-codec)
Library: (app media)
Description: True if every current browser plays this codec pair in mp4."
      (and (member video-codec '("h264" "vp9" "av1"))
           (member audio-codec '("" "aac" "mp3" "opus"))
           #t))

    (define (faststart! path timeout)
      (let ((tmp (string-append (directory-name path) "/video.tmp.mp4")))
        (guard (e (#t (when (file-exists? tmp) (delete-file tmp)) (raise e)))
          (run-with-timeout (list (ffmpeg-program)
                                  "-nostdin"
                                  "-y"
                                  "-v"
                                  "error"
                                  "-i"
                                  path
                                  "-map"
                                  "0"
                                  "-c"
                                  "copy"
                                  "-movflags"
                                  "+faststart"
                                  tmp)
                            timeout)
          (unless (move-file tmp path) (error "cannot replace video file")))))

    (define (seconds->string s) (number->string (/ (round (* 1000 s)) 1000.)))

    (define (scale-jpeg! src dest width seek timeout)
      (let ((tmp (string-append (directory-name dest) "/thumb.tmp.jpg")))
        (guard (e (#t (when (file-exists? tmp) (delete-file tmp)) (raise e)))
          (run-with-timeout (append (list (ffmpeg-program)
                                          "-nostdin"
                                          "-y"
                                          "-v"
                                          "error")
                                    (if seek
                                        (list "-ss" (seconds->string seek))
                                        '())
                                    (list
                                     "-i"
                                     src
                                     "-frames:v"
                                     "1"
                                     "-vf"
                                     (string-append "scale='min("
                                                    (number->string width)
                                                    ",iw)':-2")
                                     "-q:v"
                                     "3"
                                     tmp))
                            timeout)
          (unless (file-exists? tmp) (error "ffmpeg wrote no frame"))
          (unless (move-file tmp dest) (error "cannot write thumbnail")))))

    (define (extract-thumbnail! lib id at)
      "Syntax: (extract-thumbnail! lib id at)
Library: (app media)
Description: Writes thumb.jpg and thumb-sm.jpg of video id from the frame
  at second at."
      (let ((src (video-file lib id "video.mp4")))
        (scale-jpeg! src (video-file lib id "thumb.jpg") 1280 at 120)
        (scale-jpeg! (video-file lib id "thumb.jpg")
                     (video-file lib id "thumb-sm.jpg")
                     480
                     #f
                     60)))

    (define (image->thumbnail! lib id image-path)
      "Syntax: (image->thumbnail! lib id image-path)
Library: (app media)
Description: Makes thumb.jpg and thumb-sm.jpg of video id from an image."
      (scale-jpeg! image-path (video-file lib id "thumb.jpg") 1280 #f 60)
      (scale-jpeg! (video-file lib id "thumb.jpg")
                   (video-file lib id "thumb-sm.jpg")
                   480
                   #f
                   60))

    (define (process-video! lib id)
      "Syntax: (process-video! lib id)
Library: (app media)
Description: Probes, remuxes (if needed) and thumbnails video id, and
  records the result in its metadata (status ready or failed)."
      (let ((path (video-file lib id "video.mp4")))
        (guard (e (#t
                   (let
                       ((msg (if (error-object? e)
                                 (error-object-message e)
                                 "processing failed")))
                     (log-warn "media" (string-append id ": " msg))
                     (video-update! lib id
                                    (lambda (m)
                                      (alist-set (alist-set m 'status 'failed)
                                                 'error
                                                 msg))))))
          (let* ((info (probe-video path))
                 (duration (alist-ref info 'duration 0))
                 ;; Remuxing takes seconds; allow a generous budget for
                 ;; large files on slow disks.
                 (budget (+ 300
                            (exact (floor (/ (file-size path) 2000000))))))
            (unless (moov-first? path) (faststart! path budget))
            (let ((at (alist-ref (video-ref lib id) 'thumbnail-at #f)))
              (unless (eq? at 'custom)
                (extract-thumbnail! lib id
                                    (if (number? at) at (* duration 0.1)))))
            (video-update! lib id
                           (lambda (m)
                             (let loop ((m m)
                                        (kv
                                         (append
                                          info
                                          `((size . ,(file-size path))
                                            (status . ready)
                                            (error . #f)
                                            (thumbnail-at
                                             .
                                             ,(let
                                                  ((at
                                                    (alist-ref m
                                                               'thumbnail-at
                                                               #f)))
                                                (if at at (* duration 0.1))))
                                            (warning
                                             .
                                             ,(if
                                               (browser-playable?
                                                (alist-ref info 'video-codec)
                                                (alist-ref info 'audio-codec))
                                               #f
                                               (string-append
                                                "Codec "
                                                (alist-ref info 'video-codec)
                                                "/"
                                                (alist-ref info 'audio-codec)
                                                " wird nicht von allen Browsern abgespielt."
                                                " Empfohlen: H.264/AAC.")))))))
                               (if (null? kv)
                                   m
                                   (loop (alist-set m (caar kv) (cdar kv))
                                         (cdr kv))))))
            (log-info "media" (string-append id ": ready"))))))

    ;; --- the queue -----------------------------------------------------

    (define-record-type processor
      (%make-processor lib mutex cv queue busy)
      processor?
      (lib processor-lib)
      (mutex processor-mutex)
      (cv processor-cv)
      (queue processor-queue set-processor-queue!)
      (busy processor-busy set-processor-busy!))

    (define (processor-busy? p)
      "Syntax: (processor-busy? p)
Library: (app media)
Description: True while p has work queued or running."
      (or (processor-busy p) (pair? (processor-queue p))))

    (define (make-processor lib)
      "Syntax: (make-processor lib)
Library: (app media)
Description: Starts the worker thread and queues every video still marked
  processing (an upload interrupted by a restart)."
      (let
          ((p (%make-processor lib
                               (make-mutex)
                               (make-condition-variable)
                               '()
                               #f)))
        (thread-start! (make-thread (lambda ()
                                      (let loop ()
                                        (mutex-lock! (processor-mutex p))
                                        (let wait ()
                                          (when (null? (processor-queue p))
                                            (mutex-unlock! (processor-mutex p)
                                                           (processor-cv p))
                                            (mutex-lock! (processor-mutex p))
                                            (wait)))
                                        (let ((id (car (processor-queue p))))
                                          (set-processor-queue!
                                           p
                                           (cdr (processor-queue p)))
                                          (set-processor-busy! p #t)
                                          (mutex-unlock! (processor-mutex p))
                                          (guard
                                              (e
                                               (#t
                                                (log-error "media"
                                                           "worker error")))
                                            (when (video-ref lib id)
                                              (process-video! lib id)))
                                          (set-processor-busy! p #f))
                                        (loop)))
                                    "filme-media"))
        (for-each (lambda (m)
                    (when (eq? (alist-ref m 'status) 'processing)
                      (enqueue-video! p (alist-ref m 'id))))
                  (video-list lib))
        p))

    (define (enqueue-video! p id)
      "Syntax: (enqueue-video! p id)
Library: (app media)
Description: Queues video id for processing (once)."
      (mutex-lock! (processor-mutex p))
      (unless (member id (processor-queue p))
        (set-processor-queue! p (append (processor-queue p) (list id))))
      (condition-variable-signal! (processor-cv p))
      (mutex-unlock! (processor-mutex p)))))
