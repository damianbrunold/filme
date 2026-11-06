;; Usage: scm bin/check.scm [--config path] [--fix] [--reprocess <id>|all]
;;
;; Checks the data folder: every video directory needs a readable
;; meta.scm and a video.mp4; ready videos need their thumbnails; leftover
;; temp files and directories without metadata are reported. With --fix,
;; temp files are removed and videos with missing thumbnails are
;; processed again. --reprocess runs probing, fast-start and thumbnails
;; again for one video or all of them (custom thumbnails are kept).

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (scm fs)
        (scm module))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app cli)
        (app media)
        (app store)
        (app videos))

(define parsed (cli-config root (cdr (command-line))))
(define data-dir (cli-data-dir root (car parsed)))
(define args (cdr parsed))
(define fix? (member "--fix" args))
(define reprocess
  (let ((m (member "--reprocess" args))) (and m (pair? (cdr m)) (cadr m))))

(define lib (make-library data-dir))
(define problems 0)
(define (problem . parts)
  (set! problems (+ problems 1))
  (display (apply string-append parts))
  (newline))

(define (reprocess! id)
  (display (string-append "processing " id " ... "))
  (process-video! lib id)
  (display (symbol->string (alist-ref (video-ref lib id) 'status)))
  (newline))

(cond (reprocess (for-each (lambda (m)
                             (when
                                 (or (string=? reprocess "all")
                                     (string=? reprocess (alist-ref m 'id)))
                               (reprocess! (alist-ref m 'id))))
                           (video-list lib)))
      (else (let ((root-dir (string-append data-dir "/videos")))
              (for-each (lambda (d)
                          (let ((m (video-ref lib d)))
                            (cond ((not m)
                                   (problem d
                                            ": no readable meta.scm (directory ignored)"))
                                  (else
                                   (unless
                                       (file-exists?
                                        (video-file lib d "video.mp4"))
                                     (problem d ": video.mp4 missing"))
                                   (for-each
                                    (lambda (f)
                                      (when
                                          (file-exists? (video-file lib d f))
                                        (problem d ": leftover " f)
                                        (when fix?
                                          (delete-file
                                           (video-file lib d f)))))
                                    '("video.tmp.mp4" "thumb.tmp.jpg"
                                      "meta.scm.tmp"))
                                   (when
                                       (and
                                        (eq? (alist-ref m 'status) 'ready)
                                        (not
                                         (and
                                          (file-exists?
                                           (video-file lib d "thumb.jpg"))
                                          (file-exists?
                                           (video-file lib d
                                                       "thumb-sm.jpg")))))
                                     (problem d ": thumbnail missing")
                                     (when
                                         (and
                                          fix?
                                          (file-exists?
                                           (video-file lib d "video.mp4")))
                                       (unless
                                           (eq? (alist-ref m 'thumbnail-at)
                                                'custom)
                                         (reprocess! d))))
                                   (when (eq? (alist-ref m 'status) 'failed)
                                     (problem
                                      d
                                      ": processing failed: "
                                      (or (alist-ref m 'error) "")))))))
                        (directory-directories root-dir)))))

(library-touch! lib)
(display (string-append (number->string (length (video-list lib)))
                        " video(s), "
                        (number->string problems)
                        " problem(s)\n"))
(when (> problems 0) (exit 1))
