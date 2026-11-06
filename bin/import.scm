;; Usage: scm bin/import.scm [--config path] [--public|--restricted] [--tag t]... <file-or-dir>...
;;
;; Imports MP4 files (a directory: every *.mp4 / *.m4v in it) as new
;; videos: copies each into the data folder, probes it, remuxes it for
;; fast start if needed and makes the thumbnails. The title is the file
;; name; visibility is draft unless --public or --restricted is given.
;; A running server picks the new videos up on its next request.

(import (scheme base)
        (scheme char)
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

(define visibility 'draft)
(define tags '())
(define paths '())
(let loop ((as (cdr parsed)))
  (cond ((null? as) #t)
        ((string=? (car as) "--public")
         (set! visibility 'public)
         (loop (cdr as)))
        ((string=? (car as) "--restricted")
         (set! visibility 'restricted)
         (loop (cdr as)))
        ((and (string=? (car as) "--tag") (pair? (cdr as)))
         (set! tags (append tags (normalize-tags (cadr as))))
         (loop (cddr as)))
        (else (set! paths (append paths (list (car as)))) (loop (cdr as)))))
(when (null? paths)
  (cli-fail
   "usage: scm bin/import.scm [--config path] [--public|--restricted] [--tag t]... <file-or-dir>..."))

(define (video-file? p)
  (let ((l (string-downcase p)))
    (or (string-suffix? ".mp4" l) (string-suffix? ".m4v" l))))

(define (string-suffix? suf s)
  (and (>= (string-length s) (string-length suf))
       (string=? (substring s
                            (- (string-length s) (string-length suf))
                            (string-length s))
                 suf)))

(define files
  (apply append
         (map (lambda (p)
                (cond ((directory-exists? p)
                       (map (lambda (f) (string-append p "/" f))
                            (filter-list video-file? (directory-files p))))
                      ((file-exists? p) (list p))
                      (else (cli-fail (string-append "not found: " p)))))
              paths)))

(define (filter-list pred xs)
  (cond ((null? xs) '())
        ((pred (car xs)) (cons (car xs) (filter-list pred (cdr xs))))
        (else (filter-list pred (cdr xs)))))

(define (title-of path)
  (let* ((b (base-name path))
         (i (let loop ((i (- (string-length b) 1)))
              (cond ((<= i 0) #f)
                    ((char=? (string-ref b i) #\.) i)
                    (else (loop (- i 1)))))))
    (if i (substring b 0 i) b)))

(define lib (make-library data-dir))
(define failures 0)

(for-each (lambda (path)
            (if (not (sniff-mp4? path))
                (begin
                  (display (string-append "skipped (not MP4): " path "\n"))
                  (set! failures (+ failures 1)))
                (let* ((meta (video-create!
                              lib
                              `((title . ,(title-of path))
                                (original-filename . ,(base-name path))
                                (tags . ,tags)
                                (visibility . ,visibility)
                                (created-by . "import"))))
                       (id (alist-ref meta 'id)))
                  (display (string-append id "  " path " ... "))
                  (copy-file path (video-file lib id "video.mp4"))
                  (process-video! lib id)
                  (let ((m (video-ref lib id)))
                    (display (symbol->string (alist-ref m 'status)))
                    (when (alist-ref m 'error)
                      (display (string-append ": " (alist-ref m 'error))))
                    (when (alist-ref m 'warning)
                      (display (string-append " ("
                                              (alist-ref m 'warning)
                                              ")")))
                    (newline)
                    (unless (eq? (alist-ref m 'status) 'ready)
                      (set! failures (+ failures 1)))))))
          files)

(library-touch! lib)
(when (> failures 0) (exit 1))
