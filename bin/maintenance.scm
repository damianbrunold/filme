;; Usage: scm bin/maintenance.scm [--config path]
;;
;; Nightly housekeeping (filme-maintenance.timer): deletes unfinished
;; uploads older than upload-max-age-days and trashed videos older than
;; trash-max-age-days. Safe to run while the server runs.

(import (scheme base)
        (scheme process-context)
        (scm fs)
        (scm log)
        (scm module))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app cli)
        (app config)
        (app uploads)
        (app videos))

(define parsed (cli-config root (cdr (command-line))))
(define cfg (car parsed))
(define data-dir (cli-data-dir root cfg))
(define day (* 24 3600))

(define uploads
  (purge-stale-uploads! data-dir
                        (* day (config-ref cfg 'upload-max-age-days 1))))
(define trash
  (video-purge-trash! (make-library data-dir)
                      (* day (config-ref cfg 'trash-max-age-days 30))))

(log-info "maintenance"
          (string-append "purged "
                         (number->string (length uploads))
                         " stale upload(s), "
                         (number->string (length trash))
                         " trashed video(s)"))
