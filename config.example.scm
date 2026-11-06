;; filme configuration: a list of (define name value) forms, read as data
;; by src/app-config.sld. bin/server.scm reads config.scm; the first run
;; creates it from this file, with a fresh session-secret.

;; Bind loopback only; a reverse proxy (deploy/nginx.conf.example) fronts
;; the public interface and terminates TLS.
(define http-host "127.0.0.1")
(define http-port 8072)
(define http-max-threads 32)

;; Path the app is mounted under, e.g. "/filme"; "" for the host root. The
;; reverse proxy must pass paths through unchanged.
(define url-prefix "")

;; Signs the session and unlock cookies (base64 of 32 random bytes). To
;; rotate it, list the new key first: ("new" "old"). bin/gen-secret.scm
;; prints a new key.
(define session-secret "REPLACE_WITH_BASE64_SECRET")
(define session-max-age (* 60 60 24 14)) ; 14 days

;; Cookies are sent over HTTPS only when #t. Keep #f for http://127.0.0.1
;; during development; the Debian package sets #t.
(define secure-cookies? #f)

;; The data folder: videos, metadata, users, branding. Back up this one
;; folder (plus this config). Relative paths are relative to the app.
(define data-dir "var")
(define static-dir "static")

;; The site name until an admin changes it under "Darstellung".
(define site-name "Filme")

;; Uploads are refused when less than this much disk space would remain.
(define min-free-bytes (* 1024 1024 1024))

;; Programs used for processing (must be on the PATH, or give full paths).
(define ffmpeg "ffmpeg")
(define ffprobe "ffprobe")

;; Maintenance (bin/maintenance.scm): purge unfinished uploads and the
;; trash after this many days.
(define upload-max-age-days 1)
(define trash-max-age-days 30)
