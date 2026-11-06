;; (app web) — routes, request handlers and the middleware stack.
;;
;; make-app returns the server-level handler (lambda (req) resp):
;; bin/server.scm serves it, and the tests call it directly.
;;
;; CSRF: the session cookie is SameSite=Lax, so browsers never send it with
;; a cross-site POST/PUT/DELETE; on top of that, every state-changing
;; request whose Origin header names another host is refused (403).
;; Forms follow POST -> 303 -> GET; the outcome travels as ?m=<code>.

(define-library (app web)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 18)
          (srfi 69)
          (scm crypto password)
          (scm fs)
          (scm fs-find)
          (scm json simple)
          (scm log)
          (scm net http forms)
          (scm net http middleware)
          (scm net http multipart)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http static)
          (scm string)
          (app access)
          (app auth)
          (app media)
          (app site)
          (app store)
          (app theme)
          (app uploads)
          (app users)
          (app videos)
          (app views))
  (export app-library
          app-processor
          make-app
          upload-chunk-size)
  (begin

    (define cookie-name "filme_session")

    ;; Chunks of the resumable upload; the server's body limit must allow
    ;; one chunk (bin/server.scm passes chunk size + 4 MiB).
    (define upload-chunk-size (* 16 1024 1024))

    (define page-size 24)

    (define csp
      (string-append
       "default-src 'self'; script-src 'self'; style-src 'self'; "
       "img-src 'self' data: blob:; media-src 'self' blob:; "
       "object-src 'none'; base-uri 'self'; form-action 'self'; "
       "frame-ancestors 'none'"))

    (define (setting settings key . default)
      (let ((p (assq key settings)))
        (cond (p (cdr p))
              ((pair? default) (car default))
              (else (error "make-app: missing setting" key)))))

    ;; The last app built, for bin/ scripts and tests that need its parts.
    (define app-library (make-parameter #f))
    (define app-processor (make-parameter #f))

    ;; --- small helpers ---------------------------------------------------

    (define (redirect path . headers)
      (make-http-response 303 (cons (cons "Location" (url path)) headers) ""))

    (define (penalty) (thread-sleep! 0.5))

    (define (json-status status value)
      (make-http-response status
                          '(("Content-Type" .
                             "application/json; charset=utf-8")
                            ("Cache-Control" . "no-store"))
                          (json->string value)))

    (define (json-error status message)
      (json-status status `(("error" . ,message))))

    (define (parse-json-body req)
      (let ((body (http-request-body req)))
        (and (string? body)
             (guard (e (#t #f))
               (let ((j (json-parse body))) (and (list? j) j))))))

    (define (query req) (url-query-params (http-request-url req)))
    (define (qref req name)
      (let ((p (assoc name (query req)))) (and p (cdr p))))

    (define (client-ip req)
      (let ((xff (http-request-header req "X-Forwarded-For")))
        (if (and xff (> (string-length xff) 0))
            ;; The last entry is the one our own proxy added; earlier ones
            ;; come from the client and could be forged.
            (let ((parts (string-split-char xff #\,)))
              (string-trim-both (list-ref parts (- (length parts) 1))))
            "local")))

    (define (string-trim-both s)
      (let* ((n (string-length s))
             (a (let l ((i 0))
                  (if (and (< i n) (char-whitespace? (string-ref s i)))
                      (l (+ i 1))
                      i)))
             (b (let l ((i n))
                  (if (and (> i a) (char-whitespace? (string-ref s (- i 1))))
                      (l (- i 1))
                      i))))
        (substring s a b)))

    (define (string-search part s) (string-contains-from s part 0))

    ;; Where to go after signing in: a path of this site, never another host.
    (define (safe-next s)
      (and (string? s)
           (> (string-length s) 0)
           (< (string-length s) 2000)
           (char=? (string-ref s 0) #\/)
           (not (and (> (string-length s) 1)
                     (memv (string-ref s 1) '(#\/ #\\))))
           (let loop ((i 0))
             (or (= i (string-length s))
                 (and (char>? (string-ref s i) #\space)
                      (not (char=? (string-ref s i) #\\))
                      (loop (+ i 1)))))
           s))

    (define (base-name-of filename)
      ;; "C:\\x\\Intro.mp4" -> "Intro"
      (let* ((s (let loop ((i (- (string-length filename) 1)))
                  (cond ((< i 0) filename)
                        ((memv (string-ref filename i) '(#\/ #\\))
                         (substring filename
                                    (+ i 1)
                                    (string-length filename)))
                        (else (loop (- i 1))))))
             (dot (let loop ((i (- (string-length s) 1)))
                    (cond ((<= i 0) #f)
                          ((char=? (string-ref s i) #\.) i)
                          (else (loop (- i 1)))))))
        (if dot (substring s 0 dot) s)))

    (define (clean-filename s)
      (let ((s (if (string? s) s "video.mp4")))
        (list->string (let loop
                          ((cs (string->list
                                (if (> (string-length s) 200)
                                    (substring s 0 200)
                                    s))))
                        (cond ((null? cs) '())
                              ((or (char<? (car cs) #\space)
                                   (memv (car cs) '(#\" #\/ #\\)))
                               (cons #\_ (loop (cdr cs))))
                              (else (cons (car cs) (loop (cdr cs)))))))))

    ;; Refuses state-changing requests started by another site.
    (define (with-origin-check handler)
      (lambda (req)
        (let ((m (http-request-method req))
              (origin (http-request-header req "Origin"))
              (host (http-request-header req "Host")))
          (if (and (member m '("POST" "PUT" "DELETE" "PATCH"))
                   origin
                   (not (and host
                             (let ((i (string-search "://" origin)))
                               (string=?
                                (string-downcase
                                 (if i
                                     (substring origin
                                                (+ i 3)
                                                (string-length origin))
                                     origin))
                                (string-downcase host))))))
              (make-http-response 403
                                  '(("Content-Type" . "text/plain"))
                                  "cross-site request refused\n")
              (handler req)))))

    ;; --- failed-attempt limiter ------------------------------------------
    ;; At most `limit` failures per key within `window` seconds.

    (define (make-limiter limit window)
      (let ((table (make-hash-table equal?))
            (m (make-mutex)))
        (lambda (op key)
          (dynamic-wind
            (lambda () (mutex-lock! m))
            (lambda ()
              (let* ((now (now-seconds))
                     (recent (let loop
                                 ((ts (hash-table-ref/default table key '())))
                               (cond ((null? ts) '())
                                     ((< (- now (car ts)) window)
                                      (cons (car ts) (loop (cdr ts))))
                                     (else (loop (cdr ts)))))))
                (case op
                  ((blocked?)
                   (hash-table-set! table key recent)
                   (>= (length recent) limit))
                  ((fail!) (hash-table-set! table key (cons now recent)))
                  ((reset!) (hash-table-delete! table key)))))
            (lambda () (mutex-unlock! m))))))

    ;; --- the app ---------------------------------------------------------

    (define (make-app settings)
      "Syntax: (make-app settings)
Library: (app web)
Description: Builds the app's request handler. settings is an alist:
  url-prefix, static-dir, data-dir (absolute paths), session-secret,
  session-max-age (seconds), secure-cookies?, and optionally site-name,
  process? (#f: no background worker), min-free-bytes."
      (let* ((site (make-site (setting settings 'url-prefix)
                              (setting settings 'static-dir)))
             (prefix (site-prefix site))
             (data-dir (setting settings 'data-dir))
             (users-file (string-append data-dir "/users.scm"))
             (secret (setting settings 'session-secret))
             (secure? (setting settings 'secure-cookies?))
             (auth (make-auth cookie-name secret
                              (setting settings 'session-max-age)
                              (cookie-path site)
                              secure?
                              users-file))
             (initial-name (setting settings 'site-name "Filme"))
             (min-free (setting settings 'min-free-bytes (* 512 1024 1024)))
             (lib (make-library data-dir))
             (proc (and (setting settings 'process? #t) (make-processor lib)))
             (login-limit (make-limiter 10 600))
             (unlock-limit (make-limiter 5 600))
             (router (make-router))
             (_ (begin
                  (app-library lib)
                  (app-processor proc)
                  (ensure-dir (string-append data-dir "/uploads"))
                  (ensure-dir (settings-dir data-dir)))))

        (define (route! method path handler)
          (router-add! router method (string-append prefix path) handler))

        (define (ctx user)
          `((user . ,user)
            (settings . ,(load-settings data-dir initial-name))
            (theme-v . ,(settings-version data-dir))))

        (define (msg-of req) (notice-text (qref req "m")))

        (define (not-found user)
          (make-http-response 404
                              '(("Content-Type" . "text/html; charset=utf-8"))
                              (not-found-page (ctx user))))

        (define (current-user req) (session-user auth req))

        (define (unlocked req meta) (unlocked? secret req meta))

        (define (viewable? user req meta)
          (can-view? user meta (unlocked req meta)))

        (define (enqueue! id)
          (if proc (enqueue-video! proc id) (process-video! lib id)))

        ;; Guards: run handler with the user as a third argument.
        (define (with-role role handler)
          (lambda (req params)
            (let ((user (current-user req)))
              (cond ((has-role? user role) (handler req params user))
                    (user (make-http-response
                           403
                           '(("Content-Type" . "text/html; charset=utf-8"))
                           (not-found-page (ctx user))))
                    (else (redirect (string-append
                                     "/login?next="
                                     (url-path (http-request-url req)))))))))

        (define (api-role role handler)
          (lambda (req params)
            (let ((user (current-user req)))
              (cond ((has-role? user role) (handler req params user))
                    (user (json-error 403 "forbidden"))
                    (else (json-error 401 "not signed in"))))))

        (define (signed-in handler) (with-role 'viewer handler))

        (define (own-upload user params)
          (let ((u (upload-ref data-dir (params-ref params "id"))))
            (and u (equal? (alist-ref u 'owner) (user-name user)) u)))

        (define (thumb-json meta)
          `(("thumb"
             .
             ,(string-append (url (string-append "/media/"
                                                 (alist-ref meta 'id)
                                                 "/thumb.jpg"))
                             "?v="
                             (number->string (alist-ref meta 'updated 0))))))

        (define (image-kind bytes)
          (cond ((and (>= (bytevector-length bytes) 8)
                      (eq? (sniff-image bytes) 'png))
                 "png")
                ((eq? (sniff-image bytes) 'webp) "webp")
                ((svg-bytes? bytes) "svg")
                (else #f)))

        ;; --- static, theme, branding ---

        (route! "GET" "/static/*"
          (make-static-handler (setting settings 'static-dir)))

        (route! "GET" "/theme.css"
          (lambda (req params)
            (make-http-response 200
                                `(("Content-Type" . "text/css; charset=utf-8")
                                  ("Cache-Control"
                                   .
                                   ,(if (qref req "v")
                                        "public, max-age=31536000, immutable"
                                        "no-cache")))
                                (theme-css
                                 (load-settings data-dir initial-name)))))

        (route! "GET" "/site/:kind"
          (lambda (req params)
            (let* ((kind (params-ref params "kind"))
                   (s (load-settings data-dir initial-name))
                   (file (and (member kind '("logo" "favicon"))
                              (alist-ref s (string->symbol kind)))))
              (if (and (string? file)
                       (file-exists? (string-append (settings-dir data-dir)
                                                    "/"
                                                    file)))
                  ;; An SVG opened directly runs no script (sandbox).
                  (make-http-file-response
                   200
                   `(("Content-Type"
                      .
                      ,(let ((e (assoc (file-extension file) logo-kinds)))
                         (if e (cdr e) "application/octet-stream")))
                     ("Cache-Control" . "public, max-age=86400")
                     ("Content-Security-Policy" .
                      "default-src 'none'; style-src 'unsafe-inline'; sandbox"))
                   (string-append (settings-dir data-dir) "/" file))
                  (make-http-response 404 '() "")))))

        ;; --- browsing ---

        (route! "GET" "/"
          (lambda (req params)
            (let* ((user (current-user req))
                   (q (qref req "q"))
                   (tag (let ((t (qref req "tag")))
                          (and t (not (string=? t "")) t)))
                   (sort (let ((s (qref req "sort")))
                           (if (member s '("oldest" "title")) s "newest")))
                   (visible (let loop ((ms (video-list lib)))
                              (cond ((null? ms) '())
                                    ((or
                                      (can-edit? user)
                                      (and
                                       (eq? (alist-ref (car ms) 'status)
                                            'ready)
                                       (case (alist-ref (car ms) 'visibility)
                                         ((public) #t)
                                         ((restricted)
                                          (has-role? user 'viewer))
                                         (else #f))))
                                     (cons (car ms) (loop (cdr ms))))
                                    (else (loop (cdr ms))))))
                   (hits (search-videos visible q tag (string->symbol sort)))
                   (total (length hits))
                   (pages (max 1 (quotient (+ total page-size -1) page-size)))
                   (pg (let ((n (string->number (or (qref req "page") "1"))))
                         (if (and n (exact-integer? n) (>= n 1) (<= n pages))
                             n
                             1)))
                   (slice (let loop ((xs hits) (i 0) (acc '()))
                            (cond ((or (null? xs) (>= i (* pg page-size)))
                                   (reverse acc))
                                  ((>= i (* (- pg 1) page-size))
                                   (loop (cdr xs)
                                         (+ i 1)
                                         (cons (car xs) acc)))
                                  (else (loop (cdr xs) (+ i 1) acc))))))
              (html-response (home-page
                              (ctx user)
                              slice
                              total
                              pg
                              pages
                              q
                              tag
                              sort
                              (all-tags visible)
                              (lambda (m) (viewable? user req m)))))))

        (route! "GET" "/v/:id"
          (lambda (req params)
            (let* ((user (current-user req))
                   (meta (video-ref lib (params-ref params "id"))))
              (cond ((not meta) (not-found user))
                    ((viewable? user req meta)
                     (make-http-response
                      200
                      '(("Content-Type" . "text/html; charset=utf-8")
                        ("Cache-Control" . "private, no-cache"))
                      (watch-page (ctx user) meta)))
                    ((needs-unlock? user meta)
                     (html-response (unlock-page (ctx user) meta #f)))
                    ((and (not user)
                          (eq? (alist-ref meta 'visibility) 'restricted)
                          (eq? (alist-ref meta 'status) 'ready))
                     (redirect (string-append "/login?next=/v/"
                                              (alist-ref meta 'id))))
                    (else (not-found user))))))

        (route! "POST" "/v/:id/unlock"
          (lambda (req params)
            (let* ((user (current-user req))
                   (meta (video-ref lib (params-ref params "id")))
                   (key (and meta
                             (cons (alist-ref meta 'id) (client-ip req)))))
              (cond ((or (not meta) (not (needs-unlock? user meta)))
                     (not-found user))
                    ((unlock-limit 'blocked? key)
                     (html-response (unlock-page
                                     (ctx user)
                                     meta
                                     "Zu viele Fehlversuche. Bitte später erneut versuchen.")))
                    ((password-verify (form-ref (http-request-form req)
                                                "passphrase"
                                                "")
                                      (alist-ref meta 'passphrase-hash))
                     (unlock-limit 'reset! key)
                     (redirect (string-append "/v/" (alist-ref meta 'id))
                               (unlock-cookie secret meta
                                              (* 30 24 3600)
                                              (cookie-path site)
                                              secure?)))
                    (else (unlock-limit 'fail! key)
                          (penalty)
                          (html-response
                           (unlock-page (ctx user)
                                        meta
                                        "Falsche Passphrase.")))))))

        (route! "GET" "/media/:id/:file"
          (lambda (req params)
            (let* ((user (current-user req))
                   (file (params-ref params "file"))
                   (meta (video-ref lib (params-ref params "id"))))
              (cond ((or (not meta)
                         (not (member file
                                      '("video.mp4" "thumb.jpg"
                                        "thumb-sm.jpg"))))
                     (make-http-response 404 '() ""))
                    ((not (viewable? user req meta))
                     (make-http-response 403 '() ""))
                    (else (let
                              ((download?
                                (and (equal? file "video.mp4")
                                     (qref req "download")
                                     (alist-ref meta 'allow-download #f))))
                            (file-response
                             (video-file lib (alist-ref meta 'id) file)
                             `((request . ,req)
                               (cache-control . "private, max-age=3600")
                               ,@(if download?
                                     `((filename
                                        .
                                        ,(let
                                             ((t
                                               (clean-filename
                                                (alist-ref meta 'title ""))))
                                           (string-append
                                            (if (string=? t "") "video" t)
                                            ".mp4"))))
                                     '())))))))))

        ;; --- sign in ---

        (route! "GET" "/login"
          (lambda (req params)
            (if (current-user req)
                (redirect (or (safe-next (qref req "next")) "/"))
                (html-response (login-page (ctx #f)
                                           #f
                                           (safe-next (qref req "next")))))))

        (route! "POST" "/login"
          (lambda (req params)
            (let* ((form (http-request-form req))
                   (next (safe-next (form-ref form "next" "")))
                   (ip (client-ip req))
                   (user (and (not (login-limit 'blocked? ip))
                              (sign-in auth
                                       (form-ref form "username" "")
                                       (form-ref form "password" "")))))
              (if user
                  (begin
                    (login-limit 'reset! ip)
                    (redirect (or next "/") (start-session auth user)))
                  (begin
                    (login-limit 'fail! ip)
                    (penalty)
                    (html-response (login-page (ctx #f) #t next)))))))

        (route! "POST" "/logout"
          (lambda (req params) (redirect "/" (end-session auth))))

        (route! "GET" "/profile"
          (signed-in (lambda (req params user)
                       (html-response (profile-page (ctx user)
                                                    (msg-of req))))))

        (route! "POST" "/profile/password"
          (signed-in (lambda (req params user)
                       (let* ((form (http-request-form req))
                              (new (form-ref form "new" ""))
                              (fail (lambda (text)
                                      (html-response
                                       (profile-page (ctx user)
                                                     (cons 'error text))))))
                         (cond ((not (sign-in auth
                                              (user-name user)
                                              (form-ref form "current" "")))
                                (penalty)
                                (fail "Das aktuelle Passwort ist falsch."))
                               ((< (string-length new) min-password-length)
                                (fail
                                 "Das neue Passwort muss mindestens 8 Zeichen lang sein."))
                               ((not (string=? new
                                               (form-ref form "confirm" "")))
                                (fail
                                 "Die neuen Passwörter stimmen nicht überein."))
                               (else (let
                                         ((user
                                           (change-password! auth user new)))
                                       (redirect
                                        "/profile?m=pw"
                                        (start-session auth user)))))))))

        ;; --- editor pages ---

        (route! "GET" "/upload"
          (with-role 'editor
            (lambda (req params user)
              (html-response (upload-page
                              (ctx user)
                              (let ((free (free-bytes data-dir)))
                                (if free
                                    (string-append
                                     "Freier Speicher: "
                                     (number->string
                                      (exact (floor (/ free 1073741824))))
                                     " GB.")
                                    "")))))))

        (route! "GET" "/manage"
          (with-role 'editor
            (lambda (req params user)
              (html-response (manage-page (ctx user)
                                          (video-list lib)
                                          (msg-of req))))))

        (route! "GET" "/v/:id/edit"
          (with-role 'editor
            (lambda (req params user)
              (let ((meta (video-ref lib (params-ref params "id"))))
                (if meta
                    (html-response (edit-page (ctx user) meta (msg-of req)))
                    (not-found user))))))

        (route! "POST" "/v/:id/edit"
          (with-role 'editor
            (lambda (req params user)
              (let* ((id (params-ref params "id"))
                     (meta (video-ref lib id))
                     (form (http-request-form req))
                     (title (string-trim-both (form-ref form "title" "")))
                     (pass (form-ref form "passphrase" ""))
                     (vis (string->symbol
                           (form-ref form "visibility" "draft"))))
                (cond ((not meta) (not-found user))
                      ((string=? title "")
                       (redirect (string-append "/v/" id "/edit?m=no-title")))
                      ((and (> (string-length pass) 0)
                            (< (string-length pass) 4))
                       (redirect (string-append "/v/" id
                                                "/edit?m=short-pass")))
                      (else (video-update!
                             lib
                             id
                             (lambda (m)
                               (let* ((m
                                       (alist-set
                                        m
                                        'title
                                        (if (> (string-length title) 200)
                                            (substring title 0 200)
                                            title)))
                                      (m
                                       (alist-set
                                        m
                                        'description
                                        (form-ref form "description" "")))
                                      (m
                                       (alist-set
                                        m
                                        'tags
                                        (normalize-tags
                                         (form-ref form "tags" ""))))
                                      (m
                                       (alist-set
                                        m
                                        'visibility
                                        (if (memq vis visibility-names)
                                            vis
                                            'draft)))
                                      (m
                                       (alist-set
                                        m
                                        'allow-download
                                        (equal?
                                         (form-ref form "allow-download" "")
                                         "1")))
                                      (m
                                       (cond
                                        ((> (string-length pass) 0)
                                         (alist-set m
                                                    'passphrase-hash
                                                    (password-hash pass)))
                                        ((equal?
                                          (form-ref form "remove-passphrase"
                                                    "")
                                          "1")
                                         (alist-set m 'passphrase-hash #f))
                                        (else m))))
                                 (alist-set m 'updated-by (user-name user)))))
                            (redirect (string-append "/v/" id
                                                     "/edit?m=saved"))))))))

        (route! "POST" "/v/:id/delete"
          (with-role 'editor
            (lambda (req params user)
              (let ((id (params-ref params "id")))
                (if (video-trash! lib id)
                    (begin
                      (log-info "videos"
                                (string-append id " trashed by "
                                               (user-name user)))
                      (redirect "/manage?m=deleted"))
                    (not-found user))))))

        ;; --- upload API ---

        (route! "POST" "/api/uploads"
          (api-role 'editor
                    (lambda (req params user)
                      (let* ((j (parse-json-body req))
                             (size (and j (json-ref j "size" #f)))
                             (name (clean-filename
                                    (and j (json-ref j "filename" #f))))
                             (target (and
                                      j
                                      (let ((t (json-ref j "target" 'null)))
                                        (and (string? t) t)))))
                        (cond ((not (and (exact-integer? size) (> size 0)))
                               (json-error 400 "size required"))
                              ((and target (not (video-ref lib target)))
                               (json-error 404 "no such video"))
                              ((let ((free (free-bytes data-dir)))
                                 (and free (< free (+ size min-free))))
                               (json-error 507
                                           "Nicht genügend Speicherplatz auf dem Server."))
                              (else (let
                                        ((u
                                          (upload-start! data-dir
                                                         (user-name user)
                                                         name
                                                         size
                                                         target)))
                                      (json-status
                                       201
                                       `(("id" . ,(alist-ref u 'id))
                                         ("offset" . 0)
                                         ("chunkSize"
                                          .
                                          ,upload-chunk-size))))))))))

        (route! "GET" "/api/uploads/:id"
          (api-role 'editor
                    (lambda (req params user)
                      (let ((u (own-upload user params)))
                        (if u
                            (json-status
                             200
                             `(("id" . ,(alist-ref u 'id))
                               ("offset"
                                .
                                ,(upload-offset data-dir (alist-ref u 'id)))
                               ("size" . ,(alist-ref u 'size))
                               ("chunkSize" . ,upload-chunk-size)))
                            (json-error 404 "no such upload"))))))

        (route! "PUT" "/api/uploads/:id"
          (api-role 'editor
                    (lambda (req params user)
                      (let* ((u (own-upload user params))
                             (offset (string->number
                                      (or (qref req "offset") "")))
                             (bytes (and u (http-request-body-bytes req))))
                        (cond ((not u) (json-error 404 "no such upload"))
                              ((not (and (exact-integer? offset)
                                         (>= offset 0)))
                               (json-error 400 "offset required"))
                              ((or (not bytes)
                                   (= (bytevector-length bytes) 0))
                               (json-error 400 "empty chunk"))
                              ((> (bytevector-length bytes) upload-chunk-size)
                               (json-error 413 "chunk too large"))
                              (else (let
                                        ((r
                                          (upload-append! data-dir
                                                          (alist-ref u 'id)
                                                          offset
                                                          bytes)))
                                      (cond
                                       ((number? r)
                                        (json-status 200 `(("offset" . ,r))))
                                       ((eq? (car r) 'conflict)
                                        (json-status
                                         409
                                         `(("error" . "offset mismatch")
                                           ("offset" . ,(cdr r)))))
                                       (else
                                        (json-error 413
                                                    "more data than announced"))))))))))

        (route! "DELETE" "/api/uploads/:id"
          (api-role 'editor
                    (lambda (req params user)
                      (let ((u (own-upload user params)))
                        (when u (upload-cancel! data-dir (alist-ref u 'id)))
                        (json-status 200 '(("ok" . #t)))))))

        (route! "POST" "/api/uploads/:id/complete"
          (api-role 'editor
                    (lambda (req params user)
                      (let* ((u (own-upload user params))
                             (id (and u (alist-ref u 'id)))
                             (part (and u (upload-part-path data-dir id))))
                        (cond ((not u) (json-error 404 "no such upload"))
                              ((not (= (upload-offset data-dir id)
                                       (alist-ref u 'size)))
                               (json-status
                                409
                                `(("error" . "upload incomplete")
                                  ("offset" . ,(upload-offset data-dir id)))))
                              ((not (sniff-mp4? part))
                               (upload-cancel! data-dir id)
                               (json-error 415
                                           "Die Datei ist kein MP4-Video."))
                              (else (let*
                                        ((target (alist-ref u 'target))
                                         (meta
                                          (if
                                           target
                                           (video-update!
                                            lib
                                            target
                                            (lambda (m)
                                              (let*
                                                  ((m
                                                    (alist-set m
                                                               'status
                                                               'processing))
                                                   (m (alist-set m 'error #f))
                                                   (m
                                                    (alist-set
                                                     m
                                                     'original-filename
                                                     (alist-ref u 'filename)))
                                                   (m
                                                    (if
                                                     (eq?
                                                      (alist-ref
                                                       m
                                                       'thumbnail-at)
                                                      'custom)
                                                     m
                                                     (alist-set m
                                                                'thumbnail-at
                                                                #f))))
                                                (alist-set
                                                 m
                                                 'updated-by
                                                 (user-name user)))))
                                           (video-create!
                                            lib
                                            `((title
                                               .
                                               ,(base-name-of
                                                 (alist-ref u 'filename)))
                                              (original-filename
                                               .
                                               ,(alist-ref u 'filename))
                                              (size . ,(alist-ref u 'size))
                                              (created-by . ,(user-name user))
                                              (updated-by
                                               .
                                               ,(user-name user)))))))
                                      (if
                                       (not meta)
                                       (begin
                                         (upload-cancel! data-dir id)
                                         (json-error 404 "no such video"))
                                       (let ((vid (alist-ref meta 'id)))
                                         (unless
                                             (move-file
                                              part
                                              (video-file lib vid
                                                          "video.mp4"))
                                           (error
                                            "cannot move upload into place"
                                            id))
                                         (upload-cancel! data-dir id)
                                         (log-info
                                          "videos"
                                          (string-append vid " uploaded by "
                                                         (user-name user)))
                                         (enqueue! vid)
                                         (json-status
                                          200
                                          `(("video" . ,vid)
                                            ("editUrl"
                                             .
                                             ,(url
                                               (string-append "/v/" vid
                                                              "/edit"))))))))))))))

        ;; --- video API (status, thumbnails) ---

        (route! "GET" "/api/videos/:id"
          (api-role 'editor
                    (lambda (req params user)
                      (let ((m (video-ref lib (params-ref params "id"))))
                        (if m
                            (json-status
                             200
                             `(("id" . ,(alist-ref m 'id))
                               ("status"
                                .
                                ,(symbol->string (alist-ref m 'status)))
                               ("error" . ,(or (alist-ref m 'error) 'null))
                               ("warning"
                                .
                                ,(or (alist-ref m 'warning) 'null))
                               ("duration" . ,(alist-ref m 'duration 0))))
                            (json-error 404 "no such video"))))))

        (route! "POST" "/api/videos/:id/thumbnail"
          (api-role 'editor
                    (lambda (req params user)
                      (let* ((m (video-ref lib (params-ref params "id")))
                             (j (parse-json-body req))
                             (at (and j (json-ref j "at" #f))))
                        (cond ((not m) (json-error 404 "no such video"))
                              ((not (eq? (alist-ref m 'status) 'ready))
                               (json-error 409 "not ready"))
                              ((not (and (real? at) (>= at 0)))
                               (json-error 400 "at required"))
                              (else (let
                                        ((at
                                          (min
                                           at
                                           (max
                                            0
                                            (- (alist-ref m 'duration 0)
                                               0.1)))))
                                      (extract-thumbnail! lib
                                                          (alist-ref m 'id)
                                                          at)
                                      (json-status
                                       200
                                       (thumb-json
                                        (video-update!
                                         lib
                                         (alist-ref m 'id)
                                         (lambda (m)
                                           (alist-set m
                                                      'thumbnail-at
                                                      at))))))))))))

        (route! "POST" "/api/videos/:id/thumbnail-image"
          (api-role 'editor
                    (lambda (req params user)
                      (let* ((m (video-ref lib (params-ref params "id")))
                             (bytes (and m (http-request-body-bytes req))))
                        (cond ((not m) (json-error 404 "no such video"))
                              ((or (not bytes)
                                   (> (bytevector-length bytes)
                                      (* 10 1024 1024))
                                   (not (sniff-image bytes)))
                               (json-error 415
                                           "Nur JPEG, PNG oder WebP bis 10 MB."))
                              (else (let
                                        ((tmp
                                          (video-file
                                           lib
                                           (alist-ref m 'id)
                                           (string-append
                                            "upload."
                                            (symbol->string
                                             (sniff-image bytes))))))
                                      (write-bytevector-file tmp bytes)
                                      (guard
                                          (e (#t (delete-file tmp) (raise e)))
                                        (image->thumbnail! lib
                                                           (alist-ref m 'id)
                                                           tmp))
                                      (delete-file tmp)
                                      (json-status
                                       200
                                       (thumb-json
                                        (video-update!
                                         lib
                                         (alist-ref m 'id)
                                         (lambda (m)
                                           (alist-set m
                                                      'thumbnail-at
                                                      'custom))))))))))))

        ;; --- admin: users ---

        (route! "GET" "/admin/users"
          (with-role 'admin
            (lambda (req params user)
              (html-response (admin-users-page (ctx user)
                                               (load-users users-file)
                                               (msg-of req))))))

        (route! "POST" "/admin/users"
          (with-role 'admin
            (lambda (req params user)
              (let* ((form (http-request-form req))
                     (name (string-trim-both (form-ref form "username" "")))
                     (pw (form-ref form "password" ""))
                     (role (string->symbol (form-ref form "role" "viewer"))))
                (cond ((not (valid-username? name))
                       (redirect "/admin/users?m=bad-user"))
                      ((find-user users-file name)
                       (redirect "/admin/users?m=user-exists"))
                      ((< (string-length pw) min-password-length)
                       (redirect "/admin/users?m=short-pw"))
                      (else (save-user! users-file name
                                        (password-hash pw)
                                        (if (valid-role? role) role 'viewer))
                            (redirect "/admin/users?m=user-added")))))))

        (route! "POST" "/admin/users/:name"
          (with-role 'admin
            (lambda (req params user)
              (let* ((name (params-ref params "name"))
                     (form (http-request-form req))
                     (op (form-ref form "op" "")))
                (cond ((equal? name (user-name user))
                       (redirect "/admin/users?m=self"))
                      ((not (find-user users-file name)) (not-found user))
                      ((equal? op "role")
                       (let ((r (string->symbol (form-ref form "role" ""))))
                         (when (valid-role? r)
                           (update-user! users-file name 'role r))
                         (redirect "/admin/users?m=user-saved")))
                      ((member op '("disable" "enable"))
                       (update-user! users-file name
                                     'disabled
                                     (equal? op "disable"))
                       (redirect "/admin/users?m=user-saved"))
                      ((equal? op "password")
                       (let ((pw (form-ref form "password" "")))
                         (if (< (string-length pw) min-password-length)
                             (redirect "/admin/users?m=short-pw")
                             (begin
                               (update-user! users-file name
                                             'hash
                                             (password-hash pw))
                               (redirect "/admin/users?m=user-saved")))))
                      ((equal? op "delete")
                       (delete-user! users-file name)
                       (redirect "/admin/users?m=user-deleted"))
                      (else (redirect "/admin/users")))))))

        ;; --- admin: site settings ---

        (route! "GET" "/admin/site"
          (with-role 'admin
            (lambda (req params user)
              (let ((c (ctx user)))
                (html-response (admin-site-page
                                c
                                (msg-of req)
                                (contrast-warnings
                                 (alist-ref c 'settings))))))))

        (route! "POST" "/admin/site"
          (with-role 'admin
            (lambda (req params user)
              (let* ((form (http-request-form req))
                     (s (load-settings data-dir initial-name))
                     (colors (lambda (scheme)
                               (map (lambda (k)
                                      (let
                                          ((v
                                            (string-downcase
                                             (form-ref
                                              form
                                              (string-append
                                               (symbol->string scheme)
                                               "-"
                                               (symbol->string (car k)))
                                              ""))))
                                        (cons (car k) v)))
                                    color-keys)))
                     (light (colors 'light))
                     (dark (colors 'dark))
                     (mode (string->symbol
                            (form-ref form "default-mode" "auto")))
                     (name (string-trim-both (form-ref form "site-name" ""))))
                (if (not (let loop ((cs (append light dark)))
                           (or (null? cs)
                               (and (valid-color? (cdar cs))
                                    (loop (cdr cs))))))
                    (redirect "/admin/site?m=bad-color")
                    (begin
                      (save-settings! data-dir
                                      (let*
                                          ((s
                                            (alist-set
                                             s
                                             'site-name
                                             (if (string=? name "")
                                                 "Filme"
                                                 name)))
                                           (s
                                            (alist-set
                                             s
                                             'tagline
                                             (string-trim-both
                                              (form-ref form "tagline" ""))))
                                           (s
                                            (alist-set
                                             s
                                             'default-mode
                                             (if
                                              (memq mode '(auto light dark))
                                              mode
                                              'auto)))
                                           (s (alist-set s 'light light)))
                                        (alist-set s 'dark dark)))
                      (redirect "/admin/site?m=site-saved")))))))

        (route! "POST" "/admin/site/reset"
          (with-role 'admin
            (lambda (req params user)
              (let ((s (load-settings data-dir initial-name)))
                ;; Keep the uploaded images; reset names and colours.
                (save-settings! data-dir
                                (alist-set
                                 (alist-set default-settings
                                            'logo
                                            (alist-ref s 'logo))
                                 'favicon
                                 (alist-ref s 'favicon)))
                (redirect "/admin/site?m=site-reset")))))

        (route! "POST" "/admin/site/:kind"
          (with-role 'admin
            (lambda (req params user)
              (let* ((kind (params-ref params "kind"))
                     (boundary (multipart-boundary
                                (or (http-request-header req "Content-Type")
                                    "")))
                     (parts (and boundary
                                 (guard (e (#t #f))
                                   (parse-multipart-bytes
                                    (http-request-body-bytes req)
                                    boundary))))
                     (file (and parts
                                (let loop ((ps parts))
                                  (cond
                                   ((null? ps) #f)
                                   ((equal? (part-ref (car ps) 'name) "file")
                                    (part-ref (car ps) 'body))
                                   (else (loop (cdr ps)))))))
                     (ext (and (bytevector? file)
                               (<= 1 (bytevector-length file) (* 1024 1024))
                               (image-kind file))))
                (cond ((not (member kind '("logo" "favicon")))
                       (not-found user))
                      ((not ext) (redirect "/admin/site?m=logo-bad"))
                      (else (let* ((s (load-settings data-dir initial-name))
                                   (old (alist-ref s (string->symbol kind)))
                                   (name (string-append kind "." ext)))
                              (write-bytevector-file
                               (string-append (settings-dir data-dir)
                                              "/"
                                              name)
                               file)
                              (when
                                  (and (string? old)
                                       (not (string=? old name)))
                                (rm (string-append (settings-dir data-dir)
                                                   "/"
                                                   old)
                                    'force))
                              (save-settings!
                               data-dir
                               (alist-set s (string->symbol kind) name))
                              (redirect "/admin/site?m=logo-saved"))))))))

        (route! "POST" "/admin/site/:kind/delete"
          (with-role 'admin
            (lambda (req params user)
              (let* ((kind (params-ref params "kind"))
                     (s (load-settings data-dir initial-name)))
                (if (not (member kind '("logo" "favicon")))
                    (not-found user)
                    (let ((old (alist-ref s (string->symbol kind))))
                      (when (string? old)
                        (rm (string-append (settings-dir data-dir) "/" old)
                            'force))
                      (save-settings! data-dir
                                      (alist-set s (string->symbol kind) #f))
                      (redirect "/admin/site?m=site-saved")))))))

        ;; Outside the prefix: the health probe talks to the app directly.
        (router-add! router "GET" "/healthz"
          (lambda (req params) (text-response "ok\n")))

        (with-access-log (with-security-headers
                             (with-site site
                               (with-origin-check
                                   (with-error-handler
                                       (let ((h (router->handler router)))
                                         (lambda (req)
                                           (library-refresh! lib)
                                           (h req)))
                                     (lambda (req)
                                       (html-response
                                        (error-page (ctx #f)))))))
                           csp))))

    ;; --- misc ------------------------------------------------------------

    (define (file-extension name)
      (let loop ((i (- (string-length name) 1)))
        (cond ((< i 0) "")
              ((char=? (string-ref name i) #\.)
               (substring name (+ i 1) (string-length name)))
              (else (loop (- i 1))))))

    (define (free-bytes dir)
      ;; df reports 1K blocks.
      (let ((d (guard (e (#t '())) (df dir))))
        (and (pair? d)
             (let ((a (alist-ref (car d) 'available #f)))
               (and (number? a) (* a 1024))))))

    (define (svg-bytes? bv)
      (let* ((n (min 512 (bytevector-length bv)))
             (s (guard (e (#t "")) (utf8->string (bytevector-copy bv 0 n)))))
        (and (string-contains-from s "<svg" 0) #t)))

    (define (write-bytevector-file path bv)
      (let ((tmp (string-append path ".tmp")))
        (let ((port (open-binary-output-file tmp)))
          (write-bytevector bv port)
          (close-port port))
        (move-file tmp path)))))
