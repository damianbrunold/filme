;; Tests for the web app: sign-in, roles, access to videos and media,
;; passphrase unlock, the upload API, admin pages, CSRF origin check and
;; the URL prefix. Requests go straight to the handler from make-app (no
;; server, no socket, no ffmpeg).
;; Run: scm tests/test_pure_web.scm

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
        (scm string)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app store)
        (app users)
        (app videos)
        (app web))

(test-runner-factory scm-test-runner)
(test-begin "filme web")

(define tmp (mktempdir '(prefix . "filme-web")))
(define users-file (string-append tmp "/users.scm"))
(define fast-iterations 1000)

(define (app-with prefix)
  (make-app `((url-prefix . ,prefix)
              (static-dir . ,(string-append root "/static"))
              (data-dir . ,tmp)
              (session-secret . ,(base64-encode (random-bytes 32)))
              (session-max-age . 3600)
              (secure-cookies? . #f)
              (process? . #f)
              (min-free-bytes . 0))))

;; headers: extra (name . value) pairs; body: string, bytevector or #f.
(define (send app method url cookie . more)
  (let ((body (if (pair? more) (car more) #f))
        (headers (if (and (pair? more) (pair? (cdr more))) (cadr more) '())))
    (parameterize ((log-port (open-output-string))
                   (password-pbkdf2-iterations fast-iterations))
      (app (make-http-request method url
                              (append (if cookie
                                          (list (cons "Cookie" cookie))
                                          '())
                                      (if
                                       (string? body)
                                       '(("Content-Type" .
                                          "application/x-www-form-urlencoded"))
                                       '())
                                      headers)
                              body)))))

(define (status r) (http-response-status r))
(define (location r) (http-response-header r "Location"))
(define (body r)
  (let ((b (http-response-body r)))
    (if (bytevector? b) (utf8->string b) (or b ""))))
(define (has? s part) (and (string? s) (string-contains-from s part 0) #t))
(define (cookie-of r)
  (let ((h (http-response-header r "Set-Cookie")))
    (and h (car (string-split-char h #\;)))))

(parameterize ((password-pbkdf2-iterations fast-iterations))
  (save-user! users-file "vera" (password-hash "vera password") 'viewer)
  (save-user! users-file "ed" (password-hash "ed password1") 'editor)
  (save-user! users-file "ada" (password-hash "ada password") 'admin))

(define app (app-with ""))
(define lib (app-library))

(define (login name pw)
  (cookie-of (send app "POST" "/login" #f
                   (string-append "username=" name "&password=" pw))))

(define vera (login "vera" "vera+password"))
(define ed (login "ed" "ed+password1"))
(define ada (login "ada" "ada+password"))

;; A "ready" video without ffmpeg: fake file contents are fine here.
(define (make-video! fields)
  (let* ((m (video-create! lib
                           (append '((status . ready) (duration . 60))
                                   fields)))
         (id (alist-ref m 'id)))
    (call-with-output-file (video-file lib id "video.mp4")
      (lambda (p) (write-string (make-string 5000 #\x) p)))
    (call-with-output-file (video-file lib id "thumb-sm.jpg")
      (lambda (p) (write-string "jpg" p)))
    id))

(define pub
  (make-video! '((title . "Public tutorial")
                 (visibility . public)
                 (tags "git"))))
(define res
  (make-video! `((title . "Restricted one")
                 (visibility . restricted)
                 (passphrase-hash .
                                  ,(parameterize
                                       ((password-pbkdf2-iterations
                                         fast-iterations))
                                     (password-hash "open sesame"))))))
(define res-nopass
  (make-video! '((title . "Members only") (visibility . restricted))))
(define draft (make-video! '((title . "Draft video"))))

(test-group "sign in"
  (test-assert vera)
  (let ((r (send app "POST" "/login" #f "username=vera&password=wrong")))
    (test-equal 200 (status r))
    (test-assert (has? (body r) "falsch")))
  (let
      ((r (send app "POST" "/login" #f
                "username=ed&password=ed+password1&next=/manage")))
    (test-equal "/manage" (location r))
    (test-assert "SameSite=Lax"
                 (has? (http-response-header r "Set-Cookie") "SameSite=Lax")))
  (test-equal "foreign next ignored" "/"
              (location (send app "POST" "/login" #f
                              "username=ed&password=ed+password1&next=//evil.example")))
  (test-equal 303 (status (send app "GET" "/manage" "filme_session=forged"))))

(test-group "home listing"
  (let ((anon (body (send app "GET" "/" #f)))
        (viewer (body (send app "GET" "/" vera)))
        (editor (body (send app "GET" "/" ed))))
    (test-assert (has? anon "Public tutorial"))
    (test-assert "restricted hidden from anonymous"
                 (not (has? anon "Restricted one")))
    (test-assert (not (has? anon "Draft video")))
    (test-assert (has? viewer "Restricted one"))
    (test-assert (not (has? viewer "Draft video")))
    (test-assert (has? editor "Draft video"))
    (test-assert "no inline script" (not (has? editor "<script>")))
    (test-assert "no style attribute" (not (has? editor " style="))))
  (test-assert (has? (body (send app "GET" "/?q=TUTORIAL" #f))
                     "Public tutorial"))
  (test-assert (not (has? (body (send app "GET" "/?q=nothing" #f))
                          "Public tutorial")))
  (test-assert (has? (body (send app "GET" "/?tag=git" #f))
                     "Public tutorial")))

(test-group "watch pages"
  (test-equal 200 (status (send app "GET" (string-append "/v/" pub) #f)))
  (test-equal 404 (status (send app "GET" (string-append "/v/" draft) #f)))
  (test-equal 404 (status (send app "GET" (string-append "/v/" draft) vera)))
  (test-equal 200 (status (send app "GET" (string-append "/v/" draft) ed)))
  (test-equal 404 (status (send app "GET" "/v/doesnotexist" #f)))
  (let ((r (send app "GET" (string-append "/v/" res) #f)))
    (test-equal 200 (status r))
    (test-assert "passphrase form" (has? (body r) "name=\"passphrase\"")))
  (test-equal "restricted without passphrase -> sign in"
              (string-append "/login?next=/v/" res-nopass)
              (location (send app "GET" (string-append "/v/" res-nopass) #f)))
  (test-assert (has? (body (send app "GET" (string-append "/v/" res) vera))
                     "<video")))

(test-group "media"
  (define (media id cookie . headers)
    (send app "GET"
          (string-append "/media/" id "/video.mp4")
          cookie
          #f
          headers))
  (test-equal 200 (status (media pub #f)))
  (let ((r (media pub #f '("Range" . "bytes=100-199"))))
    (test-equal 206 (status r))
    (test-equal "bytes 100-199/5000"
                (http-response-header r "Content-Range")))
  (test-equal 403 (status (media res #f)))
  (test-equal 403 (status (media draft #f)))
  (test-equal 403 (status (media draft vera)))
  (test-equal 200 (status (media res vera)))
  (test-equal 200 (status (media draft ed)))
  (test-equal 404
              (status (send app "GET"
                            (string-append "/media/" pub "/meta.scm")
                            ed)))
  (test-equal 404
              (status (send app "GET"
                            (string-append "/media/" pub "/..%2Fusers.scm")
                            ada))))

(test-group "unlock"
  (define path (string-append "/v/" res "/unlock"))
  (let ((r (send app "POST" path #f "passphrase=wrong")))
    (test-equal 200 (status r))
    (test-equal #f (http-response-header r "Set-Cookie")))
  (let* ((r (send app "POST" path #f "passphrase=open+sesame"))
         (c (cookie-of r)))
    (test-equal (string-append "/v/" res) (location r))
    (test-assert (has? c (string-append "filme_v_" res)))
    (test-equal 200
                (status (send app "GET"
                              (string-append "/media/" res "/video.mp4")
                              c)))
    (test-assert (has? (body (send app "GET" (string-append "/v/" res) c))
                       "<video"))
    (test-equal "cookie only opens its own video" 403
                (status (send app "GET"
                              (string-append "/media/" draft "/video.mp4")
                              c)))
    ;; A new passphrase revokes earlier unlocks.
    (send app "POST"
          (string-append "/v/" res "/edit")
          ed
          "title=Restricted+one&visibility=restricted&passphrase=new+phrase")
    (test-equal 403
                (status (send app "GET"
                              (string-append "/media/" res "/video.mp4")
                              c))))
  (test-group "too many failures"
    (for-each (lambda (i) (send app "POST" path #f "passphrase=bad"))
              '(1 2 3 4 5))
    (test-assert (has? (body (send app "POST" path #f
                                   "passphrase=new+phrase"))
                       "Zu viele Fehlversuche"))))

(test-group "roles"
  (test-equal 303 (status (send app "GET" "/upload" #f)))
  (test-equal 403 (status (send app "GET" "/upload" vera)))
  (test-equal 200 (status (send app "GET" "/upload" ed)))
  (test-equal 403 (status (send app "GET" "/admin/users" ed)))
  (test-equal 200 (status (send app "GET" "/admin/users" ada)))
  (test-equal 200 (status (send app "GET" "/admin/site" ada)))
  (test-equal 401 (status (send app "POST" "/api/uploads" #f "{}")))
  (test-equal 403 (status (send app "POST" "/api/uploads" vera "{}"))))

(test-group "edit"
  (let
      ((r (send app "POST"
                (string-append "/v/" draft "/edit")
                ed
                "title=Neu&description=*hi*&tags=A,+b,+a&visibility=public&allow-download=1")))
    (test-equal (string-append "/v/" draft "/edit?m=saved") (location r))
    (let ((m (video-ref lib draft)))
      (test-equal "Neu" (alist-ref m 'title))
      (test-equal '("a" "b") (alist-ref m 'tags))
      (test-equal 'public (alist-ref m 'visibility))
      (test-equal #t (alist-ref m 'allow-download))
      (test-equal "ed" (alist-ref m 'updated-by))))
  (test-equal (string-append "/v/" draft "/edit?m=no-title")
              (location (send app "POST"
                              (string-append "/v/" draft "/edit")
                              ed
                              "title=+")))
  (test-assert "download link"
               (has? (http-response-header
                      (send app "GET"
                            (string-append "/media/" draft
                                           "/video.mp4?download=1")
                            #f)
                      "Content-Disposition")
                     "Neu.mp4"))
  (test-equal "/manage?m=deleted"
              (location (send app "POST"
                              (string-append "/v/" draft "/delete")
                              ed
                              "")))
  (test-equal 404 (status (send app "GET" (string-append "/v/" draft) ed))))

(test-group "upload API"
  (define json-headers '(("Content-Type" . "application/json")))
  (define (api method url cookie body)
    (send app method url cookie body json-headers))
  (let* ((r (api "POST" "/api/uploads" ed
                 "{\"filename\":\"a.mp4\",\"size\":10}"))
         (id (let ((b (body r)))
               ;; {"id":"...","offset":0,...}
               (substring b 7 18))))
    (test-equal 201 (status r))
    (test-equal 200
                (status (send app "PUT"
                              (string-append "/api/uploads/" id "?offset=0")
                              ed
                              (bytevector 1 2 3 4))))
    (let
        ((r (send app "PUT"
                  (string-append "/api/uploads/" id "?offset=0")
                  ed
                  (bytevector 1 2 3 4))))
      (test-equal 409 (status r))
      (test-assert (has? (body r) "\"offset\":4")))
    (test-equal "another editor cannot touch it" 404
                (status (send app "GET"
                              (string-append "/api/uploads/" id)
                              ada)))
    (test-equal "incomplete" 409
                (status (send app "POST"
                              (string-append "/api/uploads/" id "/complete")
                              ed
                              #f)))
    (send app "PUT"
          (string-append "/api/uploads/" id "?offset=4")
          ed
          (make-bytevector 6 0))
    (test-equal "not an mp4" 415
                (status (send app "POST"
                              (string-append "/api/uploads/" id "/complete")
                              ed
                              #f))))
  (test-equal 400
              (status (api "POST" "/api/uploads" ed
                           "{\"filename\":\"a.mp4\"}")))
  (test-equal 404
              (status (api "POST" "/api/uploads" ed
                           "{\"filename\":\"a.mp4\",\"size\":5,\"target\":\"nosuchvideo\"}"))))

(test-group "cross-site requests are refused"
  (test-equal 403
              (status (send app "POST" "/logout" ed ""
                            '(("Origin" . "https://evil.example")
                              ("Host" . "filme.local")))))
  (test-equal 303
              (status (send app "POST" "/logout" ed ""
                            '(("Origin" . "http://filme.local")
                              ("Host" . "filme.local"))))))

(test-group "admin"
  (test-equal "/admin/users?m=user-added"
              (location (send app "POST" "/admin/users" ada
                              "username=neu&password=neupasswort&role=editor")))
  (test-equal 'editor (user-role (find-user users-file "neu")))
  (test-equal "/admin/users?m=user-exists"
              (location (send app "POST" "/admin/users" ada
                              "username=neu&password=neupasswort")))
  (test-equal "/admin/users?m=self"
              (location (send app "POST" "/admin/users/ada" ada "op=delete")))
  (send app "POST" "/admin/users/vera" ada "op=disable")
  (test-equal "disabled user is signed out" 303
              (status (send app "GET" "/profile" vera)))
  (test-equal "/admin/site?m=bad-color"
              (location (send app "POST" "/admin/site" ada
                              "site-name=X&light-bg=red")))
  (let
      ((form (string-append
              "site-name=Technik&tagline=Intern&default-mode=dark"
              (apply string-append
                     (map (lambda (k)
                            (string-append "&light-" k "=%23ffffff&dark-" k
                                           "=%23000000"))
                          '("bg" "surface" "fg" "muted" "accent" "on-accent"
                            "border"))))))
    (test-equal "/admin/site?m=site-saved"
                (location (send app "POST" "/admin/site" ada form))))
  (let ((home (body (send app "GET" "/" #f))))
    (test-assert (has? home "Technik"))
    (test-assert (has? home "data-theme-default=\"dark\""))
    (test-assert (has? home "Intern")))
  (test-assert (has? (body (send app "GET" "/theme.css" #f))
                     "color-scheme: dark")))

(test-group "API tokens"
  (define (token-from page)
    (let* ((i (string-contains-from page "value=\"fltk_" 0)))
      (and i (substring page (+ i 7) (+ i 7 5 11 1 43)))))
  (define (bearer tok) (list (cons "Authorization" (string-append "Bearer " tok))))
  (define json-h '(("Content-Type" . "application/json")))
  (parameterize ((password-pbkdf2-iterations fast-iterations))
    (save-user! users-file "viv" (password-hash "viv password") 'viewer))
  (test-equal "viewers cannot create tokens" 403
              (status (send app "POST" "/profile/tokens" (login "viv" "viv+password") "label=x")))
  (let* ((r (send app "POST" "/profile/tokens" ed "label=Aufnahme"))
         (tok (token-from (body r))))
    (test-equal 200 (status r))
    (test-equal "shown once, not cached" "no-store" (http-response-header r "Cache-Control"))
    (test-assert (string? tok))
    (test-equal 60 (string-length tok))
    (test-assert "listed on the profile" (has? (body (send app "GET" "/profile" ed)) "Aufnahme"))
    (test-assert "but not the token itself" (not (has? (body (send app "GET" "/profile" ed)) tok)))
    (let* ((r (send app "POST" "/api/uploads" #f "{\"filename\":\"t.mp4\",\"size\":4}"
                    (append (bearer tok) json-h)))
           (id (substring (body r) 7 18)))
      (test-equal "upload with the token" 201 (status r))
      (test-equal 200 (status (send app "PUT" (string-append "/api/uploads/" id "?offset=0") #f
                                    (bytevector 1 2 3 4) (bearer tok))))
      (test-equal "the upload belongs to the token's user" 200
                  (status (send app "GET" (string-append "/api/uploads/" id) ed)))
      (test-equal 415 (status (send app "POST" (string-append "/api/uploads/" id "/complete") #f
                                    "{\"title\":\"X\"}" (append (bearer tok) json-h)))))
    (test-equal "pages do not accept tokens" 303
                (status (send app "GET" "/manage" #f #f (bearer tok))))
    (test-equal 303 (status (send app "GET" "/admin/users" #f #f (bearer tok))))
    (test-equal "a wrong secret" 401
                (status (send app "POST" "/api/uploads" #f "{}"
                              (append (bearer (string-append (substring tok 0 50) "xxxxxxxxx")) json-h))))
    (test-equal 401 (status (send app "POST" "/api/uploads" #f "{}"
                                  (append (bearer "nonsense") json-h))))
    (let* ((u (find-user users-file "ed"))
           (tid (alist-ref (car (alist-ref u 'tokens)) 'id)))
      (test-equal "/profile?m=token-revoked"
                  (location (send app "POST" (string-append "/profile/tokens/" tid "/delete") ed "")))
      (test-equal "revoked" 401
                  (status (send app "POST" "/api/uploads" #f "{\"filename\":\"t.mp4\",\"size\":4}"
                                (append (bearer tok) json-h))))))
  (let ((tok (token-from (body (send app "POST" "/profile/tokens" ed "label=B")))))
    (send app "POST" "/admin/users/ed" ada "op=disable")
    (test-equal "a disabled user's token stops working" 401
                (status (send app "POST" "/api/uploads" #f "{\"filename\":\"t.mp4\",\"size\":4}"
                              (append (bearer tok) json-h))))
    (send app "POST" "/admin/users/ed" ada "op=enable")))

(test-group "url prefix"
  (let* ((app (app-with "/filme"))
         (r (send app "POST" "/filme/login" #f
                  "username=ed&password=ed+password1")))
    (test-equal "/filme/" (location r))
    (test-assert (has? (http-response-header r "Set-Cookie") "Path=/filme/"))
    (test-assert (has? (body (send app "GET" "/filme/" #f))
                       "href=\"/filme/static/css/app.css?v="))
    (test-equal 404 (status (send app "GET" "/login" #f)))
    (test-equal "ok\n" (body (send app "GET" "/healthz" #f)))))

(rm tmp 'recursive 'force)
(test-end "filme web")
