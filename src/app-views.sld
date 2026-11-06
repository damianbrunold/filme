;; (app views) — HTML rendering. All dynamic values flow through the
;; auto-escaping (scm html builder), so user data can never inject markup.
;; Only fixed strings (the SVG icons) and the output of markdown->html
;; (which escapes text and URLs itself) are emitted raw. No inline scripts
;; or style attributes: the Content-Security-Policy forbids them.
;;
;; Every page gets a ctx alist: (user . entry-or-#f) (settings . alist)
;; (theme-v . string). The UI text is German; messages shown after a
;; redirect are looked up by code in `notices`.

(define-library (app views)
  (import (scheme base)
          (scheme char)
          (scm datetime)
          (scm html builder)
          (scm markdown)
          (app access)
          (app site)
          (app store)
          (app theme)
          (app tokens)
          (app users))
  (export admin-site-page
          admin-users-page
          edit-page
          error-page
          format-duration
          home-page
          login-page
          manage-page
          not-found-page
          notice-text
          profile-page
          unlock-page
          upload-page
          watch-page)
  (begin

    ;; --- messages ------------------------------------------------------

    (define notices
      '(("saved" success . "Gespeichert.")
        ("deleted" success . "Das Video wurde in den Papierkorb verschoben.")
        ("pw" success . "Passwort geändert.")
        ("token-revoked" success . "Token widerrufen.")
        ("user-added" success . "Benutzer angelegt.")
        ("user-saved" success . "Benutzer gespeichert.")
        ("user-deleted" success . "Benutzer gelöscht.")
        ("bad-user" error .
         "Ungültiger Benutzername (Buchstaben, Ziffern, . _ -).")
        ("user-exists" error . "Diesen Benutzer gibt es schon.")
        ("short-pw" error .
         "Das Passwort muss mindestens 8 Zeichen lang sein.")
        ("self" error . "Das eigene Konto kann so nicht geändert werden.")
        ("site-saved" success . "Darstellung gespeichert.")
        ("site-reset" success . "Darstellung zurückgesetzt.")
        ("logo-saved" success . "Bild gespeichert.")
        ("logo-bad" error . "Nur PNG, WebP oder SVG bis 1 MB.")
        ("bad-color" error . "Ungültige Farbe.")
        ("no-title" error . "Bitte einen Titel angeben.")
        ("short-pass" error .
         "Die Passphrase muss mindestens 4 Zeichen lang sein.")))

    (define (notice-text code)
      "Syntax: (notice-text code)
Library: (app views)
Description: (kind . text) for a notice code, or #f."
      (let ((p (and code (assoc code notices)))) (and p (cdr p))))

    (define (notice msg)
      (if msg
          `(p (@ (class ,(string-append "notice " (symbol->string (car msg))))
                 (role "status"))
              ,(cdr msg))
          ""))

    ;; --- formatting ------------------------------------------------------

    (define (two n)
      (if (< n 10) (string-append "0" (number->string n)) (number->string n)))

    (define (format-duration secs)
      "Syntax: (format-duration seconds)
Library: (app views)
Description: \"m:ss\" or \"h:mm:ss\"."
      (let* ((s (exact (round (if (number? secs) secs 0))))
             (h (quotient s 3600))
             (m (quotient (remainder s 3600) 60))
             (r (remainder s 60)))
        (if (> h 0)
            (string-append (number->string h) ":" (two m) ":" (two r))
            (string-append (number->string m) ":" (two r)))))

    (define (format-date secs)
      (let ((iso (format-iso8601 (if (number? secs) secs 0))))
        ;; "YYYY-MM-DDT..." -> "DD.MM.YYYY"
        (string-append (substring iso 8 10)
                       "."
                       (substring iso 5 7)
                       "."
                       (substring iso 0 4))))

    (define (format-size n)
      (cond ((>= n 1073741824)
             (string-append (number->string (/ (round (/ n 107374182.4)) 10))
                            " GB"))
            ((>= n 1048576)
             (string-append (number->string (exact (round (/ n 1048576))))
                            " MB"))
            (else (string-append (number->string (exact (round (/ n 1024))))
                                 " KB"))))

    (define visibility-labels
      '((public . "Öffentlich")
        (restricted . "Eingeschränkt")
        (draft . "Entwurf")))

    (define role-labels
      '((viewer . "Betrachter")
        (editor . "Redaktion")
        (admin . "Administration")))

    (define (label-of table key)
      (alist-ref table key (if (symbol? key) (symbol->string key) "")))

    ;; --- icons -----------------------------------------------------------

    (define (svg path)
      (raw (string-append
            "<svg viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" "
            "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" "
            "stroke-linejoin=\"round\" aria-hidden=\"true\">" path "</svg>")))

    (define theme-icons
      (raw (string-append
            "<svg class=\"i-sun\" viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\"><circle cx=\"12\" cy=\"12\" r=\"4\"/><path d=\"M12 2v2M12 20v2M2 12h2M20 12h2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M19.1 4.9l-1.4 1.4M6.3 17.7l-1.4 1.4\"/></svg>"
            "<svg class=\"i-moon\" viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\"><path d=\"M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z\"/></svg>")))

    (define lock-icon
      (svg
       "<rect x=\"5\" y=\"11\" width=\"14\" height=\"10\" rx=\"2\"/><path d=\"M8 11V7a4 4 0 0 1 8 0v4\"/>"))

    (define draft-icon
      (svg
       "<path d=\"M12 20h9\"/><path d=\"M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4z\"/>"))

    ;; --- layout ----------------------------------------------------------

    (define (ctx-user ctx) (alist-ref ctx 'user))
    (define (ctx-settings ctx) (alist-ref ctx 'settings))
    (define (site-name ctx) (alist-ref (ctx-settings ctx) 'site-name "Filme"))

    (define (site-asset ctx kind)
      (string-append (url (string-append "/site/" kind))
                     "?v="
                     (alist-ref ctx 'theme-v "0")))

    (define (nav-link href label current here)
      `(a (@ (href ,(url href))
             (aria-current ,(if (equal? current here) "page" #f)))
          ,label))

    (define (page ctx title here body . scripts)
      (let* ((user (ctx-user ctx))
             (settings (ctx-settings ctx))
             (mode (alist-ref settings 'default-mode 'auto)))
        (string-append (html->string (html5
                                      `(@
                                        (lang "de")
                                        (data-theme-default
                                         ,(if (eq? mode 'auto)
                                              #f
                                              (symbol->string mode))))
                                      `(head
                                        (meta (@ (charset "utf-8")))
                                        (meta
                                         (@
                                          (name "viewport")
                                          (content
                                           "width=device-width, initial-scale=1")))
                                        (title
                                         ,(if
                                           title
                                           (string-append title " · "
                                                          (site-name ctx))
                                           (site-name ctx)))
                                        (link
                                         (@
                                          (rel "icon")
                                          (href
                                           ,(if (alist-ref settings 'favicon)
                                                (site-asset ctx "favicon")
                                                (asset "favicon.svg")))))
                                        (link
                                         (@ (rel "stylesheet")
                                            (href ,(asset "css/app.css"))))
                                        (link
                                         (@
                                          (rel "stylesheet")
                                          (href
                                           ,(string-append
                                             (url "/theme.css")
                                             "?v="
                                             (alist-ref ctx 'theme-v "0")))))
                                        (script
                                         (@
                                          (src ,(asset "js/theme-init.js"))))
                                        (script
                                         (@
                                          (src ,(asset "js/theme-toggle.js"))
                                          (defer #t)))
                                        ,@(map
                                           (lambda (s)
                                             `(script
                                               (@ (src ,(asset s))
                                                  (type "module"))))
                                           scripts))
                                      `(body
                                        (a (@ (class "skip") (href "#main"))
                                           "Zum Inhalt")
                                        (header
                                         (@ (class "app-header"))
                                         (a
                                          (@ (class "brand")
                                             (href ,(url "/")))
                                          ,(if
                                            (alist-ref settings 'logo)
                                            `(img
                                              (@
                                               (src ,(site-asset ctx "logo"))
                                               (alt "")
                                               (class "logo")))
                                            "")
                                          (span (@ (class "brand-name"))
                                                ,(site-name ctx)))
                                         (nav
                                          (@ (class "nav")
                                             (aria-label "Hauptnavigation"))
                                          ,(nav-link "/" "Videos" here 'home)
                                          ,@(if
                                             (can-edit? user)
                                             (list
                                              (nav-link "/upload" "Hochladen"
                                                        here
                                                        'upload)
                                              (nav-link "/manage" "Verwalten"
                                                        here
                                                        'manage))
                                             '())
                                          ,@(if
                                             (has-role? user 'admin)
                                             (list
                                              (nav-link "/admin/users"
                                                        "Benutzer"
                                                        here
                                                        'users)
                                              (nav-link "/admin/site"
                                                        "Darstellung"
                                                        here
                                                        'site))
                                             '())
                                          ,(if
                                            user
                                            `(a
                                              (@ (href ,(url "/profile"))
                                                 (class "who"))
                                              ,(user-name user))
                                            (nav-link "/login" "Anmelden" here
                                                      'login))
                                          (button
                                           (@
                                            (class "icon-btn js-theme-toggle")
                                            (type "button")
                                            (aria-label
                                             "Hell/Dunkel umschalten"))
                                           ,theme-icons)))
                                        (main (@ (id "main")) ,body)
                                        ,(let
                                             ((tag
                                               (alist-ref settings
                                                          'tagline
                                                          "")))
                                           (if
                                            (string=? tag "")
                                            ""
                                            `(footer (@ (class "app-footer"))
                                                     ,tag))))))
                       "\n")))

    ;; --- sign in / profile -----------------------------------------------

    (define (login-page ctx error? next)
      (page ctx "Anmelden"
            'login
            `(section (@ (class "card narrow"))
                      (h1 "Anmelden")
                      ,(notice (and error?
                                    '(error .
                                      "Benutzername oder Passwort ist falsch.")))
                      (form (@ (method "post") (action ,(url "/login")))
                            (input (@ (type "hidden")
                                      (name "next")
                                      (value ,(or next ""))))
                            (label "Benutzername"
                                   (input
                                    (@ (name "username")
                                       (autocomplete "username")
                                       (autofocus #t)
                                       (required #t))))
                            (label "Passwort"
                                   (input
                                    (@ (name "password")
                                       (type "password")
                                       (autocomplete "current-password")
                                       (required #t))))
                            (button (@ (type "submit") (class "primary"))
                                    "Anmelden")))))

    (define (profile-page ctx msg . new-token)
      (let ((user (ctx-user ctx)))
        (page ctx "Profil"
              'profile
              `(section (@ (class "card narrow"))
                        (h1 "Profil")
                        ,(notice msg)
                        (p "Angemeldet als "
                           (strong ,(user-name user))
                           " ("
                           ,(label-of role-labels (user-role user))
                           ").")
                        (h2 "Passwort ändern")
                        (form (@ (method "post")
                                 (action ,(url "/profile/password")))
                              (label "Aktuelles Passwort"
                                     (input
                                      (@ (name "current")
                                         (type "password")
                                         (autocomplete "current-password")
                                         (required #t))))
                              (label "Neues Passwort"
                                     (input
                                      (@ (name "new")
                                         (type "password")
                                         (minlength "8")
                                         (autocomplete "new-password")
                                         (required #t))))
                              (label "Neues Passwort bestätigen"
                                     (input
                                      (@ (name "confirm")
                                         (type "password")
                                         (minlength "8")
                                         (autocomplete "new-password")
                                         (required #t))))
                              (button (@ (type "submit") (class "primary"))
                                      "Passwort ändern"))
                        ,(if (has-role? user 'editor)
                             (tokens-section
                              user
                              (and (pair? new-token) (car new-token)))
                             "")
                        (form (@ (method "post")
                                 (action ,(url "/logout"))
                                 (class "logout-form"))
                              (button (@ (type "submit")) "Abmelden")))
              "js/confirm.js")))

    ;; Personal API tokens (for the aufnahme recorder's "Publish to filme").
    (define (tokens-section user new-token)
      `(div (@ (class "tokens"))
            (h2 "API-Tokens")
            (p (@ (class "hint"))
               "Mit einem Token können Programme wie die Aufnahme-App in Ihrem Namen Videos "
               "hochladen. Sie landen als Entwurf in der Übersicht.")
            ,(if new-token
                 `(div (@ (class "notice success new-token"))
                       (p (strong
                           "Neues Token – jetzt kopieren, es wird nicht wieder angezeigt:"))
                       (input (@ (readonly #t)
                                 (value ,new-token)
                                 (class "token-value")
                                 (aria-label "Neues Token"))))
                 "")
            ,(let ((ts (user-tokens user)))
               (if (null? ts)
                   '(p (@ (class "hint")) "Noch keine Tokens.")
                   `(ul (@ (class "token-list"))
                        ,@(map (lambda (t)
                                 `(li (span
                                       ,(alist-ref t 'label "Token")
                                       (span
                                        (@ (class "hint"))
                                        ,(string-append
                                          " · seit "
                                          (format-date
                                           (alist-ref t 'created 0)))))
                                      (form
                                       (@
                                        (method "post")
                                        (class "inline js-confirm")
                                        (data-confirm
                                         "Dieses Token widerrufen?")
                                        (action
                                         ,(url
                                           (string-append "/profile/tokens/"
                                                          (alist-ref t 'id)
                                                          "/delete"))))
                                       (button
                                        (@ (type "submit") (class "danger"))
                                        "Widerrufen"))))
                               ts))))
            (form (@ (method "post")
                     (action ,(url "/profile/tokens"))
                     (class "inline"))
                  (input (@ (name "label")
                            (placeholder "Bezeichnung, z. B. Aufnahme")
                            (maxlength "60")
                            (aria-label "Bezeichnung")))
                  (button (@ (type "submit")) "Token erstellen"))))

    ;; --- the video grid ----------------------------------------------------

    (define (thumb-url meta size)
      (string-append (url (string-append "/media/"
                                         (alist-ref meta 'id)
                                         "/"
                                         size))
                     "?v="
                     (number->string (alist-ref meta 'updated 0))))

    (define (badge meta)
      (case (alist-ref meta 'visibility)
        ((restricted)
         `(span (@ (class "badge") (title "Eingeschränkt")) ,lock-icon))
        ((draft)
         `(span (@ (class "badge draft") (title "Entwurf")) ,draft-icon))
        (else "")))

    ;; show-thumb?: whether the viewer may see the thumbnail (else a
    ;; placeholder with a lock).
    (define (video-card meta show-thumb?)
      `(li (@ (class "card video-card"))
           (a (@ (href ,(url (string-append "/v/" (alist-ref meta 'id)))))
              (div (@ (class "thumb"))
                   ,(cond ((not (eq? (alist-ref meta 'status) 'ready))
                           `(span (@ (class "thumb-placeholder"))
                                  ,(if (eq? (alist-ref meta 'status) 'failed)
                                       "Fehler"
                                       "Wird verarbeitet …")))
                          (show-thumb? `(img
                                         (@
                                          (src
                                           ,(thumb-url meta "thumb-sm.jpg"))
                                          (alt "")
                                          (loading "lazy")
                                          (width "480")
                                          (height "270"))))
                          (else `(span (@ (class "thumb-placeholder"))
                                       ,lock-icon)))
                   (span (@ (class "duration"))
                         ,(format-duration (alist-ref meta 'duration 0)))
                   ,(badge meta))
              (h2 (@ (class "video-title"))
                  ,(let ((t (alist-ref meta 'title "")))
                     (if (string=? t "") "(ohne Titel)" t)))
              (p (@ (class "meta-line"))
                 ,(format-date (alist-ref meta 'created 0))))))

    (define (query-string pairs)
      (let loop ((ps pairs) (acc ""))
        (cond ((null? ps) acc)
              ((or (not (cdar ps)) (equal? (cdar ps) "")) (loop (cdr ps) acc))
              (else (loop (cdr ps)
                          (string-append acc
                                         (if (string=? acc "") "?" "&")
                                         (caar ps)
                                         "="
                                         (url-encode (cdar ps))))))))

    (define (url-encode s)
      (let ((bv (string->utf8 s)))
        (let loop ((i 0) (acc '()))
          (if (= i (bytevector-length bv))
              (apply string-append (reverse acc))
              (let ((b (bytevector-u8-ref bv i)))
                (loop (+ i 1)
                      (cons (if (or (and (>= b 48) (<= b 57))
                                    (and (>= b 65) (<= b 90))
                                    (and (>= b 97) (<= b 122))
                                    (memv b '(45 46 95 126)))
                                (string (integer->char b))
                                (string-append
                                 "%"
                                 (if (< b 16) "0" "")
                                 (string-upcase (number->string b 16))))
                            acc)))))))

    ;; videos: the hits of this page; total: hits overall.
    (define (home-page ctx videos total pg pages q tag sort tags thumb-ok?)
      (page ctx #f
            'home
            `(section (form (@ (class "search")
                               (method "get")
                               (action ,(url "/"))
                               (role "search"))
                            (input (@ (type "search")
                                      (name "q")
                                      (value ,(or q ""))
                                      (placeholder "Suchen …")
                                      (aria-label "Suchen")))
                            ,(if tag
                                 `(input
                                   (@ (type "hidden")
                                      (name "tag")
                                      (value ,tag)))
                                 "")
                            (select (@ (name "sort")
                                       (aria-label "Sortierung"))
                                    ,@(map
                                       (lambda (o)
                                         `(option
                                           (@
                                            (value ,(car o))
                                            (selected ,(equal? (car o) sort)))
                                           ,(cdr o)))
                                       '(("newest" . "Neueste zuerst")
                                         ("oldest" . "Älteste zuerst")
                                         ("title" . "Titel A–Z"))))
                            (button (@ (type "submit")) "Suchen"))
                      ,(if (null? tags)
                           ""
                           `(ul (@ (class "tags") (aria-label "Schlagwörter"))
                                (li (a (@
                                        (href
                                         ,(url
                                           (string-append
                                            "/"
                                            (query-string
                                             `(("q" . ,q)
                                               ("sort" . ,sort))))))
                                        (class ,(if tag "tag" "tag active")))
                                       "Alle"))
                                ,@(map (lambda (t)
                                         `(li
                                           (a
                                            (@
                                             (href
                                              ,(url
                                                (string-append
                                                 "/"
                                                 (query-string
                                                  `(("q" . ,q)
                                                    ("tag" . ,t)
                                                    ("sort" . ,sort))))))
                                             (class
                                              ,(if (equal? t tag)
                                                   "tag active"
                                                   "tag")))
                                            ,t)))
                                       tags)))
                      ,(if (null? videos)
                           `(p (@ (class "empty"))
                               ,(if (or (and q (not (string=? q ""))) tag)
                                    "Keine Videos gefunden."
                                    "Noch keine Videos."))
                           `(ul (@ (class "grid"))
                                ,@(map (lambda (m)
                                         (video-card m (thumb-ok? m)))
                                       videos)))
                      ,(if (> pages 1)
                           `(nav (@ (class "pager") (aria-label "Seiten"))
                                 ,(if (> pg 1)
                                      `(a
                                        (@
                                         (href
                                          ,(url
                                            (string-append
                                             "/"
                                             (query-string
                                              `(("q" . ,q)
                                                ("tag" . ,tag)
                                                ("sort" . ,sort)
                                                ("page"
                                                 .
                                                 ,(number->string
                                                   (- pg 1)))))))))
                                        "‹ Zurück")
                                      "")
                                 (span ,(string-append
                                         "Seite "
                                         (number->string pg)
                                         " von "
                                         (number->string pages)))
                                 ,(if (< pg pages)
                                      `(a
                                        (@
                                         (href
                                          ,(url
                                            (string-append
                                             "/"
                                             (query-string
                                              `(("q" . ,q)
                                                ("tag" . ,tag)
                                                ("sort" . ,sort)
                                                ("page"
                                                 .
                                                 ,(number->string
                                                   (+ pg 1)))))))))
                                        "Weiter ›")
                                      ""))
                           ""))))

    ;; --- watch page ------------------------------------------------------

    (define (description-html text)
      (if (string=? text "") "" (raw (markdown->html text))))

    (define (watch-page ctx meta)
      (let* ((id (alist-ref meta 'id))
             (user (ctx-user ctx))
             (ready? (eq? (alist-ref meta 'status) 'ready))
             (src (string-append (url (string-append "/media/" id
                                                     "/video.mp4"))
                                 "?v="
                                 (number->string
                                  (alist-ref meta 'updated 0)))))
        (page ctx
              (alist-ref meta 'title "Video")
              'watch
              `(article (@ (class "watch"))
                        ,(if ready?
                             `(video (@
                                      (class "player js-player")
                                      (controls #t)
                                      (preload "metadata")
                                      (playsinline #t)
                                      (data-video-id ,id)
                                      (poster ,(thumb-url meta "thumb.jpg")))
                                     (source
                                      (@ (src ,src) (type "video/mp4")))
                                     "Ihr Browser kann dieses Video nicht abspielen.")
                             `(div (@ (class "player placeholder"))
                                   ,(if
                                     (eq? (alist-ref meta 'status) 'failed)
                                     (string-append
                                      "Verarbeitung fehlgeschlagen: "
                                      (or (alist-ref meta 'error) ""))
                                     "Das Video wird verarbeitet …")))
                        (div (@ (class "watch-head"))
                             (h1 ,(let ((t (alist-ref meta 'title "")))
                                    (if (string=? t "") "(ohne Titel)" t)))
                             (div (@ (class "actions"))
                                  (button
                                   (@ (type "button")
                                      (class "js-copy-link")
                                      (data-url
                                       ,(url (string-append "/v/" id))))
                                   "Link kopieren")
                                  ,(if (and
                                        ready?
                                        (alist-ref meta 'allow-download #f))
                                       `(a
                                         (@
                                          (class "button")
                                          (href
                                           ,(string-append src
                                                           "&download=1")))
                                         "Herunterladen")
                                       "")
                                  ,(if (can-edit? user)
                                       `(a
                                         (@
                                          (class "button")
                                          (href
                                           ,(url
                                             (string-append "/v/" id
                                                            "/edit"))))
                                         "Bearbeiten")
                                       "")))
                        (p (@ (class "meta-line"))
                           ,(format-date (alist-ref meta 'created 0))
                           " · "
                           ,(format-duration (alist-ref meta 'duration 0))
                           ,@(if (eq? (alist-ref meta 'visibility) 'public)
                                 '()
                                 (list " · "
                                       (label-of
                                        visibility-labels
                                        (alist-ref meta 'visibility)))))
                        ,(let ((tags (alist-ref meta 'tags '())))
                           (if (null? tags)
                               ""
                               `(ul (@ (class "tags"))
                                    ,@(map
                                       (lambda (t)
                                         `(li
                                           (a
                                            (@
                                             (class "tag")
                                             (href
                                              ,(url
                                                (string-append
                                                 "/?tag="
                                                 (url-encode t)))))
                                            ,t)))
                                       tags))))
                        (div (@ (class "description"))
                             ,(description-html
                               (alist-ref meta 'description ""))))
              "js/player.js")))

    (define (unlock-page ctx meta error)
      (page ctx "Geschützt"
            'unlock
            `(section (@ (class "card narrow"))
                      (h1 ,lock-icon
                          " "
                          ,(let ((t (alist-ref meta 'title "")))
                             (if (string=? t "") "Geschütztes Video" t)))
                      (p
                       "Dieses Video ist geschützt. Geben Sie die Passphrase ein oder "
                       (a (@ (href ,(url
                                     (string-append "/login?next=/v/"
                                                    (alist-ref meta 'id)))))
                          "melden Sie sich an")
                       ".")
                      ,(notice (and error (cons 'error error)))
                      (form (@ (method "post")
                               (action ,(url
                                         (string-append "/v/"
                                                        (alist-ref meta 'id)
                                                        "/unlock"))))
                            (label "Passphrase"
                                   (input
                                    (@ (name "passphrase")
                                       (type "password")
                                       (autocomplete "off")
                                       (autofocus #t)
                                       (required #t))))
                            (button (@ (type "submit") (class "primary"))
                                    "Öffnen")))))

    ;; --- editor pages ----------------------------------------------------

    (define (upload-page ctx max-hint)
      (page ctx "Hochladen"
            'upload
            `(section (h1 "Videos hochladen")
                      (div (@ (class "dropzone js-dropzone") (tabindex "0"))
                           (p "MP4-Dateien hierher ziehen oder")
                           (label (@ (class "button primary"))
                                  "Dateien auswählen"
                                  (input
                                   (@ (type "file")
                                      (class "js-file visually-hidden")
                                      (accept "video/mp4,.mp4,.m4v")
                                      (multiple #t))))
                           (p (@ (class "hint"))
                              "Empfohlen: H.264-Video mit AAC-Ton. Grosse Dateien werden in "
                              "Teilen übertragen; ein Abbruch kann fortgesetzt werden. "
                              ,max-hint))
                      (ul (@ (class "upload-list js-upload-list")))
                      (template (@ (id "upload-item"))
                                (li (@ (class "card upload-item"))
                                    (div (@ (class "upload-name")))
                                    (progress (@ (max "100") (value "0")))
                                    (div
                                     (@ (class "upload-status")
                                        (aria-live "polite")))
                                    (div
                                     (@ (class "upload-actions"))
                                     (button
                                      (@ (type "button") (class "js-pause"))
                                      "Pause")
                                     (button
                                      (@ (type "button") (class "js-cancel"))
                                      "Abbrechen")
                                     (a
                                      (@ (class "button primary js-edit")
                                         (hidden #t))
                                      "Bearbeiten")))))
            "js/upload.js"))

    (define (status-label meta)
      (case (alist-ref meta 'status)
        ((ready) (if (alist-ref meta 'warning) "Bereit (Hinweis)" "Bereit"))
        ((failed) "Fehler")
        (else "Verarbeitung …")))

    (define (manage-page ctx videos msg)
      (page ctx "Verwalten"
            'manage
            `(section (h1 "Alle Videos")
                      ,(notice msg)
                      ,(if (null? videos)
                           `(p (@ (class "empty"))
                               "Noch keine Videos. "
                               (a (@ (href ,(url "/upload")))
                                  "Jetzt hochladen"))
                           `(div (@ (class "table-wrap"))
                                 (table
                                  (@ (class "data"))
                                  (thead
                                   (tr (th "Titel")
                                       (th "Sichtbarkeit")
                                       (th "Status")
                                       (th (@ (class "num")) "Dauer")
                                       (th (@ (class "num")) "Grösse")
                                       (th "Hochgeladen")
                                       (th "")))
                                  (tbody
                                   ,@(map
                                      (lambda (m)
                                        (let ((id (alist-ref m 'id)))
                                          `(tr
                                            (td
                                             (a
                                              (@
                                               (href
                                                ,(url
                                                  (string-append "/v/" id))))
                                              ,(let
                                                   ((t
                                                     (alist-ref m 'title "")))
                                                 (if (string=? t "")
                                                     "(ohne Titel)"
                                                     t))))
                                            (td
                                             ,(label-of
                                               visibility-labels
                                               (alist-ref m 'visibility))
                                             ,(if
                                               (alist-ref m 'passphrase-hash)
                                               " 🔑"
                                               ""))
                                            (td ,(status-label m))
                                            (td
                                             (@ (class "num"))
                                             ,(format-duration
                                               (alist-ref m 'duration 0)))
                                            (td
                                             (@ (class "num"))
                                             ,(format-size
                                               (alist-ref m 'size 0)))
                                            (td
                                             ,(format-date
                                               (alist-ref m 'created 0))
                                             ,(let
                                                  ((by
                                                    (alist-ref m
                                                               'created-by)))
                                                (if by
                                                    (string-append " · " by)
                                                    "")))
                                            (td
                                             (a
                                              (@
                                               (href
                                                ,(url
                                                  (string-append "/v/" id
                                                                 "/edit"))))
                                              "Bearbeiten")))))
                                      videos))))))))

    (define (edit-page ctx meta msg)
      (let* ((id (alist-ref meta 'id))
             (ready? (eq? (alist-ref meta 'status) 'ready))
             (base (url (string-append "/v/" id))))
        (page ctx "Bearbeiten"
              'edit
              `(section (@ (class "edit")
                           (data-video-id ,id)
                           (data-status
                            ,(symbol->string (alist-ref meta 'status)))
                           (data-duration
                            ,(number->string (alist-ref meta 'duration 0))))
                        (p (a (@ (href ,base)) "‹ Zum Video"))
                        (h1 "Video bearbeiten")
                        ,(notice msg)
                        ,(if (alist-ref meta 'warning)
                             `(p (@ (class "notice warning"))
                                 ,(alist-ref meta 'warning))
                             "")
                        ,(case (alist-ref meta 'status)
                           ((failed)
                            `(p (@ (class "notice error"))
                                "Verarbeitung fehlgeschlagen: "
                                ,(or (alist-ref meta 'error) "")))
                           ((processing)
                            `(p (@ (class "notice js-processing"))
                                "Das Video wird verarbeitet … Diese Seite aktualisiert sich."))
                           (else ""))
                        (div (@ (class "edit-grid"))
                             (form (@ (method "post")
                                      (action ,(string-append base "/edit"))
                                      (class "card"))
                                   (h2 "Angaben")
                                   (label
                                    "Titel"
                                    (input
                                     (@ (name "title")
                                        (required #t)
                                        (maxlength "200")
                                        (value ,(alist-ref meta 'title "")))))
                                   (label
                                    "Beschreibung (Markdown)"
                                    (textarea
                                     (@ (name "description") (rows "8"))
                                     ,(alist-ref meta 'description "")))
                                   (label
                                    "Schlagwörter (durch Komma getrennt)"
                                    (input
                                     (@
                                      (name "tags")
                                      (value
                                       ,(let loop
                                            ((ts (alist-ref meta 'tags '()))
                                             (acc ""))
                                          (if
                                           (null? ts)
                                           acc
                                           (loop
                                            (cdr ts)
                                            (if
                                             (string=? acc "")
                                             (car ts)
                                             (string-append acc ", "
                                                            (car ts))))))))))
                                   (fieldset
                                    (legend "Sichtbarkeit")
                                    ,@(map
                                       (lambda (v)
                                         `(label
                                           (@ (class "radio"))
                                           (input
                                            (@
                                             (type "radio")
                                             (name "visibility")
                                             (value ,(symbol->string (car v)))
                                             (checked
                                              ,(eq?
                                                (car v)
                                                (alist-ref meta
                                                           'visibility)))))
                                           (span
                                            (strong ,(cdr v))
                                            " – "
                                            ,(case (car v)
                                               ((public) "für alle sichtbar")
                                               ((restricted)
                                                "nur für angemeldete Benutzer oder mit Passphrase")
                                               (else
                                                "nur für die Redaktion")))))
                                       visibility-labels))
                                   (fieldset
                                    (legend
                                     "Passphrase (für eingeschränkte Videos)")
                                    (p (@ (class "hint"))
                                       ,(if
                                         (alist-ref meta 'passphrase-hash)
                                         "Eine Passphrase ist gesetzt. Eine neue ersetzt sie; bisherige Freigaben verfallen."
                                         "Keine Passphrase gesetzt: nur angemeldete Benutzer sehen das Video."))
                                    (label
                                     "Neue Passphrase"
                                     (input
                                      (@ (name "passphrase")
                                         (type "password")
                                         (autocomplete "new-password"))))
                                    ,(if
                                      (alist-ref meta 'passphrase-hash)
                                      `(label
                                        (@ (class "check"))
                                        (input
                                         (@ (type "checkbox")
                                            (name "remove-passphrase")
                                            (value "1")))
                                        "Passphrase entfernen")
                                      ""))
                                   (label
                                    (@ (class "check"))
                                    (input
                                     (@
                                      (type "checkbox")
                                      (name "allow-download")
                                      (value "1")
                                      (checked
                                       ,(alist-ref meta 'allow-download #f))))
                                    "Herunterladen erlauben")
                                   (button
                                    (@ (type "submit") (class "primary"))
                                    "Speichern"))
                             (div (@ (class "card thumb-editor"))
                                  (h2 "Vorschaubild")
                                  ,(if ready?
                                       `(div
                                         (img
                                          (@
                                           (class
                                            "js-thumb-preview current-thumb")
                                           (src ,(thumb-url meta "thumb.jpg"))
                                           (alt "Aktuelles Vorschaubild")))
                                         (video
                                          (@
                                           (class "js-frame-picker")
                                           (controls #t)
                                           (preload "metadata")
                                           (muted #t)
                                           (src
                                            ,(url
                                              (string-append "/media/" id
                                                             "/video.mp4")))))
                                         (div
                                          (@ (class "button-row"))
                                          (button
                                           (@ (type "button")
                                              (class "primary js-use-frame"))
                                           "Aktuelles Bild verwenden")
                                          ,@(map
                                             (lambda (p)
                                               `(button
                                                 (@ (type "button")
                                                    (class "js-frame-at")
                                                    (data-fraction ,(car p)))
                                                 ,(cdr p)))
                                             '(("0" . "Anfang")
                                               ("0.1" . "10 %")
                                               ("0.25" . "25 %")
                                               ("0.5" . "50 %"))))
                                         (label
                                          "Eigenes Bild (JPEG, PNG, WebP)"
                                          (input
                                           (@
                                            (type "file")
                                            (class "js-thumb-file")
                                            (accept
                                             "image/jpeg,image/png,image/webp"))))
                                         (p
                                          (@ (class "hint js-thumb-status")
                                             (aria-live "polite"))))
                                       '(p
                                         (@ (class "hint"))
                                         "Verfügbar, sobald das Video verarbeitet ist."))))
                        (div (@ (class "card"))
                             (h2 "Datei")
                             (dl (@ (class "facts"))
                                 (dt "Originaldatei")
                                 (dd ,(alist-ref meta 'original-filename ""))
                                 (dt "Grösse")
                                 (dd ,(format-size (alist-ref meta 'size 0)))
                                 (dt "Auflösung")
                                 (dd ,(string-append
                                       (number->string
                                        (alist-ref meta 'width 0))
                                       "×"
                                       (number->string
                                        (alist-ref meta 'height 0))))
                                 (dt "Codecs")
                                 (dd ,(string-append
                                       (alist-ref meta 'video-codec "")
                                       " / "
                                       (alist-ref meta 'audio-codec "")))
                                 (dt "Hochgeladen")
                                 (dd ,(format-date
                                       (alist-ref meta 'created 0))
                                     ,(let ((by (alist-ref meta 'created-by)))
                                        (if by
                                            (string-append " von " by)
                                            ""))))
                             (div (@ (class "replace"))
                                  (label
                                   (@ (class "button"))
                                   "Videodatei ersetzen …"
                                   (input
                                    (@ (type "file")
                                       (class
                                        "js-replace-file visually-hidden")
                                       (accept "video/mp4,.mp4,.m4v"))))
                                  (progress
                                   (@ (class "js-replace-progress")
                                      (max "100")
                                      (value "0")
                                      (hidden #t)))
                                  (span
                                   (@ (class "hint js-replace-status")
                                      (aria-live "polite")))))
                        (form (@ (method "post")
                                 (action ,(string-append base "/delete"))
                                 (class "card danger-zone js-confirm")
                                 (data-confirm "Das Video wirklich löschen?"))
                              (h2 "Löschen")
                              (p
                               "Das Video kommt in den Papierkorb und wird nach einiger Zeit endgültig gelöscht.")
                              (button (@ (type "submit") (class "danger"))
                                      "Video löschen")))
              "js/edit.js"
              "js/confirm.js")))

    ;; --- admin pages -----------------------------------------------------

    (define (admin-users-page ctx users msg)
      (let ((me (user-name (ctx-user ctx))))
        (page ctx "Benutzer"
              'users
              `(section (h1 "Benutzer")
                        ,(notice msg)
                        (div (@ (class "table-wrap"))
                             (table (@ (class "data"))
                                    (thead
                                     (tr (th "Name")
                                         (th "Rolle")
                                         (th "Status")
                                         (th "Seit")
                                         (th "")))
                                    (tbody
                                     ,@(map
                                        (lambda (u)
                                          (let*
                                              ((name (user-name u))
                                               (action
                                                (url
                                                 (string-append
                                                  "/admin/users/" name))))
                                            `(tr
                                              (td ,name)
                                              (td
                                               ,(if
                                                 (string=? name me)
                                                 (label-of role-labels
                                                           (user-role u))
                                                 `(form
                                                   (@ (method "post")
                                                      (action ,action)
                                                      (class "inline"))
                                                   (input
                                                    (@ (type "hidden")
                                                       (name "op")
                                                       (value "role")))
                                                   (select
                                                    (@ (name "role")
                                                       (aria-label "Rolle"))
                                                    ,@(map
                                                       (lambda (r)
                                                         `(option
                                                           (@
                                                            (value
                                                             ,(symbol->string
                                                               (car r)))
                                                            (selected
                                                             ,(eq?
                                                               (car r)
                                                               (user-role
                                                                u))))
                                                           ,(cdr r)))
                                                       role-labels))
                                                   (button (@ (type "submit"))
                                                           "Setzen"))))
                                              (td
                                               ,(if (user-disabled? u)
                                                    "gesperrt"
                                                    "aktiv"))
                                              (td
                                               ,(format-date
                                                 (user-created u)))
                                              (td
                                               ,(if
                                                 (string=? name me)
                                                 "(Sie)"
                                                 `(div
                                                   (@ (class "button-row"))
                                                   (form
                                                    (@ (method "post")
                                                       (action ,action)
                                                       (class "inline"))
                                                    (input
                                                     (@
                                                      (type "hidden")
                                                      (name "op")
                                                      (value
                                                       ,(if (user-disabled? u)
                                                            "enable"
                                                            "disable"))))
                                                    (button
                                                     (@ (type "submit"))
                                                     ,(if (user-disabled? u)
                                                          "Entsperren"
                                                          "Sperren")))
                                                   (form
                                                    (@ (method "post")
                                                       (action ,action)
                                                       (class "inline"))
                                                    (input
                                                     (@ (type "hidden")
                                                        (name "op")
                                                        (value "password")))
                                                    (input
                                                     (@
                                                      (type "password")
                                                      (name "password")
                                                      (placeholder
                                                       "Neues Passwort")
                                                      (minlength "8")
                                                      (autocomplete
                                                       "new-password")
                                                      (required #t)
                                                      (aria-label
                                                       "Neues Passwort")))
                                                    (button
                                                     (@ (type "submit"))
                                                     "Setzen"))
                                                   (form
                                                    (@
                                                     (method "post")
                                                     (action ,action)
                                                     (class
                                                      "inline js-confirm")
                                                     (data-confirm
                                                      ,(string-append name
                                                                      " löschen?")))
                                                    (input
                                                     (@ (type "hidden")
                                                        (name "op")
                                                        (value "delete")))
                                                    (button
                                                     (@ (type "submit")
                                                        (class "danger"))
                                                     "Löschen"))))))))
                                        users))))
                        (form (@ (method "post")
                                 (action ,(url "/admin/users"))
                                 (class "card narrow"))
                              (h2 "Neuer Benutzer")
                              (label "Benutzername"
                                     (input
                                      (@ (name "username")
                                         (required #t)
                                         (autocomplete "off"))))
                              (label "Passwort"
                                     (input
                                      (@ (name "password")
                                         (type "password")
                                         (minlength "8")
                                         (autocomplete "new-password")
                                         (required #t))))
                              (label "Rolle"
                                     (select
                                      (@ (name "role"))
                                      ,@(map
                                         (lambda (r)
                                           `(option
                                             (@
                                              (value
                                               ,(symbol->string (car r))))
                                             ,(cdr r)))
                                         role-labels)))
                              (p (@ (class "hint"))
                                 "Betrachter sehen eingeschränkte Videos; Redaktion lädt hoch und bearbeitet; "
                                 "Administration verwaltet zusätzlich Benutzer und Darstellung.")
                              (button (@ (type "submit") (class "primary"))
                                      "Anlegen")))
              "js/confirm.js")))

    (define (color-inputs scheme colors)
      `(fieldset (legend ,(if (eq? scheme 'light)
                              "Helles Farbschema"
                              "Dunkles Farbschema"))
                 (div (@ (class "color-grid"))
                      ,@(map (lambda (k)
                               (let
                                   ((name
                                     (string-append
                                      (symbol->string scheme)
                                      "-"
                                      (symbol->string (car k)))))
                                 `(label
                                   (@ (class "color"))
                                   (input
                                    (@ (type "color")
                                       (name ,name)
                                       (class "js-color")
                                       (data-scheme ,(symbol->string scheme))
                                       (data-key ,(symbol->string (car k)))
                                       (value ,(alist-ref colors (car k)))))
                                   (span ,(cdr k)))))
                             color-keys))))

    (define (admin-site-page ctx msg warnings)
      (let ((s (ctx-settings ctx)))
        (page ctx "Darstellung"
              'site
              `(section (h1 "Darstellung")
                        ,(notice msg)
                        ,@(map (lambda (w)
                                 `(p (@ (class "notice warning")) ,w))
                               warnings)
                        (form (@ (method "post")
                                 (action ,(url "/admin/site"))
                                 (class "card js-site-form"))
                              (label "Name der Seite"
                                     (input
                                      (@
                                       (name "site-name")
                                       (required #t)
                                       (maxlength "80")
                                       (value ,(alist-ref s 'site-name "")))))
                              (label "Fusszeile (optional)"
                                     (input
                                      (@ (name "tagline")
                                         (maxlength "200")
                                         (value ,(alist-ref s 'tagline "")))))
                              (fieldset
                               (legend "Standard-Farbschema")
                               ,@(map (lambda (m)
                                        `(label
                                          (@ (class "radio"))
                                          (input
                                           (@
                                            (type "radio")
                                            (name "default-mode")
                                            (value ,(symbol->string (car m)))
                                            (checked
                                             ,(eq?
                                               (car m)
                                               (alist-ref s
                                                          'default-mode
                                                          'auto)))))
                                          ,(cdr m)))
                                      '((auto . "Wie das Betriebssystem")
                                        (light . "Hell")
                                        (dark . "Dunkel"))))
                              ,(color-inputs 'light (alist-ref s 'light))
                              ,(color-inputs 'dark (alist-ref s 'dark))
                              (div (@ (class "preview-pair"))
                                   ,@(map
                                      (lambda (scheme)
                                        `(div
                                          (@ (class "preview js-preview")
                                             (data-scheme ,scheme))
                                          (div
                                           (@ (class "pv-surface"))
                                           (strong "Beispieltitel")
                                           (p (@ (class "pv-muted"))
                                              "06.11.2026 · 12:34")
                                           (p
                                            "Text mit "
                                            (span (@ (class "pv-link"))
                                                  "Link"))
                                           (span (@ (class "pv-button"))
                                                 "Button"))))
                                      '("light" "dark")))
                              (button (@ (type "submit") (class "primary"))
                                      "Speichern"))
                        (div (@ (class "card"))
                             (h2 "Logo und Favicon")
                             (p (@ (class "hint"))
                                "PNG, WebP oder SVG, höchstens 1 MB. Das Logo erscheint links in der Kopfzeile.")
                             ,@(map (lambda (k)
                                      `(div
                                        (@ (class "logo-row"))
                                        (strong
                                         ,(if (equal? k "logo")
                                              "Logo"
                                              "Favicon"))
                                        ,(if
                                          (alist-ref s (string->symbol k))
                                          `(img
                                            (@ (src ,(site-asset ctx k))
                                               (alt "")
                                               (class "logo-preview")))
                                          '(span (@ (class "hint")) "keines"))
                                        (form
                                         (@
                                          (method "post")
                                          (enctype "multipart/form-data")
                                          (action
                                           ,(url
                                             (string-append "/admin/site/"
                                                            k)))
                                          (class "inline js-image-upload"))
                                         (input
                                          (@
                                           (type "file")
                                           (name "file")
                                           (accept
                                            "image/png,image/webp,image/svg+xml")
                                           (aria-label "Bild wählen")))
                                         (button (@ (type "submit"))
                                                 "Hochladen"))
                                        ,(if
                                          (alist-ref s (string->symbol k))
                                          `(form
                                            (@
                                             (method "post")
                                             (action
                                              ,(url
                                                (string-append "/admin/site/"
                                                               k "/delete")))
                                             (class "inline"))
                                            (button (@ (type "submit"))
                                                    "Entfernen"))
                                          "")))
                                    '("logo" "favicon")))
                        (form (@ (method "post")
                                 (action ,(url "/admin/site/reset"))
                                 (class "card js-confirm")
                                 (data-confirm
                                  "Alle Farben und Namen auf die Standardwerte zurücksetzen?"))
                              (button (@ (type "submit"))
                                      "Auf Standard zurücksetzen")))
              "js/site-settings.js"
              "js/confirm.js")))

    ;; --- errors --------------------------------------------------------------

    (define (not-found-page ctx)
      (page ctx "Nicht gefunden" #f
            `(section (h1 "Nicht gefunden")
                      (p
                       "Diese Seite oder dieses Video gibt es nicht (mehr).")
                      (p (a (@ (href ,(url "/"))) "Zur Übersicht")))))

    (define (error-page ctx)
      (page ctx "Fehler" #f
            '(section (h1 "Etwas ist schiefgelaufen")
                      (p "Der Fehler wurde protokolliert."))))))
