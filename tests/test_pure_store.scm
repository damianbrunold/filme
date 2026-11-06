;; Tests for the data layer without HTTP or ffmpeg: store, library index,
;; search, uploads, users, the access rules and the theme.
;; Run: scm tests/test_pure_store.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scm fs)
        (scm module)
        (scm string)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app access)
        (app store)
        (app theme)
        (app uploads)
        (app users)
        (app videos))

(test-runner-factory scm-test-runner)
(test-begin "filme store")

(define tmp (mktempdir '(prefix . "filme-store")))

(test-group "store"
  (let ((p (string-append tmp "/a/b/x.scm")))
    (test-equal 'none (read-datum p 'none))
    (write-datum! p '((a . 1) (b "x" "y")))
    (test-equal '((a . 1) (b "x" "y")) (read-datum p #f))
    (test-assert "no temp file left"
                 (not (file-exists? (string-append p ".tmp")))))
  (test-equal 2 (alist-ref '((a . 1) (b . 2)) 'b))
  (test-equal 'd (alist-ref '((a . 1)) 'z 'd))
  (test-equal '((a . 9) (b . 2)) (alist-set '((a . 1) (b . 2)) 'a 9))
  (test-equal '((a . 1) (c . 3)) (alist-set '((a . 1)) 'c 3))
  (let ((id (new-id)))
    (test-equal 11 (string-length id))
    (test-assert (valid-id? id)))
  (test-assert (not (valid-id? "../etc")))
  (test-assert (not (valid-id? "a/b..")))
  (test-equal 42 (with-data-lock tmp (lambda () 42))))

(define lib (make-library tmp))

(test-group "library"
  (let* ((a (video-create! lib
                           '((title . "Git Grundlagen")
                             (tags "git" "intro")
                             (description . "Commits und Branches")
                             (created . 100))))
         (b (video-create! lib
                           '((title . "Docker Compose")
                             (tags "docker")
                             (created . 200))))
         (c (video-create! lib '((title . "Alpha") (created . 50)))))
    (test-equal 3 (length (video-list lib)))
    (test-equal "newest first" "Docker Compose"
                (alist-ref (car (video-list lib)) 'title))
    (test-equal 'draft (alist-ref a 'visibility))
    (test-equal 'processing (alist-ref a 'status))
    (test-assert (file-exists? (string-append tmp "/videos/"
                                              (alist-ref a 'id)
                                              "/meta.scm")))
    (test-equal "reloaded from disk" 3
                (length (video-list (make-library tmp))))
    (let
        ((m (video-update! lib
                           (alist-ref a 'id)
                           (lambda (m) (alist-set m 'title "Git Basics")))))
      (test-equal "Git Basics" (alist-ref m 'title))
      (test-equal "Git Basics"
                  (alist-ref (video-ref (make-library tmp) (alist-ref a 'id))
                             'title)))
    (test-equal #f (video-update! lib "nosuchvideo" (lambda (m) m)))

    (test-group "search"
      (define (titles ms) (map (lambda (m) (alist-ref m 'title)) ms))
      (test-equal '("Git Basics")
                  (titles (search-videos (video-list lib)
                                         "BRANCHES"
                                         #f
                                         'newest)))
      (test-equal '("Git Basics")
                  (titles (search-videos (video-list lib)
                                         "git commits"
                                         #f
                                         'newest)))
      (test-equal '()
                  (titles (search-videos (video-list lib)
                                         "git docker"
                                         #f
                                         'newest)))
      (test-equal '("Docker Compose")
                  (titles (search-videos (video-list lib)
                                         ""
                                         "docker"
                                         'newest)))
      (test-equal '("Alpha" "Docker Compose" "Git Basics")
                  (titles (search-videos (video-list lib) #f #f 'title)))
      (test-equal '("Alpha" "Git Basics" "Docker Compose")
                  (titles (search-videos (video-list lib) #f #f 'oldest)))
      (test-equal '("docker" "git" "intro") (all-tags (video-list lib)))
      (test-equal '("git" "intro") (normalize-tags " Git, intro ,, GIT ")))

    (test-group "trash"
      (test-assert (video-trash! lib (alist-ref c 'id)))
      (test-equal 2 (length (video-list lib)))
      (test-assert (directory-exists? (string-append tmp "/trash/"
                                                     (alist-ref c 'id))))
      (test-equal '() (video-purge-trash! lib 3600))
      (test-equal (list (alist-ref c 'id)) (video-purge-trash! lib -1)))

    (test-group "refresh after another process changes videos/"
      (let ((other (make-library tmp)))
        (video-create! other '((title . "From the CLI")))
        (library-touch! other)
        (test-equal 2 (length (video-list lib)))
        (library-refresh! lib)
        (test-equal 3 (length (video-list lib)))))))

(test-group "uploads"
  (let* ((u (upload-start! tmp "alice" "x.mp4" 10 #f))
         (id (alist-ref u 'id)))
    (test-equal 0 (upload-offset tmp id))
    (test-equal 4 (upload-append! tmp id 0 (bytevector 1 2 3 4)))
    (test-equal "retried chunk"
                '(conflict . 4)
                (upload-append! tmp id 0 (bytevector 1 2 3 4)))
    (test-equal '(too-large . 10)
                (upload-append! tmp id 4 (make-bytevector 7 0)))
    (test-equal 10 (upload-append! tmp id 4 (make-bytevector 6 9)))
    (test-equal 10 (file-size (upload-part-path tmp id)))
    (test-equal "alice" (alist-ref (upload-ref tmp id) 'owner))
    (test-equal #f (upload-ref tmp "../../x"))
    (test-equal '() (purge-stale-uploads! tmp 3600))
    (test-equal (list id) (purge-stale-uploads! tmp -1))
    (test-equal #f (upload-ref tmp id))))

(define users-file (string-append tmp "/users.scm"))

(test-group "users"
  (save-user! users-file "vera" "h1" 'viewer)
  (save-user! users-file "ed" "h2" 'editor)
  (save-user! users-file "ada" "h3" 'admin)
  (test-equal 3 (length (load-users users-file)))
  (test-equal 'editor (user-role (find-user users-file "ed")))
  (test-assert (has-role? (find-user users-file "ada") 'editor))
  (test-assert (not (has-role? (find-user users-file "vera") 'editor)))
  (test-assert (not (has-role? #f 'viewer)))
  (update-user! users-file "ed" 'disabled #t)
  (test-assert "disabled users have no rights"
               (not (has-role? (find-user users-file "ed") 'viewer)))
  (delete-user! users-file "ed")
  (test-equal #f (find-user users-file "ed"))
  (test-assert (valid-username? "a.b_c-1"))
  (test-assert (not (valid-username? "a b"))))

(test-group "access"
  (define (video vis status . pass)
    `((id . "abcdefghijk")
      (visibility . ,vis)
      (status . ,status)
      (passphrase-hash . ,(if (pair? pass) (car pass) #f))))
  (define viewer (find-user users-file "vera"))
  (define admin (find-user users-file "ada"))
  (define editor '((name . "e") (role . editor) (disabled . #f)))
  ;; (who visibility unlocked?) -> expected
  (for-each (lambda (row)
              (let ((who (car row))
                    (vis (cadr row))
                    (unl (caddr row))
                    (want (cadddr row)))
                (test-equal (list who vis unl)
                            want
                            (can-view? (case who
                                         ((anon) #f)
                                         ((viewer) viewer)
                                         ((editor) editor)
                                         ((admin) admin))
                                       (video vis 'ready)
                                       unl))))
            '((anon public #f #t)
              (anon restricted #f #f)
              (anon restricted #t #t)
              (anon draft #f #f)
              (anon draft #t #f)
              (viewer public #f #t)
              (viewer restricted #f #t)
              (viewer draft #f #f)
              (editor public #f #t)
              (editor restricted #f #t)
              (editor draft #f #t)
              (admin draft #f #t)))
  (test-assert "not ready: editors only"
               (not (can-view? viewer (video 'public 'processing) #f)))
  (test-assert (can-view? editor (video 'public 'failed) #f))
  (test-assert (needs-unlock? #f (video 'restricted 'ready "pbkdf2$x")))
  (test-assert "no passphrase, nothing to unlock"
               (not (needs-unlock? #f (video 'restricted 'ready))))
  (test-assert (not (needs-unlock? viewer
                                   (video 'restricted 'ready "pbkdf2$x")))))

(define (filter-warnings ws prefix)
  (let loop ((ws ws))
    (cond ((null? ws) '())
          ((string-contains-from (car ws) prefix 0)
           (cons (car ws) (loop (cdr ws))))
          (else (loop (cdr ws))))))

(test-group "theme"
  (let ((s (load-settings tmp "Technik")))
    (test-equal "initial name from the config" "Technik"
                (alist-ref s 'site-name))
    (test-equal 'auto (alist-ref s 'default-mode))
    (test-equal '() (contrast-warnings s))
    (save-settings! tmp
                    (alist-set s
                               'light
                               (alist-set (alist-ref s 'light)
                                          'fg
                                          "#fefefe")))
    (let ((s2 (load-settings tmp "Technik")))
      (test-equal "#fefefe" (alist-ref (alist-ref s2 'light) 'fg))
      (test-equal 1
                  (length (filter-warnings (contrast-warnings s2)
                                           "Hell: Text hat"))))
    (test-assert (string-contains-from (theme-css s)
                                       "--accent: light-dark(#2563eb, #6ea8fe);"
                                       0)))
  (test-assert "partial file falls back to defaults"
               (begin
                 (write-datum! (string-append tmp "/site/settings.scm")
                               '((light (bg . "bad"))))
                 (equal? "#ffffff"
                         (alist-ref (alist-ref (load-settings tmp) 'light)
                                    'bg))))
  (test-assert (valid-color? "#a0B1c2"))
  (test-assert (not (valid-color? "red")))
  (test-assert (> (contrast-ratio "#000000" "#ffffff") 20.9)))

(rm tmp 'recursive 'force)
(test-end "filme store")
