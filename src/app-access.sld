;; (app access) — who may see which video.
;;
;;                  public   restricted               draft
;;   anonymous      yes      with the passphrase      no
;;   viewer         yes      yes                      no
;;   editor/admin   yes      yes                      yes
;;
;; Videos that are not ready yet (processing, failed) are editor-only
;; whatever their visibility. A passphrase unlock is a signed cookie
;; filme_v_<id> whose datum carries a stamp of the passphrase hash: setting
;; a new passphrase (or removing it) invalidates every earlier unlock.

(define-library (app access)
  (import (scheme base)
          (scm crypto)
          (scm crypto token)
          (scm net http cookies)
          (scm net http request)
          (app store)
          (app users))
  (export can-edit?
          can-view?
          needs-unlock?
          unlock-cookie
          unlocked?
          visibility-names)
  (begin

    (define visibility-names '(public restricted draft))

    (define (can-edit? user)
      "Syntax: (can-edit? user)
Library: (app access)
Description: True if user may upload, edit and delete videos."
      (has-role? user 'editor))

    (define (stamp hash)
      (base64-encode (bytevector-copy (sha256-hash (string->utf8 hash)) 0 9)))

    (define (cookie-name id) (string-append "filme_v_" id))

    (define (unlocked? secret req meta)
      "Syntax: (unlocked? secret req meta)
Library: (app access)
Description: True if req carries a valid unlock cookie for meta's current
  passphrase."
      (let ((hash (alist-ref meta 'passphrase-hash)))
        (and (string? hash)
             (let* ((id (alist-ref meta 'id))
                    (tok (cookie-ref (parse-cookie-header
                                      (http-request-header req "Cookie"))
                                     (cookie-name id)))
                    (d (and tok
                            (token-verify secret tok
                                          '((purpose . "unlock"))))))
               (and (pair? d)
                    (equal? (alist-ref d 'id) id)
                    (equal? (alist-ref d 'stamp) (stamp hash)))))))

    (define (unlock-cookie secret meta max-age path secure?)
      "Syntax: (unlock-cookie secret meta max-age path secure?)
Library: (app access)
Description: The Set-Cookie header pair that unlocks meta's video."
      (let ((id (alist-ref meta 'id)))
        (cons "Set-Cookie"
              (apply format-set-cookie
                     (cookie-name id)
                     (token-sign secret
                                 `((id . ,id)
                                   (stamp
                                    .
                                    ,(stamp
                                      (alist-ref meta 'passphrase-hash))))
                                 max-age
                                 '((purpose . "unlock")))
                     max-age
                     path
                     (if secure? '(lax) '(lax no-secure))))))

    (define (can-view? user meta unlocked)
      "Syntax: (can-view? user meta unlocked)
Library: (app access)
Description: True if user (or #f) may watch meta's video; unlocked tells
  whether the request carries a valid passphrase unlock."
      (cond ((can-edit? user) #t)
            ((not (eq? (alist-ref meta 'status) 'ready)) #f)
            (else (case (alist-ref meta 'visibility)
                    ((public) #t)
                    ((restricted)
                     (or (has-role? user 'viewer) (and unlocked #t)))
                    (else #f)))))

    (define (needs-unlock? user meta)
      "Syntax: (needs-unlock? user meta)
Library: (app access)
Description: True if meta is a ready, restricted video with a passphrase
  that user cannot watch without entering it."
      (and (not (has-role? user 'viewer))
           (eq? (alist-ref meta 'status) 'ready)
           (eq? (alist-ref meta 'visibility) 'restricted)
           (string? (alist-ref meta 'passphrase-hash))))))
