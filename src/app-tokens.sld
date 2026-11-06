;; (app tokens) — personal API tokens, for programs that upload on a
;; user's behalf (the aufnahme recorder's "Publish to filme").
;;
;; A token looks like  fltk_<id>_<secret>  : id names the token (11
;; characters, see new-id), secret is 32 random bytes in base64url. Only a
;; SHA-256 of the secret is stored, in the user's entry in users.scm:
;;   (tokens ((id . "...") (label . "Aufnahme") (hash . "<hex>")
;;            (created . 1793976000)) ...)
;; The secret is random and long, so a plain hash is enough (no PBKDF2).
;; A token acts as its user, but (app web) accepts it only on the upload
;; and video API routes, never for pages, settings or user admin. Disabling
;; or deleting the user disables the token too.

(define-library (app tokens)
  (import (scheme base)
          (scheme char)
          (scm crypto)
          (app store)
          (app users))
  (export bearer-token
          create-token!
          revoke-token!
          token-user
          user-tokens)
  (begin

    (define prefix "fltk_")

    (define (user-tokens u)
      "Syntax: (user-tokens user)
Library: (app tokens)
Description: The token entries (id, label, hash, created) of user."
      (let ((ts (alist-ref u 'tokens '()))) (if (list? ts) ts '())))

    (define (hex bv)
      (let loop ((i 0) (acc '()))
        (if (= i (bytevector-length bv))
            (apply string-append (reverse acc))
            (let ((b (bytevector-u8-ref bv i)))
              (loop (+ i 1)
                    (cons (string-append (if (< b 16) "0" "")
                                         (number->string b 16))
                          acc))))))

    (define (secret-hash secret) (hex (sha256-hash (string->utf8 secret))))

    (define (base64url bv)
      (list->string (let loop ((cs (string->list (base64-encode bv))))
                      (cond ((null? cs) '())
                            ((char=? (car cs) #\=) '())
                            ((char=? (car cs) #\+) (cons #\- (loop (cdr cs))))
                            ((char=? (car cs) #\/) (cons #\_ (loop (cdr cs))))
                            (else (cons (car cs) (loop (cdr cs))))))))

    (define (create-token! users-file name label)
      "Syntax: (create-token! users-file name label)
Library: (app tokens)
Description: Adds a token labelled label to user name and returns the
  token string. It is not stored and cannot be shown again."
      (let* ((u (find-user users-file name))
             (id (new-id))
             (secret (base64url (random-bytes 32)))
             (entry `((id . ,id)
                      (label . ,label)
                      (hash . ,(secret-hash secret))
                      (created . ,(now-seconds)))))
        (unless u (error "no such user" name))
        (update-user! users-file name
                      'tokens
                      (append (user-tokens u) (list entry)))
        (string-append prefix id "_" secret)))

    (define (revoke-token! users-file name id)
      "Syntax: (revoke-token! users-file name id)
Library: (app tokens)
Description: Removes token id from user name."
      (let ((u (find-user users-file name)))
        (when u
          (update-user! users-file name
                        'tokens
                        (let loop ((ts (user-tokens u)))
                          (cond ((null? ts) '())
                                ((equal? (alist-ref (car ts) 'id) id)
                                 (cdr ts))
                                (else (cons (car ts) (loop (cdr ts))))))))))

    ;; "fltk_<11-char id>_<secret>" -> (id . secret), or #f
    (define (parse s)
      (let ((n (string-length prefix)))
        (and (string? s)
             (> (string-length s) (+ n 12))
             (string=? (substring s 0 n) prefix)
             (char=? (string-ref s (+ n 11)) #\_)
             (cons (substring s n (+ n 11))
                   (substring s (+ n 12) (string-length s))))))

    (define (constant-time-string=? a b)
      (and (= (string-length a) (string-length b))
           (let loop ((i 0) (diff 0))
             (if (= i (string-length a))
                 (= diff 0)
                 (loop (+ i 1)
                       (if (char=? (string-ref a i) (string-ref b i))
                           diff
                           (+ diff 1)))))))

    (define (token-user users-file token)
      "Syntax: (token-user users-file token)
Library: (app tokens)
Description: The (enabled) user that token belongs to, or #f."
      (let ((p (parse token)))
        (and p
             (let ((h (secret-hash (cdr p))))
               (let loop ((us (load-users users-file)))
                 (cond ((null? us) #f)
                       ((let find ((ts (user-tokens (car us))))
                          (cond ((null? ts) #f)
                                ((equal? (alist-ref (car ts) 'id) (car p))
                                 (constant-time-string=?
                                  (alist-ref (car ts) 'hash "")
                                  h))
                                (else (find (cdr ts)))))
                        (and (not (user-disabled? (car us))) (car us)))
                       (else (loop (cdr us)))))))))

    (define (bearer-token header)
      "Syntax: (bearer-token authorization-header)
Library: (app tokens)
Description: The token in an \"Authorization: Bearer <token>\" header
  value, or #f."
      (and (string? header)
           (> (string-length header) 7)
           (string=? (string-downcase (substring header 0 7)) "bearer ")
           (substring header 7 (string-length header))))))
