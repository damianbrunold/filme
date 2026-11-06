;; (app uploads) — chunked, resumable uploads.
;;
;; An upload is a directory data-dir/uploads/<id>/ with upload.scm (file
;; name, expected size, owner, target video or #f, created) and data.part.
;; The browser sends the file in chunks: PUT ...?offset=N appends a chunk
;; only when N is the current size of data.part, so a retried chunk is
;; harmless and an out-of-order one is refused with the offset to resume
;; from. Unfinished uploads are purged by maintenance after a day.

(define-library (app uploads)
  (import (scheme base)
          (scheme file)
          (srfi 18)
          (scm fs)
          (scm random access)
          (app store))
  (export purge-stale-uploads!
          upload-append!
          upload-cancel!
          upload-offset
          upload-part-path
          upload-ref
          upload-start!)
  (begin

    (define (uploads-root data-dir) (string-append data-dir "/uploads"))
    (define (upload-dir data-dir id)
      (string-append (uploads-root data-dir) "/" id))

    (define (upload-part-path data-dir id)
      "Syntax: (upload-part-path data-dir id)
Library: (app uploads)
Description: The path of upload id's data so far."
      (string-append (upload-dir data-dir id) "/data.part"))

    (define (upload-start! data-dir owner filename size target)
      "Syntax: (upload-start! data-dir owner filename size target)
Library: (app uploads)
Description: Begins an upload of size bytes for owner (a username);
  target is the id of a video whose file it replaces, or #f. Returns the
  upload's alist (with id)."
      (let* ((id (new-id))
             (dir (upload-dir data-dir id))
             (u `((id . ,id)
                  (owner . ,owner)
                  (filename . ,filename)
                  (size . ,size)
                  (target . ,target)
                  (created . ,(now-seconds)))))
        (ensure-dir dir)
        (call-with-random-access-file (upload-part-path data-dir id)
          'write
          (lambda (f) #t))
        (write-datum! (string-append dir "/upload.scm") u)
        u))

    (define (upload-ref data-dir id)
      "Syntax: (upload-ref data-dir id)
Library: (app uploads)
Description: Upload id's alist, or #f."
      (and (valid-id? id)
           (let
               ((u (read-datum (string-append (upload-dir data-dir id)
                                              "/upload.scm")
                               #f)))
             (and (list? u) u))))

    (define (upload-offset data-dir id)
      "Syntax: (upload-offset data-dir id)
Library: (app uploads)
Description: The number of bytes received so far."
      (let ((p (upload-part-path data-dir id)))
        (if (file-exists? p) (file-size p) 0)))

    (define append-mutex (make-mutex))

    (define (upload-append! data-dir id offset bytes)
      "Syntax: (upload-append! data-dir id offset bytes)
Library: (app uploads)
Description: Appends bytes at offset. Returns the new offset, or
  (conflict . current-offset) when offset is not where the data ends, or
  (too-large . size) when the chunk would exceed the announced size."
      (dynamic-wind
        (lambda () (mutex-lock! append-mutex))
        (lambda ()
          (let ((u (upload-ref data-dir id))
                (cur (upload-offset data-dir id)))
            (cond ((not (= offset cur)) (cons 'conflict cur))
                  ((> (+ cur (bytevector-length bytes)) (alist-ref u 'size 0))
                   (cons 'too-large (alist-ref u 'size 0)))
                  (else (call-with-random-access-file
                            (upload-part-path data-dir id)
                          'update
                          (lambda (f)
                            (random-access-file-write! f cur bytes)))
                        (+ cur (bytevector-length bytes))))))
        (lambda () (mutex-unlock! append-mutex))))

    (define (upload-cancel! data-dir id)
      "Syntax: (upload-cancel! data-dir id)
Library: (app uploads)
Description: Deletes upload id and its data."
      (when (valid-id? id) (rm (upload-dir data-dir id) 'recursive 'force)))

    (define (purge-stale-uploads! data-dir max-age-seconds)
      "Syntax: (purge-stale-uploads! data-dir max-age-seconds)
Library: (app uploads)
Description: Deletes uploads started more than max-age-seconds ago.
  Returns the purged ids."
      (let ((root (uploads-root data-dir))
            (now (now-seconds)))
        (if (not (directory-exists? root))
            '()
            (let loop ((ids (directory-directories root))
                       (acc '()))
              (cond ((null? ids) (reverse acc))
                    ((let ((u (upload-ref data-dir (car ids))))
                       (or (not u)
                           (> (- now (alist-ref u 'created 0))
                              max-age-seconds)))
                     (upload-cancel! data-dir (car ids))
                     (loop (cdr ids) (cons (car ids) acc)))
                    (else (loop (cdr ids) acc)))))))))
