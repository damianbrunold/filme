;; (app videos) — the video library: metadata files and the in-memory index.
;;
;; Each video lives in data-dir/videos/<id>/ with meta.scm (one alist, see
;; docs/DATA-FORMAT.md), video.mp4 and the thumbnails. At startup every
;; meta.scm is read into a hash table; every change writes the file first
;; and then updates the table. Listing and search run on the table, which
;; is cheap for hundreds of videos.

(define-library (app videos)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 18)
          (srfi 69)
          (srfi 132)
          (scm fs)
          (scm string)
          (app store))
  (export all-tags
          library-data-dir
          library-load!
          library-refresh!
          library-touch!
          make-library
          normalize-tags
          search-videos
          video-create!
          video-dir
          video-file
          video-list
          video-purge-trash!
          video-ref
          video-save!
          video-trash!
          video-update!)
  (begin

    (define-record-type library
      (%make-library data-dir table mutex stamp)
      library?
      (data-dir library-data-dir)
      (table library-table)
      (mutex library-mutex)
      (stamp library-stamp set-library-stamp!))

    (define (make-library data-dir)
      "Syntax: (make-library data-dir)
Library: (app videos)
Description: A library over data-dir/videos, loaded from disk."
      (let
          ((lib (%make-library data-dir
                               (make-hash-table equal?)
                               (make-mutex)
                               #f)))
        (ensure-dir (string-append data-dir "/videos"))
        (library-load! lib)
        lib))

    (define (locked lib thunk)
      (dynamic-wind
        (lambda () (mutex-lock! (library-mutex lib)))
        thunk
        (lambda () (mutex-unlock! (library-mutex lib)))))

    (define (videos-root lib)
      (string-append (library-data-dir lib) "/videos"))

    (define (video-dir lib id)
      "Syntax: (video-dir lib id)
Library: (app videos)
Description: The directory of video id."
      (string-append (videos-root lib) "/" id))

    (define (video-file lib id name)
      "Syntax: (video-file lib id name)
Library: (app videos)
Description: The path of file name (\"video.mp4\", \"thumb.jpg\") of video id."
      (string-append (video-dir lib id) "/" name))

    (define (meta-path lib id) (video-file lib id "meta.scm"))

    (define (library-load! lib)
      "Syntax: (library-load! lib)
Library: (app videos)
Description: (Re)reads every meta.scm into the index. Directories without
  a readable meta.scm are skipped."
      (locked lib
              (lambda ()
                (set-library-stamp! lib (dir-stamp lib))
                (for-each (lambda (k)
                            (hash-table-delete! (library-table lib) k))
                          (hash-table-keys (library-table lib)))
                (for-each (lambda (id)
                            (when (valid-id? id)
                              (let
                                  ((m (guard (e (#t #f))
                                        (read-datum (meta-path lib id) #f))))
                                (when
                                    (and (list? m)
                                         (equal? (alist-ref m 'id) id))
                                  (hash-table-set! (library-table lib)
                                                   id
                                                   m)))))
                          (directory-directories (videos-root lib))))))

    (define (dir-stamp lib) (file-modification-timestamp (videos-root lib)))

    (define (library-refresh! lib)
      "Syntax: (library-refresh! lib)
Library: (app videos)
Description: Reloads the index if videos/ changed on disk since it was
  loaded (a video added or removed by another process, or library-touch!)."
      (unless (equal? (dir-stamp lib) (library-stamp lib))
        (library-load! lib)))

    (define (library-touch! lib)
      "Syntax: (library-touch! lib)
Library: (app videos)
Description: Marks videos/ as changed, so a running server reloads its
  index on the next request. Call after changing meta files from a script."
      (write-datum! (string-append (videos-root lib) "/.changed")
                    (now-seconds)))

    (define (video-ref lib id)
      "Syntax: (video-ref lib id)
Library: (app videos)
Description: The metadata alist of video id, or #f."
      (and (string? id)
           (locked lib
                   (lambda ()
                     (hash-table-ref/default (library-table lib) id #f)))))

    (define (video-list lib)
      "Syntax: (video-list lib)
Library: (app videos)
Description: All metadata alists, newest first."
      (list-sort (lambda (a b)
                   (> (alist-ref a 'created 0) (alist-ref b 'created 0)))
                 (locked lib
                         (lambda ()
                           (hash-table-values (library-table lib))))))

    (define (video-save! lib meta)
      "Syntax: (video-save! lib meta)
Library: (app videos)
Description: Writes meta (which carries its id) to disk and the index."
      (let ((id (alist-ref meta 'id)))
        (unless (valid-id? id) (error "video-save!: bad id" id))
        (write-datum! (meta-path lib id) meta)
        (locked lib (lambda () (hash-table-set! (library-table lib) id meta)))
        meta))

    (define (video-update! lib id proc)
      "Syntax: (video-update! lib id proc)
Library: (app videos)
Description: Replaces the metadata of id with (proc meta), stamping
  updated. Returns the new metadata, or #f if there is no such video."
      (let ((m (video-ref lib id)))
        (and m
             (video-save! lib (alist-set (proc m) 'updated (now-seconds))))))

    (define (video-create! lib fields)
      "Syntax: (video-create! lib fields)
Library: (app videos)
Description: Makes a new video directory with a fresh id; fields (an
  alist) are added to the defaults. Returns the new metadata."
      (let loop ()
        (let ((id (new-id)))
          (if (or (video-ref lib id) (file-exists? (video-dir lib id)))
              (loop)
              (let ((now (now-seconds)))
                (ensure-dir (video-dir lib id))
                (video-save! lib
                             (let fill
                                 ((m `((version . 1)
                                       (id . ,id)
                                       (title . "")
                                       (description . "")
                                       (tags . ())
                                       (visibility . draft)
                                       (passphrase-hash . #f)
                                       (allow-download . #f)
                                       (status . processing)
                                       (error . #f)
                                       (warning . #f)
                                       (original-filename . "")
                                       (size . 0)
                                       (duration . 0)
                                       (width . 0)
                                       (height . 0)
                                       (video-codec . "")
                                       (audio-codec . "")
                                       (thumbnail-at . #f)
                                       (created . ,now)
                                       (updated . ,now)
                                       (created-by . #f)
                                       (updated-by . #f)))
                                  (fs fields))
                               (if (null? fs)
                                   m
                                   (fill (alist-set m (caar fs) (cdar fs))
                                         (cdr fs))))))))))

    (define (trash-root lib) (string-append (library-data-dir lib) "/trash"))

    (define (video-trash! lib id)
      "Syntax: (video-trash! lib id)
Library: (app videos)
Description: Moves video id to data-dir/trash/<id> and drops it from the
  index. Returns #t on success."
      (and (video-ref lib id)
           (begin
             (ensure-dir (trash-root lib))
             (let ((target (string-append (trash-root lib) "/" id)))
               (when (file-exists? target) (rm target 'recursive 'force))
               (and (move-directory (video-dir lib id) target)
                    (begin
                      ;; The trash time decides when maintenance purges it.
                      (write-datum! (string-append target "/trashed.scm")
                                    (now-seconds))
                      (locked lib
                              (lambda ()
                                (hash-table-delete! (library-table lib) id)))
                      #t))))))

    (define (video-purge-trash! lib max-age-seconds)
      "Syntax: (video-purge-trash! lib max-age-seconds)
Library: (app videos)
Description: Deletes trashed videos older than max-age-seconds. Returns
  the list of purged ids."
      (let ((root (trash-root lib))
            (now (now-seconds)))
        (if (not (directory-exists? root))
            '()
            (let loop ((ds (directory-directories root))
                       (acc '()))
              (if (null? ds)
                  (reverse acc)
                  (let* ((dir (string-append root "/" (car ds)))
                         (t (read-datum (string-append dir "/trashed.scm")
                                        0)))
                    (if (and (number? t) (> (- now t) max-age-seconds))
                        (begin
                          (rm dir 'recursive 'force)
                          (loop (cdr ds) (cons (car ds) acc)))
                        (loop (cdr ds) acc))))))))

    ;; --- tags and search ---------------------------------------------

    (define (trim s)
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

    (define (normalize-tags s)
      "Syntax: (normalize-tags s)
Library: (app videos)
Description: The tags in s (separated by commas), trimmed, lower-cased,
  without empty or duplicate entries."
      (let loop ((parts (string-split-char s #\,))
                 (acc '()))
        (if (null? parts)
            (reverse acc)
            (let ((t (string-downcase (trim (car parts)))))
              (if (or (string=? t "") (member t acc) (> (string-length t) 40))
                  (loop (cdr parts) acc)
                  (loop (cdr parts) (cons t acc)))))))

    (define (all-tags metas)
      "Syntax: (all-tags metas)
Library: (app videos)
Description: The sorted list of distinct tags used in metas."
      (list-sort string<?
                 (let loop ((ms metas) (acc '()))
                   (if (null? ms)
                       acc
                       (loop (cdr ms)
                             (let add ((ts (alist-ref (car ms) 'tags '()))
                                       (acc acc))
                               (cond ((null? ts) acc)
                                     ((member (car ts) acc)
                                      (add (cdr ts) acc))
                                     (else
                                      (add (cdr ts)
                                           (cons (car ts) acc))))))))))

    (define (words s)
      (let loop ((cs (string->list (string-downcase s)))
                 (cur '())
                 (acc '()))
        (define (flush)
          (if (null? cur) acc (cons (list->string (reverse cur)) acc)))
        (cond ((null? cs) (reverse (flush)))
              ((char-whitespace? (car cs)) (loop (cdr cs) '() (flush)))
              (else (loop (cdr cs) (cons (car cs) cur) acc)))))

    (define (contains? hay needle)
      (and (string-contains-from hay needle 0) #t))

    (define (matches? meta ws)
      (let
          ((text (string-downcase (string-append
                                   (alist-ref meta 'title "")
                                   "\n"
                                   (alist-ref meta 'description "")
                                   "\n"
                                   (apply
                                    string-append
                                    (map (lambda (t) (string-append t " "))
                                         (alist-ref meta 'tags '())))))))
        (let loop ((ws ws))
          (or (null? ws) (and (contains? text (car ws)) (loop (cdr ws)))))))

    (define (search-videos metas query tag sort)
      "Syntax: (search-videos metas query tag sort)
Library: (app videos)
Description: The metas whose title, description or tags contain every
  word of query (case-insensitive), that carry tag (unless #f), ordered
  by sort: 'newest, 'oldest or 'title."
      (let* ((ws (words (or query "")))
             (hits (let loop ((ms metas))
                     (cond ((null? ms) '())
                           ((and (matches? (car ms) ws)
                                 (or (not tag)
                                     (member tag
                                             (alist-ref (car ms) 'tags '()))))
                            (cons (car ms) (loop (cdr ms))))
                           (else (loop (cdr ms)))))))
        (list-sort (case sort
                     ((title)
                      (lambda (a b)
                        (string<? (string-downcase (alist-ref a 'title ""))
                                  (string-downcase (alist-ref b 'title "")))))
                     ((oldest)
                      (lambda (a b)
                        (< (alist-ref a 'created 0)
                           (alist-ref b 'created 0))))
                     (else (lambda (a b)
                             (> (alist-ref a 'created 0)
                                (alist-ref b 'created 0)))))
                   hits)))))
