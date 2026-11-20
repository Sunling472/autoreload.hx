;; autoreload.scm -- reload open files when they change on disk.
;;
;; Polls the mtime of every open, file-backed document and reloads it when the
;; file changed externally. Deliberately conservative:
;;
;;   * never touches a buffer with unsaved edits (it warns instead);
;;   * ignores our own saves (the document-saved hook adopts the new mtime);
;;   * compares the file contents with the buffer and skips the reload when
;;     they are identical (touch, git checkout of the same bytes, ...);
;;   * waits for normal mode before reloading the focused buffer;
;;   * one timer chain, coalesced per tick.
;;
;; Config:
;;   (autoreload-config #:interval-ms 700)   ; default 700
;;   (autoreload-config #:enabled #f)
;; Commands:
;;   :autoreload-toggle / :autoreload-enable / :autoreload-disable
;;
;; Notes: this is polling, not inotify. Latency equals the interval, and each
;; tick runs a tiny bit of steel (a stat per open file).

(require-builtin steel/filesystem)

(require "helix/editor.scm")
(require "helix/misc.scm")

(require-builtin helix/core/text as text.)

(provide autoreload-enable
         autoreload-disable
         autoreload-toggle
         autoreload-config)

;; ---------------------------------------------------------------------------
;; state
;; ---------------------------------------------------------------------------

(define *ar-config*
  (hash 'enabled #t
        'interval-ms 700))

;; path -> SystemTime of the last version we know about
(define *ar-mtimes* (hash))
;; path -> #t, set after our own save so the next change is adopted silently
(define *ar-skip* (hash))
;; path -> SystemTime we already warned about, to avoid status spam
(define *ar-warned* (hash))
;; a tick chain is scheduled
(define *ar-timer?* #f)

;; ---------------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------------

(define (ar-get h k)
  (if (hash-contains? h k) (hash-ref h k) #f))

(define (ar-path doc)
  (with-handler (lambda (_) #f) (editor-document->path doc)))

(define (ar-mtime path)
  (with-handler
    (lambda (_) #f)
    (define md (file-metadata path))
    (if md (fs-metadata-modified md) #f)))

(define (ar-remember! path m)
  (set! *ar-mtimes* (hash-insert *ar-mtimes* path m)))

(define (ar-unsuppress! path)
  (when (hash-contains? *ar-skip* path)
    (set! *ar-skip* (hash-remove *ar-skip* path))))

(define (ar-same-as-disk? doc path)
  ;; cheap enough for the rare case where the mtime moved: compare the buffer
  ;; with the file so touch/no-op writes do not trigger a reload
  (with-handler
    (lambda (_) #f)
    (let* ([p (open-input-file path)]
           [on-disk (read-port-to-string p)])
      (close-input-port p)
      (equal? (text.rope->string (editor->text doc)) on-disk))))

(define (ar-focused-doc)
  (with-handler (lambda (_) #f) (editor->doc-id (editor-focus))))

(define (ar-editing?)
  (with-handler
    (lambda (_) #f)
    (equal? (editor-mode) (string->editor-mode "insert"))))

(define (ar-warn-once! path m)
  (define last (ar-get *ar-warned* path))
  (unless (and last (equal? last m))
    (set! *ar-warned* (hash-insert *ar-warned* path m))
    (set-warning! (string-append "autoreload: " (file-name path)
                                 " changed on disk (buffer has unsaved changes)"))))

(define (ar-reload! doc path m)
  (with-handler
    (lambda (e)
      (ar-remember! path m)
      (set-error! (string-append "autoreload: failed to reload " (file-name path)
                                 ": " (to-string e))))
    (begin
      (editor-document-reload doc)
      (ar-remember! path m)
      (ar-unsuppress! path)
      (set-status! (string-append "autoreload: reloaded " (file-name path))))))

;; ---------------------------------------------------------------------------
;; the check
;; ---------------------------------------------------------------------------

(define (ar-check-doc! doc)
  (define path (ar-path doc))
  (when (and path (string? path) (is-file? path))
    (define m (ar-mtime path))
    (when m
      (define prev (ar-get *ar-mtimes* path))
      (cond
        ;; first time we see this document: just remember the mtime
        [(not prev) (ar-remember! path m)]

        ;; nothing changed
        [(equal? prev m) (ar-unsuppress! path)]

        ;; our own save just landed: adopt the mtime without reloading
        [(hash-contains? *ar-skip* path)
         (ar-remember! path m)
         (set! *ar-skip* (hash-remove *ar-skip* path))]

        ;; mtime moved but the bytes are the same: nothing to do
        [(ar-same-as-disk? doc path) (ar-remember! path m)]

        ;; unsaved edits: never clobber them
        [(editor-document-dirty? doc) (ar-warn-once! path m)]

        ;; wait until the user leaves insert mode before touching the buffer
        [(and (equal? doc (ar-focused-doc)) (ar-editing?)) #f]

        [else (ar-reload! doc path m)]))))

(define (ar-scan!)
  (for-each ar-check-doc! (editor-all-documents)))

;; ---------------------------------------------------------------------------
;; timer
;; ---------------------------------------------------------------------------

(define (ar-tick)
  (when (hash-ref *ar-config* 'enabled)
    (with-handler (lambda (_) #f) (ar-scan!)))
  (enqueue-thread-local-callback-with-delay
   (hash-ref *ar-config* 'interval-ms)
   ar-tick))

(define (ar-start!)
  (unless *ar-timer?*
    (set! *ar-timer?* #t)
    (enqueue-thread-local-callback-with-delay 300 ar-tick)))

;; ---------------------------------------------------------------------------
;; hooks and public api
;; ---------------------------------------------------------------------------

;; our own writes must not bounce back as external changes
(register-hook
 'document-saved
 (lambda (doc)
   (define path (ar-path doc))
   (when (and path (string? path))
     (set! *ar-skip* (hash-insert *ar-skip* path #t)))))

(define (autoreload-enable)
  (set! *ar-config* (hash-insert *ar-config* 'enabled #t))
  (ar-start!)
  (set-status! "autoreload: on"))

(define (autoreload-disable)
  (set! *ar-config* (hash-insert *ar-config* 'enabled #f))
  (set-status! "autoreload: off"))

(define (autoreload-toggle)
  (if (hash-ref *ar-config* 'enabled)
      (autoreload-disable)
      (autoreload-enable)))

;;@doc
;; Configure autoreload (polling interval in milliseconds, on/off)
(define (autoreload-config #:interval-ms [interval-ms #f]
                           #:enabled [enabled #f])
  (when interval-ms
    (set! *ar-config* (hash-insert *ar-config* 'interval-ms (max 100 interval-ms))))
  (when enabled
    (set! *ar-config* (hash-insert *ar-config* 'enabled enabled)))
  (when (hash-ref *ar-config* 'enabled) (ar-start!))
  (set-status! (string-append "autoreload: "
                              (if (hash-ref *ar-config* 'enabled) "on" "off")
                              ", every "
                              (number->string (hash-ref *ar-config* 'interval-ms))
                              " ms")))

(ar-start!)
