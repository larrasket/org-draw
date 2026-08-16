;;; org-draw --- Seamless iPad drawing into org-mode  -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Saleh

;; Author: Saleh <root@lr0.org>
;; Maintainer: Saleh <root@lr0.org>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: org, multimedia, hypermedia
;; URL: https://github.com/larrasket/org-draw

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Draw on an iPad (or any device) in a browser canvas and have it land as a
;; self-contained, re-editable PNG inside your org-mode document.  Emacs runs a
;; pure-Elisp HTTP server; a browser tab at /canvas long-polls it and posts the
;; figure back.  See the README for the full protocol and setup.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'json)
(require 'transient)

(defgroup org-draw nil
  "Seamless iPad drawing into Org mode."
  :group 'org
  :prefix "org-draw-")

(defcustom org-draw-port 8777
  "TCP port the org-draw HTTP server listens on."
  :type 'integer :group 'org-draw)

(defcustom org-draw-token-file
  (expand-file-name "org-draw-tokens" user-emacs-directory)
  "File where paired-device tokens are persisted, one token per line."
  :type 'file :group 'org-draw)

(defcustom org-draw-require-pairing nil
  "Whether a browser must pair with a 6-digit code before it can draw.

Default nil: no code is required.  Any browser that opens the /canvas URL can
receive draw requests and post figures back.  This is the convenient default
for a trusted home/office LAN.

Set to non-nil to require pairing: the canvas shows a code-entry screen and the
server rejects unauthenticated /session, /result and /cancel requests (401).
Turn this on if other people share your network and you don't want them able to
reach the server."
  :type 'boolean :group 'org-draw)

(defcustom org-draw-copy-url t
  "When non-nil, copy the receiver URL to the kill-ring on `org-draw-setup' and
on the first `org-draw'/`org-draw-edit' of a session.  Set to nil to never touch
the kill-ring; the URL is still echoed and listed in the setup buffer."
  :type 'boolean :group 'org-draw)

(defcustom org-draw-directory "figures"
  "Directory for new figures, resolved relative to the visited org file."
  :type 'string :group 'org-draw)

(defcustom org-draw-file-name-function #'org-draw-default-file-name
  "Function returning the base file name (no directory) for a new figure."
  :type 'function :group 'org-draw)

(defcustom org-draw-insert-attr-width nil
  "When non-nil, insert `#+ATTR_ORG: :width N' above a new figure's link.
The value is the pixel width N."
  :type '(choice (const :tag "Off" nil) integer) :group 'org-draw)

(defconst org-draw--max-body (* 50 1024 1024)
  "Maximum accepted request body size in bytes (50 MB).  Larger => 413.")

(defconst org-draw--longpoll-seconds 55
  "Seconds a long-poll connection may be parked before a 204 is sent.")

;;;; PNG format

(defconst org-draw--png-signature
  (unibyte-string #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)
  "The 8-byte PNG file signature.")

(defconst org-draw--crc32-table
  (let ((table (make-vector 256 0)))
    (dotimes (n 256)
      (let ((c n))
        (dotimes (_ 8)
          (if (= (logand c 1) 1)
              (setq c (logxor #xEDB88320 (ash c -1)))
            (setq c (ash c -1))))
        (aset table n c)))
    table)
  "Precomputed CRC32 lookup table (polynomial #xEDB88320).")

(defun org-draw--crc32 (bytes)
  "Compute the PNG/zlib CRC32 of unibyte string BYTES.
Return a 32-bit unsigned integer.  Matches Python `zlib.crc32'."
  (let ((crc #xFFFFFFFF) (len (length bytes)))
    (dotimes (i len)
      (setq crc (logxor (aref org-draw--crc32-table
                              (logand (logxor crc (aref bytes i)) #xFF))
                        (ash crc -8))))
    (logxor crc #xFFFFFFFF)))

(defun org-draw--u32-encode (n)
  "Encode integer N as a 4-byte big-endian unibyte string."
  (unibyte-string (logand (ash n -24) #xFF) (logand (ash n -16) #xFF)
                  (logand (ash n -8) #xFF) (logand n #xFF)))

(defun org-draw--u32-decode (bytes offset)
  "Decode 4 big-endian bytes from unibyte BYTES at OFFSET into an integer."
  (logior (ash (aref bytes offset) 24) (ash (aref bytes (+ offset 1)) 16)
          (ash (aref bytes (+ offset 2)) 8) (aref bytes (+ offset 3))))

(defconst org-draw--png-chunk-type "orPd" "Private PNG chunk type holding drawing bytes.")

(defun org-draw--png-chunks (bytes)
  "Walk PNG unibyte string BYTES, returning a list of chunk descriptor plists:
\(:type STR :data-start INT :data-len INT :chunk-start INT :chunk-end INT).
Signal an error on a bad signature or truncation."
  (unless (and (>= (length bytes) 8)
               (string= (substring bytes 0 8) org-draw--png-signature))
    (error "Org-draw: not a PNG (bad signature)"))
  (let ((pos 8) (len (length bytes)) (chunks '()))
    (while (< pos len)
      (when (> (+ pos 8) len) (error "Org-draw: truncated PNG chunk header at %d" pos))
      (let* ((data-len (org-draw--u32-decode bytes pos))
             (type (substring bytes (+ pos 4) (+ pos 8)))
             (data-start (+ pos 8))
             (chunk-end (+ data-start data-len 4)))
        (when (> chunk-end len) (error "Org-draw: truncated PNG chunk %s at %d" type pos))
        (push (list :type type :data-start data-start :data-len data-len
                    :chunk-start pos :chunk-end chunk-end)
              chunks)
        (setq pos chunk-end)))
    (nreverse chunks)))

(defun org-draw--png-make-chunk (type data)
  "Build a serialized PNG chunk from 4-char TYPE and unibyte DATA."
  (let* ((type-data (concat type data)) (crc (org-draw--crc32 type-data)))
    (concat (org-draw--u32-encode (length data)) type-data (org-draw--u32-encode crc))))

;;;; HTTP server

(defvar org-draw--server-process nil "The listening server process, or nil when stopped.")

(defun org-draw--status-text (status)
  "Return the HTTP reason phrase for numeric STATUS."
  (pcase status
    (200 "OK") (204 "No Content") (302 "Found") (400 "Bad Request")
    (401 "Unauthorized") (404 "Not Found") (405 "Method Not Allowed")
    (413 "Payload Too Large") (500 "Internal Server Error") (_ "Status")))

(defun org-draw--safe-delete (proc)
  "Delete PROC if live, ignoring errors."
  (when (process-live-p proc) (ignore-errors (delete-process proc))))

(defun org-draw--respond (proc status content-type body &optional headers keep-alive)
  "Write an HTTP/1.1 response to PROC and (unless KEEP-ALIVE) close it.
STATUS is a number, CONTENT-TYPE a string or nil, BODY a unibyte string or nil,
HEADERS an alist of extra (NAME . VALUE).  Content-Length is always sent."
  (when (process-live-p proc)
    (let* ((body (or body ""))
           (body (if (multibyte-string-p body) (encode-coding-string body 'utf-8) body))
           (parts (list (format "HTTP/1.1 %d %s\r\n" status (org-draw--status-text status)))))
      (when content-type (push (format "Content-Type: %s\r\n" content-type) parts))
      (push (format "Content-Length: %d\r\n" (length body)) parts)
      (push (format "Connection: %s\r\n" (if keep-alive "keep-alive" "close")) parts)
      (dolist (h headers) (push (format "%s: %s\r\n" (car h) (cdr h)) parts))
      (push "\r\n" parts)
      (let ((head (apply #'concat (nreverse parts))))
        (process-send-string proc (concat (string-to-unibyte head) body)))
      (if keep-alive (org-draw--reset-conn-state proc) (org-draw--safe-delete proc)))))

(defvar org-draw--routes nil
  "Alist of ((METHOD . PATH) . HANDLER).  METHOD uppercase string, PATH exact.")

(defun org-draw-route (method path handler)
  "Register HANDLER for METHOD (string) and exact PATH (string)."
  (setf (alist-get (cons (upcase method) path) org-draw--routes nil nil #'equal) handler))

(defun org-draw--parse-request-line (line)
  "Parse HTTP request LINE -> (METHOD PATH QUERY VERSION) or nil."
  (when (string-match
         "\\`\\([A-Za-z]+\\) \\([^ ?]*\\)\\(?:\\?\\([^ ]*\\)\\)? \\(HTTP/[0-9.]+\\)\\'" line)
    (list (upcase (match-string 1 line)) (match-string 2 line)
          (match-string 3 line) (match-string 4 line))))

(defun org-draw--parse-headers (block)
  "Parse header BLOCK (CRLF-separated) -> alist of (LOWERCASE-NAME . VALUE)."
  (let (headers)
    (dolist (line (split-string block "\r\n" t))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line)) headers)))
    (nreverse headers)))

(defun org-draw--header (req name)
  "Value of header NAME (case-insensitive) from REQ, or nil."
  (cdr (assoc (downcase name) (plist-get req :headers))))

(defun org-draw--dispatch (req)
  "Find and call the handler for REQ, or send 404/405/500."
  (let* ((method (plist-get req :method)) (path (plist-get req :path))
         (proc (plist-get req :proc))
         (handler (cdr (assoc (cons method path) org-draw--routes))))
    (cond
     (handler (condition-case err (funcall handler req)
                (error (org-draw--respond proc 500 "text/plain" (format "Internal error: %S" err)))))
     ((cl-some (lambda (r) (equal (cdar r) path)) org-draw--routes)
      (org-draw--respond proc 405 "text/plain" "Method Not Allowed"))
     (t (org-draw--respond proc 404 "text/plain" "Not Found")))))

(defun org-draw--reset-conn-state (proc)
  "Initialise/reset the per-connection parse state on PROC."
  (process-put proc :org-draw-state
               (list :buf "" :phase 'headers :header-end nil :method nil :path nil
                     :query nil :version nil :headers nil :content-length nil)))

(defun org-draw--filter (proc chunk)
  "Process filter: accumulate unibyte CHUNK on PROC and drive parsing."
  (let ((st (process-get proc :org-draw-state)))
    (unless st (org-draw--reset-conn-state proc) (setq st (process-get proc :org-draw-state)))
    (plist-put st :buf (concat (plist-get st :buf) chunk))
    (org-draw--advance proc st)))

(defun org-draw--advance (proc st)
  "Advance the parse state machine for PROC given state ST."
  (pcase (plist-get st :phase)
    ('headers (org-draw--try-parse-headers proc st))
    ('body    (org-draw--try-parse-body proc st))
    ('done    nil)))

(defun org-draw--try-parse-headers (proc st)
  "Parse the header block from ST if complete; advance to body.
PROC is the connection process to respond on."
  (let* ((buf (plist-get st :buf)) (sep (string-search "\r\n\r\n" buf)))
    (when sep
      (let* ((head (substring buf 0 sep)) (nl (string-search "\r\n" head))
             (req-line (if nl (substring head 0 nl) head))
             (hdr-block (if nl (substring head (+ nl 2)) ""))
             (parsed (org-draw--parse-request-line req-line)))
        (if (not parsed)
            (org-draw--respond proc 400 "text/plain" "Bad Request Line")
          (cl-destructuring-bind (method path query version) parsed
            (let* ((headers (org-draw--parse-headers hdr-block))
                   (cl-str (cdr (assoc "content-length" headers)))
                   (clen (and cl-str (string-to-number cl-str))))
              (cond
               ((and cl-str (not (string-match-p "\\`[0-9]+\\'" cl-str)))
                (org-draw--respond proc 400 "text/plain" "Bad Content-Length"))
               ((and clen (> clen org-draw--max-body))
                (org-draw--respond proc 413 "text/plain" "Payload Too Large"))
               (t (plist-put st :method method) (plist-put st :path path)
                  (plist-put st :query query) (plist-put st :version version)
                  (plist-put st :headers headers) (plist-put st :content-length (or clen 0))
                  (plist-put st :header-end (+ sep 4)) (plist-put st :phase 'body)
                  (org-draw--try-parse-body proc st))))))))))

(defun org-draw--try-parse-body (proc st)
  "Collect Content-Length bytes of body and dispatch when complete.
PROC is the connection process and ST its parse state."
  (let* ((buf (plist-get st :buf)) (start (plist-get st :header-end))
         (clen (plist-get st :content-length)) (have (- (length buf) start)))
    (when (>= have clen)
      (let ((req (list :proc proc :method (plist-get st :method) :path (plist-get st :path)
                       :query (plist-get st :query) :version (plist-get st :version)
                       :headers (plist-get st :headers)
                       :body (substring buf start (+ start clen)))))
        (plist-put st :phase 'done)
        (org-draw--dispatch req)))))

;;;; Long-poll: park a connection, answer later; 55s deadline -> 204.

(defun org-draw-park (proc &optional seconds on-timeout)
  "Park connection PROC for a long-poll, arming a deadline timer.
After SECONDS (default `org-draw--longpoll-seconds') with no reply, call
ON-TIMEOUT with PROC, else send 204.  Return the timer."
  (let* ((secs (or seconds org-draw--longpoll-seconds))
         (timer (run-at-time secs nil
                             (lambda ()
                               (process-put proc :org-draw-timer nil)
                               (when (process-live-p proc)
                                 (if on-timeout (funcall on-timeout proc)
                                   (org-draw--respond proc 204 nil nil)))))))
    (process-put proc :org-draw-timer timer)
    (process-put proc :org-draw-parked t)
    timer))

(defun org-draw-answer (proc status content-type body &optional headers)
  "Answer a parked long-poll PROC, disarming its deadline timer.
STATUS, CONTENT-TYPE, BODY and HEADERS are passed through to
`org-draw--respond'."
  (let ((timer (process-get proc :org-draw-timer)))
    (when timer (cancel-timer timer)))
  (process-put proc :org-draw-timer nil)
  (process-put proc :org-draw-parked nil)
  (org-draw--respond proc status content-type body headers))

;; Declared here (ahead of the /session handler that populates it) so the
;; sentinel below compiles clean under `byte-compile-error-on-warn'.
(defvar org-draw--session-waiters nil "Parked long-poll connection processes.")

(defun org-draw--sentinel (proc _event)
  "Connection sentinel: clean up parked timers and waiters on disconnect.
PROC is the connection process whose state is cleaned up."
  (unless (process-live-p proc)
    (let ((timer (process-get proc :org-draw-timer)))
      (when timer (cancel-timer timer) (process-put proc :org-draw-timer nil)))
    (setq org-draw--session-waiters (delq proc org-draw--session-waiters))))

(defun org-draw--log (_server conn _msg)
  "Set up an accepted connection CONN (called by :log on accept)."
  (set-process-coding-system conn 'binary 'binary)
  (set-process-query-on-exit-flag conn nil)
  (org-draw--reset-conn-state conn)
  (set-process-filter conn #'org-draw--filter)
  (set-process-sentinel conn #'org-draw--sentinel))

(defun org-draw--server-start (&optional port)
  "Start the org-draw HTTP server on PORT (default `org-draw-port').  Return the process."
  (when (process-live-p org-draw--server-process) (error "Org-draw server already running"))
  (setq org-draw--server-process
        (make-network-process
         :name "org-draw-server" :server t :host "0.0.0.0" :service (or port org-draw-port)
         :family 'ipv4 :coding 'binary :reuseaddr t :nowait nil :log #'org-draw--log)))

(defun org-draw--server-stop ()
  "Stop the org-draw HTTP server if running.
Also cancel any parked long-poll deadline timers and clear the waiter list so
nothing lingers (bounded to 55s otherwise) after shutdown."
  (dolist (proc org-draw--session-waiters)
    (let ((timer (and (processp proc) (process-get proc :org-draw-timer))))
      (when timer (cancel-timer timer))))
  (setq org-draw--session-waiters nil)
  (when (process-live-p org-draw--server-process) (delete-process org-draw--server-process))
  (setq org-draw--server-process nil))

;;;; Sessions & pairing

(cl-defstruct (org-draw-session (:constructor org-draw-session--make) (:copier nil))
  id mode name marker file drawing-bytes)

(defvar org-draw--queue nil "FIFO list of `org-draw-session' structs; head is served first.")

(defun org-draw--queue-reset ()
  "Empty the session queue."
  (setq org-draw--queue nil))
(defun org-draw-enqueue (session)
  "Append SESSION to the tail of the queue and return it."
  (setq org-draw--queue (append org-draw--queue (list session))) session)
(defun org-draw-queue-head ()
  "Return the session at the head of the queue, or nil."
  (car org-draw--queue))
(defun org-draw--queue-find (id)
  "Return the queued session whose id is ID, or nil."
  (cl-find id org-draw--queue :key #'org-draw-session-id :test #'equal))
(defun org-draw-queue-complete (id)
  "Remove the session with id ID from the queue and return it."
  (let ((s (org-draw--queue-find id)))
    (when s (setq org-draw--queue (delq s org-draw--queue))) s))
(defun org-draw-queue-cancel (id)
  "Remove the session with id ID from the queue, releasing its marker.
Return the removed session, or nil."
  (let ((s (org-draw--queue-find id)))
    (when s
      (when (markerp (org-draw-session-marker s)) (set-marker (org-draw-session-marker s) nil))
      (setq org-draw--queue (delq s org-draw--queue)))
    s))
(defun org-draw-queue-length ()
  "Return the number of sessions currently in the queue."
  (length org-draw--queue))

(defun org-draw--random-bytes (n)
  "Return N cryptographically-random bytes (unibyte).  Prefers /dev/urandom."
  (or (ignore-errors
        (let ((s (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (let ((coding-system-for-read 'binary))
                     (when (zerop (call-process "head" nil t nil "-c" (number-to-string n) "/dev/urandom"))
                       (buffer-string))))))
          (and s (= (length s) n) s)))
      ;; Fallback: NOT cryptographically strong (Emacs `random' PRNG).
      (let ((s (make-string n 0))) (dotimes (i n) (aset s i (random 256))) s)))

(defun org-draw--hex (bytes)
  "Return the lowercase hexadecimal encoding of unibyte string BYTES."
  (mapconcat (lambda (b) (format "%02x" b)) bytes ""))
(defun org-draw--random-uint (n)
  "Return an unsigned integer built from N random bytes."
  (let ((bytes (org-draw--random-bytes n)) (acc 0))
    (dotimes (i (length bytes)) (setq acc (+ (* acc 256) (aref bytes i)))) acc))
(defun org-draw-generate-id ()
  "Return a fresh random 32-hex-character session id."
  (org-draw--hex (org-draw--random-bytes 16)))
(defun org-draw-generate-token ()
  "Return a fresh random 32-hex-character pairing token."
  (org-draw--hex (org-draw--random-bytes 16)))

(cl-defstruct (org-draw-pairing (:constructor org-draw-pairing--make) (:copier nil))
  code (attempts-left 5) active)
(defvar org-draw--pairing nil "Current `org-draw-pairing' state, or nil.")

(defun org-draw-pairing-start ()
  "Begin pairing: fresh 6-digit code, reset attempts.  Return the code."
  (let ((code (format "%06d" (mod (org-draw--random-uint 3) 1000000))))
    (setq org-draw--pairing (org-draw-pairing--make :code code :attempts-left 5 :active t))
    code))
(defun org-draw-pairing-stop ()
  "End the current pairing session, clearing its state."
  (setq org-draw--pairing nil))

(defun org-draw-pairing-verify (code)
  "Verify CODE.  Return (:ok . TOKEN) / (:bad . N-left) / :closed."
  (let ((p org-draw--pairing))
    (cond
     ((or (null p) (not (org-draw-pairing-active p))) :closed)
     ((equal code (org-draw-pairing-code p))
      (let ((token (org-draw-generate-token)))
        (org-draw--persist-token token) (org-draw-pairing-stop) (cons :ok token)))
     (t (let ((left (1- (org-draw-pairing-attempts-left p))))
          (setf (org-draw-pairing-attempts-left p) left)
          (if (<= left 0) (progn (org-draw-pairing-stop) :closed) (cons :bad left)))))))

(defun org-draw--persist-token (token)
  "Append TOKEN as a line to `org-draw-token-file'."
  (let ((dir (file-name-directory org-draw-token-file)))
    (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat token "\n") nil org-draw-token-file 'append 'silent))
  token)
(defun org-draw--load-tokens ()
  "Return the list of persisted tokens from `org-draw-token-file'."
  (when (file-readable-p org-draw-token-file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents org-draw-token-file))
      (cl-remove-if #'string-empty-p (mapcar #'string-trim (split-string (buffer-string) "\n"))))))
(defun org-draw-token-valid-p (token)
  "Return non-nil if TOKEN is a non-empty string present in the token file."
  (and (stringp token) (not (string-empty-p token)) (member token (org-draw--load-tokens)) t))

;;;; Org integration

(defun org-draw--link-file-at-point ()
  "Absolute path of the `file:' link at point, or nil."
  (let ((ctx (org-element-context)))
    (when (and ctx (eq (org-element-type ctx) 'link)
               (equal (org-element-property :type ctx) "file"))
      (expand-file-name (org-element-property :path ctx)
                        (file-name-directory (or (buffer-file-name) default-directory))))))

(defun org-draw--make-insertion-marker ()
  "Insertion marker at end of the current line (insertion type t)."
  (copy-marker (line-end-position) t))

(defun org-draw-dwim-at-point ()
  "Classify point for `org-draw': (:edit FILE DRAWING) or (:new MARKER)."
  (unless (derived-mode-p 'org-mode) (user-error "Org-draw: not an Org buffer"))
  (unless (buffer-file-name) (user-error "Org-draw: buffer is not visiting a file"))
  (let* ((file (org-draw--link-file-at-point))
         (drawing (and file (org-draw--file-has-drawing-p file))))
    (if drawing (list :edit file drawing) (list :new (org-draw--make-insertion-marker)))))

(defun org-draw-default-file-name ()
  "Return a timestamped default PNG file name for a new figure."
  (format-time-string "fig-%Y%m%d-%H%M%S.png"))

(defun org-draw-resolve-directory (org-file)
  "Absolute figures dir for ORG-FILE, created on demand."
  (let ((dir (expand-file-name org-draw-directory (file-name-directory org-file))))
    (unless (file-directory-p dir) (make-directory dir t)) dir))

(defun org-draw--link-for (org-file target-file)
  "Return an Org `file:' link to TARGET-FILE relative to ORG-FILE."
  (format "[[file:%s]]" (file-relative-name target-file (file-name-directory org-file))))

(defun org-draw--refresh-inline-images (file)
  "Refresh inline images in every org buffer that displays FILE."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'org-mode) (buffer-file-name)
                 (save-excursion (goto-char (point-min))
                                 (search-forward (file-name-nondirectory file) nil t)))
        (cond ((fboundp 'org-redisplay-inline-images) (org-redisplay-inline-images))
              ((fboundp 'org-display-inline-images) (org-display-inline-images t t)))))))

;;;; Protocol endpoints

(defun org-draw--parse-json-body (req)
  "Parse REQ's body as JSON, returning a hash-table (string keys) or nil."
  (ignore-errors
    (json-parse-string (decode-coding-string (or (plist-get req :body) "") 'utf-8))))

(defun org-draw--require-token (req)
  "Return t if REQ is authorized; else send 401 and return nil.
When `org-draw-require-pairing' is nil (the default) every request is
authorized without a token."
  (if (or (not org-draw-require-pairing)
          (org-draw-token-valid-p (org-draw--header req "X-OrgDraw-Token")))
      t
    (org-draw--respond (plist-get req :proc) 401 "text/plain" "Unauthorized") nil))

(defvar org-draw--session-waiters nil "Parked long-poll connection processes.")

(defun org-draw--wake-waiters ()
  "Answer any parked pollers with the current queue head."
  (let ((head (org-draw-queue-head)))
    (when head
      (dolist (proc org-draw--session-waiters)
        (when (process-live-p proc)
          (org-draw-answer proc 200 "application/json" (org-draw--session-json head))))
      (setq org-draw--session-waiters nil))))

(defun org-draw--handle-pair (req)
  "POST /pair: verify the 6-digit code, issue a token.
REQ is the parsed request plist."
  (let* ((proc (plist-get req :proc)) (body (org-draw--parse-json-body req))
         (code (and body (gethash "code" body)))
         (result (and code (org-draw-pairing-verify code))))
    (pcase result
      (`(:ok . ,token)
       (org-draw--respond proc 200 "application/json"
                          (encode-coding-string (json-serialize (list :token token)) 'utf-8)))
      (_ (org-draw--respond proc 401 "text/plain" "Pairing failed")))))

(defun org-draw--handle-session (req)
  "GET /session: deliver the head session now, or park until one arrives.
REQ is the parsed request plist."
  (when (org-draw--require-token req)
    (let ((proc (plist-get req :proc)) (head (org-draw-queue-head)))
      (if head
          (org-draw-answer proc 200 "application/json" (org-draw--session-json head))
        (push proc org-draw--session-waiters)
        (org-draw-park proc org-draw--longpoll-seconds
                       (lambda (p)
                         (setq org-draw--session-waiters (delq p org-draw--session-waiters))
                         (org-draw--respond p 204 nil nil)))))))

(defun org-draw--handle-cancel (req)
  "POST /cancel: drop the named session from the queue.
REQ is the parsed request plist."
  (when (org-draw--require-token req)
    (let* ((proc (plist-get req :proc)) (body (org-draw--parse-json-body req))
           (id (and body (gethash "session_id" body))))
      (when id (org-draw-queue-cancel id))
      ;; Deliver the next head to any parked poller (defensive; multi-session).
      (org-draw--wake-waiters)
      (org-draw--respond proc 200 "application/json"
                         (encode-coding-string (json-serialize '(:ok t)) 'utf-8)))))

;;;; Network

(defun org-draw--ipv4-addresses ()
  "Non-loopback LAN IPv4 address strings, de-duplicated.
Excludes 127.x loopback and the 100.64.0.0/10 CGNAT range (Tailscale and
carrier NAT), which is generally not reachable for a same-network iPad."
  (let (out)
    (dolist (iface (network-interface-list))
      (let ((vec (cdr iface)))
        (when (and (vectorp vec) (= (length vec) 5)
                   (/= (aref vec 0) 127)                                   ; loopback
                   (not (and (= (aref vec 0) 100)                          ; 100.64/10 CGNAT
                             (>= (aref vec 1) 64) (<= (aref vec 1) 127))))
          (let ((ip (format "%d.%d.%d.%d" (aref vec 0) (aref vec 1) (aref vec 2) (aref vec 3))))
            (unless (member ip out) (push ip out))))))
    (nreverse out)))

(defvar org-draw--local-hostname-cache 'unset
  "Cached result of `org-draw--local-hostname' (computed once per session).")

(defun org-draw--local-hostname ()
  "Return this machine's mDNS hostname (e.g. \"my-mac.local\"), or nil.
Such names resolve via Bonjour/mDNS on macOS and iOS to the machine's CURRENT
IP address, so URLs built from them keep working across DHCP / network changes
-- no IP to track or re-enter.  On macOS the authoritative name comes from
`scutil --get LocalHostName'; elsewhere it falls back to the system name."
  (when (eq org-draw--local-hostname-cache 'unset)
    (setq org-draw--local-hostname-cache
          (let ((name (cond
                       ((executable-find "scutil")
                        (let ((n (string-trim
                                  (shell-command-to-string "scutil --get LocalHostName 2>/dev/null"))))
                          (and (not (string-empty-p n)) n)))
                       (t (car (split-string (system-name) "\\." t))))))
            (and name (not (string-empty-p name))
                 (if (string-suffix-p ".local" name) name (concat name ".local"))))))
  org-draw--local-hostname-cache)

(defun org-draw--host-candidates ()
  "Return the hosts used to build setup and receiver URLs.
The mDNS `.local' name comes FIRST (IP-change-proof), then the LAN IPv4
addresses as fallbacks."
  (let ((local (org-draw--local-hostname)))
    (append (and local (list local)) (org-draw--ipv4-addresses))))

;;;; Package assets

(defvar org-draw--package-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory the package (and the web canvas) lives in.")

(defun org-draw--read-file-unibyte (path)
  "Return the raw bytes of PATH as a unibyte string (no decoding)."
  (with-temp-buffer (set-buffer-multibyte nil)
                    (let ((coding-system-for-read 'binary)) (insert-file-contents-literally path))
                    (buffer-string)))

;;;; Commands

;;;###autoload
(defun org-draw-server-start ()
  "Start the org-draw HTTP server."
  (interactive)
  (unless (process-live-p org-draw--server-process)
    (org-draw--register-routes)
    (org-draw--server-start org-draw-port)
    (message "org-draw: server on port %d" org-draw-port)))

;;;###autoload
(defun org-draw-server-stop ()
  "Stop the org-draw HTTP server."
  (interactive)
  (org-draw--server-stop)
  (message "org-draw: server stopped"))

;;;###autoload
(defun org-draw-edit ()
  "Explicitly re-edit the org-draw figure at point.
Signal a clear error if point is on a foreign PNG (no embedded strokes, so not
re-editable) or not on a figure at all."
  (interactive)
  (let ((dwim (org-draw-dwim-at-point)))
    (unless (eq (car dwim) :edit)
      (let ((file (org-draw--link-file-at-point)))
        (if (and file (string-suffix-p ".png" (downcase file)))
            (user-error "Org-draw: %s has no embedded strokes; not re-editable (foreign PNG)"
                        (file-name-nondirectory file))
          (user-error "Org-draw: point is not on an org-draw figure"))))
    (org-draw)))

(defvar org-draw--url-announced nil
  "Non-nil once the receiver URL was copied to the clipboard this session.")

(defun org-draw--announce-receiver-url (&optional force)
  "Echo the primary receiver URL, and copy it when `org-draw-copy-url' is set.
Does nothing after the first call unless FORCE is non-nil.  Returns the URL."
  (let ((url (car (org-draw--web-receiver-urls))))
    (when (and url (or force (not org-draw--url-announced)))
      (setq org-draw--url-announced t)
      (if org-draw-copy-url
          (progn (kill-new url) (message "OrgDraw: open %s (copied to clipboard)" url))
        (message "OrgDraw: open %s" url)))
    url))

;;;###autoload
(defun org-draw-setup ()
  "Start the server and show the receiver URL (copied to the clipboard).
When `org-draw-require-pairing' is non-nil, also generate and show a 6-digit
pairing code."
  (interactive)
  (org-draw-server-start)
  (let* ((urls (org-draw--web-receiver-urls))
         (primary (org-draw--announce-receiver-url t))
         (code (and org-draw-require-pairing (org-draw-pairing-start))))
    (with-current-buffer (get-buffer-create "*org-draw setup*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "OrgDraw setup\n\n"
                "Open one of these URLs in a browser on your iPad (or any device).\n"
                "The first (.local) address is best; it keeps working even if your\n"
                "IP changes, so there's no IP to track:\n\n")
        (dolist (url urls)
          (insert "   " url "\n"))
        (insert "\n(the first URL is on your clipboard; with Universal Clipboard\n"
                " you can paste it straight into Safari on the iPad)\n")
        (if code
            (insert (format "\nThis network needs pairing. Enter this code on the page:\n\n      %s\n\n" code)
                    "   (expires after 5 wrong attempts; run M-x org-draw-setup again to reset)\n")
          (insert "\nNo pairing needed: open the URL and start drawing.\n"
                  "(set org-draw-require-pairing to require a code if others share your network)\n"))
        (insert "\nLeave the tab open; it receives every M-x org-draw.\n"))
      (special-mode)
      (display-buffer (current-buffer)))
    (or code primary)))


;;;; Chunk FORMAT byte

;; Named orPd chunk FORMAT bytes.  0x02 (web) is what this package produces now;
;; 0x01 (legacy Apple PKDrawing, from the retired native app) is still recognised
;; on read so any old figures degrade gracefully rather than erroring.
(defconst org-draw-format-pkdrawing #x01
  "Chunk FORMAT byte identifying legacy Apple PKDrawing stroke bytes (read-only).")
(defconst org-draw-format-web #x02
  "Chunk FORMAT byte identifying web-canvas JSON stroke data (UTF-8).")

(defun org-draw-format-valid-p (format)
  "Return non-nil if FORMAT is a known orPd chunk format byte."
  (memq format (list org-draw-format-pkdrawing org-draw-format-web)))

(defun org-draw--png-embed (png-bytes drawing-bytes &optional format)
  "Return PNG-BYTES with an `orPd' chunk (FORMAT byte + DRAWING-BYTES) before IEND.
FORMAT defaults to `org-draw-format-pkdrawing' (#x01) for back-compat, so existing
two-argument callers produce byte-identical output to v1.  Both PNG-BYTES and
DRAWING-BYTES must be unibyte.  Any existing `orPd' chunk is removed first."
  (let ((format (or format org-draw-format-pkdrawing)))
    (unless (and (integerp format) (<= 0 format 255))
      (error "Org-draw: bad orPd format byte: %S" format))
    (let* ((chunks (org-draw--png-chunks png-bytes))
           (iend (seq-find (lambda (c) (string= (plist-get c :type) "IEND")) chunks)))
      (unless iend (error "Org-draw: PNG has no IEND chunk"))
      (let* ((iend-start (plist-get iend :chunk-start))
             (data (concat (unibyte-string format) drawing-bytes))
             (new-chunk (org-draw--png-make-chunk org-draw--png-chunk-type data))
             (orpd (seq-find (lambda (c) (string= (plist-get c :type)
                                                  org-draw--png-chunk-type))
                             chunks))
             (prefix (if orpd
                         (concat (substring png-bytes 0 (plist-get orpd :chunk-start))
                                 (substring png-bytes (plist-get orpd :chunk-end) iend-start))
                       (substring png-bytes 0 iend-start))))
        (concat prefix new-chunk (substring png-bytes iend-start))))))

(defun org-draw--png-extract (png-bytes)
  "Return (FORMAT . BYTES) for the embedded orPd chunk in PNG-BYTES, or nil.
FORMAT is the leading format byte (`org-draw-format-pkdrawing' etc.); BYTES is the
remaining stroke payload (\"\" for an embedded-but-empty drawing).  Returns nil
for a foreign PNG with no orPd chunk (distinct from an empty payload).

NOTE: v1 returned BYTES directly.  Callers wanting only the bytes should use
`org-draw--png-extract-bytes'."
  (let* ((chunks (org-draw--png-chunks png-bytes))
         (orpd (seq-find (lambda (c) (string= (plist-get c :type)
                                              org-draw--png-chunk-type))
                         chunks)))
    (when orpd
      (let ((start (plist-get orpd :data-start))
            (dlen (plist-get orpd :data-len)))
        (when (< dlen 1) (error "Org-draw: orPd chunk is empty (no format byte)"))
        (cons (aref png-bytes start)
              (substring png-bytes (1+ start) (+ start dlen)))))))

(defun org-draw--png-extract-bytes (png-bytes)
  "Return only the embedded stroke BYTES from PNG-BYTES, or nil for a foreign PNG.
Compatibility shim reproducing the v1 `org-draw--png-extract' contract on top of
the v2 (FORMAT . BYTES) return."
  (let ((pair (org-draw--png-extract png-bytes)))
    (and pair (cdr pair))))

(defun org-draw--png-extract-format (png-bytes)
  "Return only the embedded FORMAT byte from PNG-BYTES, or nil for a foreign PNG."
  (let ((pair (org-draw--png-extract png-bytes)))
    (and pair (car pair))))

(defun org-draw--file-drawing (file)
  "Return (FORMAT . BYTES) for FILE if it is a PNG with an orPd chunk, else nil."
  (and (stringp file) (file-readable-p file)
       (org-draw--png-extract
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((coding-system-for-read 'binary))
            (insert-file-contents-literally file))
          (buffer-string)))))

(defun org-draw--file-has-drawing-p (file)
  "Return embedded stroke BYTES for FILE if it has an orPd chunk, else nil.
Bytes-only compatibility shim over `org-draw--file-drawing'."
  (let ((pair (org-draw--file-drawing file)))
    (and pair (cdr pair))))

;;;; Figure background

(defcustom org-draw-figure-background 'transparent
  "Background baked behind a new figure by the drawing client.
One of:
  `transparent' (default): no fill; the PNG is exported with an alpha channel so
     it adapts to any theme.  Fixes the v1 white-ink-on-white bug.
  `white':  bake an opaque white background (v1 behaviour).
  `dark':   bake an opaque dark (near-black) background.
  a string: any CSS/hex colour, e.g. \"#1e1e2e\", baked opaque.
The value travels to the client in the session JSON \"background\" field; the
client decides how to render it (or leaves the PNG transparent)."
  :type '(choice (const :tag "Transparent (theme-adaptive)" transparent)
          (const :tag "White" white)
          (const :tag "Dark" dark)
          (string :tag "Colour (hex/CSS)"))
  :group 'org-draw)

(defun org-draw--background-wire (&optional value)
  "Return the wire string for VALUE (default `org-draw-figure-background').
The `white' symbol maps to \"light\" to match both the web canvas and the native
CanvasBackground enum rawValue; `transparent'/`dark' map to their names; a colour
string passes through verbatim.  Always a non-empty string so the field is stable."
  (let ((v (or value org-draw-figure-background)))
    (cond
     ((eq v 'transparent) "transparent")
     ((eq v 'white) "light")
     ((eq v 'dark) "dark")
     ((and (stringp v) (not (string-empty-p v))) v)
     (t "transparent"))))

;;;; Web-open behaviour

(defcustom org-draw-open-browser nil
  "When non-nil, `org-draw' and `org-draw-edit' open the canvas in a web browser
on the machine running Emacs (via `browse-url'), so you can draw right there.
Leave nil to just queue the drawing for a receiver tab you keep open elsewhere.
For a custom opener, set `org-draw-web-open-function' instead."
  :type 'boolean :group 'org-draw)

(defcustom org-draw-web-open-function nil
  "Custom function to open the per-session /canvas URL on the Emacs host.
When set, it takes precedence over `org-draw-open-browser' and is called with the
URL string (e.g. `browse-url', or a function that targets a specific browser)."
  :type '(choice (const :tag "None" nil) (function :tag "Opener function"))
  :group 'org-draw)

(defun org-draw--url-opener ()
  "Return the function to open a URL on this host, or nil if auto-open is off."
  (cond ((functionp org-draw-web-open-function) org-draw-web-open-function)
        (org-draw-open-browser #'browse-url)))

;;;; Session struct extension
;;
;; The `org-draw-session' struct has no `format' slot.  Rather than redefine the
;; cl-defstruct (which would break byte-compiled accessors elsewhere), we keep
;; the format in a parallel hash table keyed by session id.

(defvar org-draw--session-format (make-hash-table :test 'equal)
  "Map session-id -> chunk FORMAT byte for the session's target client.")

(defun org-draw--session-set-format (id format)
  "Record FORMAT (chunk byte) for session ID."
  (puthash id format org-draw--session-format))

(defun org-draw--session-get-format (id)
  "Return the recorded chunk FORMAT byte for session ID, defaulting to PKDrawing."
  (or (gethash id org-draw--session-format) org-draw-format-pkdrawing))

;;;; Session JSON

(defun org-draw--session-json (session)
  "Serialize SESSION to the wire JSON string (unibyte-safe UTF-8).
v2 shape adds:
  \"background\": the string from `org-draw-figure-background' (theme hint).
  \"format\": \"pkdrawing\"|\"web\", which client owns this session (from the
     recorded target format), so a client can sanity-check it should handle it."
  (let* ((drawing (org-draw-session-drawing-bytes session))
         (id (org-draw-session-id session))
         (format (org-draw--session-get-format id))
         (fmt-name (if (eql format org-draw-format-web) "web" "pkdrawing")))
    (encode-coding-string
     (json-serialize
      (list :session_id id
            :mode (symbol-name (org-draw-session-mode session))
            :name (or (org-draw-session-name session) "")
            :background (org-draw--background-wire)
            :format fmt-name
            :drawing (if drawing (base64-encode-string drawing t) :null)))
     'utf-8)))

;;;; /result web-format handling

(defun org-draw--result-format (body)
  "Return the chunk FORMAT byte requested by a /result BODY hash-table.
Reads the optional \"format\" string field: \"web\" -> 0x02, else 0x01."
  (let ((f (and (hash-table-p body) (gethash "format" body))))
    (if (and (stringp f) (string= f "web")) org-draw-format-web
      org-draw-format-pkdrawing)))

(defun org-draw--write-png (file png-bytes drawing-bytes &optional format)
  "Embed DRAWING-BYTES (with FORMAT byte) into PNG-BYTES and write FILE (binary).
FORMAT defaults to `org-draw-format-pkdrawing' for byte-identical v1 behaviour."
  (let ((out (org-draw--png-embed png-bytes drawing-bytes
                                  (or format org-draw-format-pkdrawing))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert out)
      (let ((coding-system-for-write 'binary))
        (write-region (point-min) (point-max) file nil 'silent))))
  file)

(defun org-draw-insert-new-figure (session png-bytes drawing-bytes &optional format)
  "Handle a `new'-mode SESSION result: write file (with FORMAT), insert link.
PNG-BYTES is the exported PNG and DRAWING-BYTES the strokes to embed.
FORMAT defaults to PKDrawing.  Behaviour otherwise matches v1."
  (let* ((format (or format org-draw-format-pkdrawing))
         (marker (org-draw-session-marker session))
         (buf (and (markerp marker) (marker-buffer marker))))
    (if (not (buffer-live-p buf))
        (let ((file (or (org-draw-session-file session)
                        (expand-file-name (or (org-draw-session-name session)
                                              (org-draw-default-file-name))
                                          default-directory))))
          (org-draw--write-png file png-bytes drawing-bytes format)
          (display-warning 'org-draw
                           (format "Target buffer gone; figure written to %s" file)
                           :warning)
          file)
      (with-current-buffer buf
        (let* ((org-file (buffer-file-name))
               (dir (org-draw-resolve-directory org-file))
               (name (or (org-draw-session-name session)
                         (funcall org-draw-file-name-function)))
               (file (expand-file-name name dir)))
          (org-draw--write-png file png-bytes drawing-bytes format)
          (save-excursion
            (goto-char (marker-position marker))
            (when org-draw-insert-attr-width
              (insert (format "#+ATTR_ORG: :width %d\n" org-draw-insert-attr-width)))
            (insert (org-draw--link-for org-file file)))
          (set-marker marker nil)
          (org-draw--refresh-inline-images file)
          file)))))

(defun org-draw-overwrite-figure (session png-bytes drawing-bytes &optional format)
  "Handle an `edit'-mode SESSION result: overwrite in place with FORMAT.
PNG-BYTES is the exported PNG and DRAWING-BYTES the strokes to embed.
Return path."
  (let ((file (org-draw-session-file session))
        (format (or format org-draw-format-pkdrawing)))
    (org-draw--write-png file png-bytes drawing-bytes format)
    (org-draw--refresh-inline-images file)
    file))

(defun org-draw--handle-result (req)
  "POST /result: decode png+drawing, embed with the requested FORMAT, complete.
REQ is the parsed request plist.
Body: {session_id, png:base64, drawing:base64, format?:\"web\"|\"pkdrawing\"}.
When \"format\" is \"web\", the strokes are embedded as an orPd chunk with the
0x02 format byte so the figure re-edits back to the web canvas."
  (when (org-draw--require-token req)
    (let* ((proc (plist-get req :proc)) (body (org-draw--parse-json-body req)))
      (if (not body)
          (org-draw--respond proc 400 "text/plain" "Bad JSON")
        (let* ((id (gethash "session_id" body))
               (session (org-draw--queue-find id))
               (format (org-draw--result-format body))
               (png (ignore-errors (base64-decode-string (gethash "png" body))))
               (drawing (ignore-errors (base64-decode-string (gethash "drawing" body)))))
          (cond
           ((not session) (org-draw--respond proc 404 "text/plain" "Unknown session"))
           ((or (not png) (not drawing))
            (org-draw--respond proc 400 "text/plain" "Bad payload"))
           (t (condition-case err
                  (progn
                    (if (eq (org-draw-session-mode session) 'edit)
                        (org-draw-overwrite-figure session png drawing format)
                      (org-draw-insert-new-figure session png drawing format))
                    (org-draw-queue-complete id)
                    (remhash id org-draw--session-format)
                    (org-draw--wake-waiters)
                    (org-draw--respond proc 200 "application/json"
                                       (encode-coding-string
                                        (json-serialize '(:ok t)) 'utf-8)))
                (error (org-draw--respond proc 500 "text/plain" (format "%S" err)))))))))))

;;;; /canvas (and /web) endpoint

(defcustom org-draw-web-canvas-file
  (expand-file-name "web/canvas.html" org-draw--package-dir)
  "Path to the shipped web-canvas HTML served by GET /canvas.
The server injects a JSON config block; see `org-draw--canvas-html'."
  :type 'file :group 'org-draw)

(defun org-draw--query-param (query name)
  "Return the value of URL query parameter NAME from QUERY string, or nil.
QUERY is the raw part after `?' (may be nil).  Values are URL-decoded."
  (when (and query (stringp query))
    (catch 'found
      (dolist (pair (split-string query "&" t))
        (let ((eq-pos (string-search "=" pair)))
          (when eq-pos
            (let ((k (substring pair 0 eq-pos))
                  (v (substring pair (1+ eq-pos))))
              (when (string= (url-unhex-string k) name)
                (throw 'found (url-unhex-string v)))))))
      nil)))

(defun org-draw--json-escape (string)
  "Return STRING as a JSON string literal (including the surrounding quotes)."
  (json-serialize string))

(cl-defun org-draw--canvas-config (session-id token mode background web-json
                                              &key name result-url cancel-url)
  "Return the injected JS config block string for the web canvas.
SESSION-ID, TOKEN, MODE, BACKGROUND, WEB-JSON and NAME populate the config
fields.  Defines window.ORGDRAW_CONFIG = {...}.  Field names are the exact contract the
shipped web/canvas.html reads: `session_id', `token', `mode', `name',
`background', `resultUrl', `drawing', plus `format' (\"web\"), and the
convenience aliases `result_path'/`cancel_path'/`token_header'.
RESULT-URL/CANCEL-URL default to the relative \"/result\"/\"/cancel\" (the canvas
falls back to these too); pass absolute URLs when the browser's origin differs."
  (let ((json (json-serialize
               (list :session_id (or session-id "")
                     :token (or token "")
                     :mode (or mode "new")
                     :name (or name "")
                     :background (or background "transparent")
                     ;; The canvas reads cfg.resultUrl (camelCase); keep result_path as an alias.
                     :resultUrl (or result-url "/result")
                     :result_path "/result"
                     :cancel_path (or cancel-url "/cancel")
                     :session_path "/session"
                     :pair_path "/pair"
                     :token_header "X-OrgDraw-Token"
                     :require_pairing (if org-draw-require-pairing t :false)
                     :format "web"
                     :drawing (if (and web-json (stringp web-json) (not (string-empty-p web-json)))
                                  web-json :null)))))
    ;; Escape < and > so a string field (e.g. a figure name containing the text
    ;; "</script>") cannot break out of this inline <script> element.  These
    ;; become < / >, which JS reads back as the original characters.
    (setq json (replace-regexp-in-string "<" "\\u003c" json t t))
    (setq json (replace-regexp-in-string ">" "\\u003e" json t t))
    (concat "<script>window.ORGDRAW_CONFIG=" json ";</script>")))

(defun org-draw--canvas-fallback-html ()
  "Minimal self-contained canvas HTML used when the shipped file is absent.
This is a placeholder so /canvas is never a 404 during integration; the web
agent ships the real full-featured web/canvas.html."
  (concat
   "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
   "<title>OrgDraw Canvas</title></head><body>"
   "<p>OrgDraw web canvas placeholder. Config injected below; the shipped "
   "web/canvas.html replaces this page.</p>"
   "<pre id=\"cfg\"></pre>"
   "<script>document.getElementById('cfg').textContent="
   "JSON.stringify(window.ORGDRAW_CONFIG,null,2);</script>"
   "</body></html>"))

(cl-defun org-draw--canvas-html (session-id token mode background web-json
                                            &key name result-url cancel-url)
  "Return the full web-canvas HTML with the config block injected.
SESSION-ID, TOKEN, MODE, BACKGROUND and WEB-JSON populate the config block.
Reads `org-draw-web-canvas-file' when present, else a placeholder page.  The
config <script> is injected just before </head> (or prepended if no </head>).
NAME/RESULT-URL/CANCEL-URL are threaded into the config block."
  (let* ((config (org-draw--canvas-config session-id token mode background web-json
                                          :name name :result-url result-url
                                          :cancel-url cancel-url))
         (template (if (file-readable-p org-draw-web-canvas-file)
                       (org-draw--read-file-unibyte org-draw-web-canvas-file)
                     (encode-coding-string (org-draw--canvas-fallback-html) 'utf-8)))
         (template (if (multibyte-string-p template)
                       template
                     (decode-coding-string template 'utf-8)))
         (head-end (or (string-search "</head>" template)
                       (string-search "</HEAD>" template))))
    (if head-end
        (concat (substring template 0 head-end) config (substring template head-end))
      (concat config template))))

(defun org-draw--handle-canvas (req)
  "GET /canvas (and /web): serve the web canvas page.
REQ is the parsed request plist.

The page is static (HTML + JS) and is served unconditionally: the sensitive
endpoints it calls (/session, /result, /cancel) stay token-gated server-side,
and /pair is code-gated, so serving the shell to an unauthenticated browser is
safe.  This lets the page act as a RECEIVER; it pairs in-browser (using the
`org-draw-setup' code), stores its token, and long-polls /session.

A SPECIFIC session is injected only for the opt-in host-open flow, i.e. when a
valid ?token= AND ?session= are present in the URL; then the page draws that one
session immediately (and, for an edit, restores its strokes).  Otherwise the
page boots as a receiver."
  (let* ((proc (plist-get req :proc))
         (query (plist-get req :query))
         (q-session (org-draw--query-param query "session"))
         (q-token (org-draw--query-param query "token"))
         ;; With pairing off, a ?session= URL is served without a token so the
         ;; host-open (auto-open) flow can go straight into the drawing.
         (authed (or (not org-draw-require-pairing) (org-draw-token-valid-p q-token)))
         (session (and authed q-session (org-draw--queue-find q-session)))
         (session-id (if session q-session ""))
         (token (if authed q-token ""))
         (mode (if session (symbol-name (org-draw-session-mode session)) "new"))
         (name (and session (or (org-draw-session-name session) "")))
         (drawing (and session (org-draw-session-drawing-bytes session)))
         ;; For a web edit the stored strokes ARE the JSON text (unibyte UTF-8).
         ;; The canvas restore() base64-decodes CONFIG.drawing (atob), so inject
         ;; base64 of the raw web-JSON bytes, not the raw JSON text.
         (web-json (and drawing (base64-encode-string drawing t)))
         (background (org-draw--background-wire))
         ;; Absolute POST targets on the request's own Host so a browser reaching
         ;; us by IP/MagicDNS posts back to the same origin.
         (host (or (org-draw--header req "host")
                   (format "127.0.0.1:%d" org-draw-port)))
         (result-url (format "http://%s/result" host))
         (cancel-url (format "http://%s/cancel" host)))
    (org-draw--respond proc 200 "text/html; charset=utf-8"
                       (encode-coding-string
                        (org-draw--canvas-html session-id token mode background web-json
                                               :name name :result-url result-url
                                               :cancel-url cancel-url)
                        'utf-8)
                       ;; Never let the browser cache the canvas shell, otherwise
                       ;; an updated web/canvas.html is masked by a stale copy.
                       '(("Cache-Control" . "no-store, no-cache, must-revalidate")
                         ("Pragma" . "no-cache")))))

(defun org-draw--canvas-url (host session-id token)
  "Build the http://HOST/canvas?session=.. URL string.
&token= is appended only when TOKEN is a non-empty string."
  (concat (format "http://%s/canvas?session=%s" host (url-hexify-string (or session-id "")))
          (if (and (stringp token) (not (string-empty-p token)))
              (format "&token=%s" (url-hexify-string token))
            "")))

(defun org-draw--first-host ()
  "Return a \"IP:PORT\" reachable host for building client URLs.
Prefers the first non-loopback IPv4; falls back to 127.0.0.1."
  (let ((ip (car (org-draw--ipv4-addresses))))
    (format "%s:%d" (or ip "127.0.0.1") org-draw-port)))

;;;; Route registration

(defun org-draw--register-routes ()
  "Register all protocol + asset routes (idempotent)."
  (org-draw-route "POST" "/pair" #'org-draw--handle-pair)
  (org-draw-route "GET" "/session" #'org-draw--handle-session)
  (org-draw-route "POST" "/result" #'org-draw--handle-result)
  (org-draw-route "POST" "/cancel" #'org-draw--handle-cancel)
  (org-draw-route "GET" "/canvas" #'org-draw--handle-canvas)
  (org-draw-route "GET" "/web" #'org-draw--handle-canvas))

;;;; org-draw routing

(defun org-draw--web-receiver-urls ()
  "List of http://HOST:PORT/canvas receiver URLs; the IP-stable `.local' host first.
Open one ONCE on the iPad (or any browser); it pairs in-page and then long-polls
for drawings queued by `org-draw'.  Prefer the `.local' URL; it survives
the Mac's IP changing, so you never have to re-open a new address."
  (mapcar (lambda (h) (format "http://%s:%d/canvas" h org-draw-port))
          (org-draw--host-candidates)))

(defun org-draw--open-web-session (session-id &optional token)
  "Queue web SESSION-ID for the receiver, and optionally open it on this host.
By default nothing opens here; a receiver tab you keep open long-polls and picks
the session up.  When `org-draw-open-browser' (or a custom
`org-draw-web-open-function') is set, ALSO open the per-session URL on this
machine.  TOKEN rides in the URL only when pairing is required."
  (let ((opener (org-draw--url-opener)))
    (when opener
      (let ((tok (if org-draw-require-pairing
                     (or token (car (last (org-draw--load-tokens))))
                   "")))
        (funcall opener (org-draw--canvas-url (org-draw--first-host) session-id tok)))))
  (let ((receiver (car (org-draw--web-receiver-urls))))
    (if receiver
        (message "org-draw: queued (%s)" receiver)
      (message "org-draw: queued")))
  session-id)

;;;###autoload
(defun org-draw ()
  "Draw into the org buffer at point.  On an org-draw figure link, re-edit it.
Both new figures and edits open the web canvas at GET /canvas: a receiver tab
you keep open long-polls and picks up the request (or set
`org-draw-web-open-function' to also open it on the machine running Emacs)."
  (interactive)
  (org-draw-server-start)
  (org-draw--announce-receiver-url)
  (let ((dwim (org-draw-dwim-at-point)))
    (pcase dwim
      (`(:edit ,file ,drawing)
       ;; DRAWING may be bare bytes or a (FORMAT . BYTES) pair.
       (let* ((pair (org-draw--file-drawing file))
              (format (if pair (car pair) org-draw-format-web))
              (bytes (if pair (cdr pair) drawing))
              (id (org-draw-generate-id)))
         (org-draw--session-set-format id format)
         (org-draw-enqueue (org-draw-session--make
                            :id id :mode 'edit
                            :name (file-name-nondirectory file)
                            :file file :drawing-bytes bytes))
         (org-draw--wake-waiters)
         (org-draw--open-web-session id nil)))
      (`(:new ,marker)
       (let ((id (org-draw-generate-id)))
         (org-draw--session-set-format id org-draw-format-web)
         (org-draw-enqueue (org-draw-session--make
                            :id id :mode 'new
                            :name (funcall org-draw-file-name-function)
                            :marker marker))
         (org-draw--wake-waiters)
         (org-draw--open-web-session id nil))))))

;;;; Transient menu

(defun org-draw-set-background (value)
  "Set `org-draw-figure-background' to VALUE interactively."
  (interactive
   (list (let ((choice (completing-read
                        "Figure background: "
                        '("transparent" "white" "dark" "custom colour") nil t)))
           (pcase choice
             ("transparent" 'transparent)
             ("white" 'white)
             ("dark" 'dark)
             (_ (read-string "Hex/CSS colour: " "#1e1e2e"))))))
  (setq org-draw-figure-background value)
  (message "org-draw: figure background is now %s" (org-draw--background-wire)))

(defun org-draw-server-toggle ()
  "Start the server if stopped, else stop it."
  (interactive)
  (if (process-live-p org-draw--server-process)
      (org-draw-server-stop)
    (org-draw-server-start)))

;;;###autoload (autoload 'org-draw-menu "org-draw" nil t)
(transient-define-prefix org-draw-menu ()
  "OrgDraw command dispatcher."
  [["Draw"
    ("d" "Draw / edit at point" org-draw)
    ("e" "Edit figure at point" org-draw-edit)]
   ["Setup"
    ("s" "Setup + pair" org-draw-setup)
    ("S" "Toggle server" org-draw-server-toggle)]
   ["Config"
    ("b" "Set figure background" org-draw-set-background
     :transient t)]])

(provide 'org-draw)
;;; org-draw.el ends here
