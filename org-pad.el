;;; org-pad.el --- Seamless iPad drawing into org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Saleh

;; Author: Saleh <root@lr0.org>
;; Maintainer: Saleh <root@lr0.org>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: org, multimedia, hypermedia
;; URL: https://github.com/larrasket/org-pad.el

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

(defgroup org-pad nil
  "Seamless iPad drawing into Org mode."
  :group 'org
  :prefix "org-pad-")

(defcustom org-pad-port 8777
  "TCP port the org-pad HTTP server listens on."
  :type 'integer :group 'org-pad)

(defcustom org-pad-token-file
  (expand-file-name "org-pad-tokens" user-emacs-directory)
  "File where paired-device tokens are persisted, one token per line."
  :type 'file :group 'org-pad)

(defcustom org-pad-directory "figures"
  "Directory for new figures, resolved relative to the visited org file."
  :type 'string :group 'org-pad)

(defcustom org-pad-file-name-function #'org-pad-default-file-name
  "Function returning the base file name (no directory) for a new figure."
  :type 'function :group 'org-pad)

(defcustom org-pad-insert-attr-width nil
  "When non-nil, insert `#+ATTR_ORG: :width N' above a new figure's link.
The value is the pixel width N."
  :type '(choice (const :tag "Off" nil) integer) :group 'org-pad)

(defconst org-pad--max-body (* 50 1024 1024)
  "Maximum accepted request body size in bytes (50 MB).  Larger => 413.")

(defconst org-pad--longpoll-seconds 55
  "Seconds a long-poll connection may be parked before a 204 is sent.")

;;;; PNG format ------------------------------------------------------------

(defconst org-pad--png-signature
  (unibyte-string #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)
  "The 8-byte PNG file signature.")

(defconst org-pad--crc32-table
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

(defun org-pad--crc32 (bytes)
  "Compute the PNG/zlib CRC32 of unibyte string BYTES.
Return a 32-bit unsigned integer.  Matches Python `zlib.crc32'."
  (let ((crc #xFFFFFFFF) (len (length bytes)))
    (dotimes (i len)
      (setq crc (logxor (aref org-pad--crc32-table
                              (logand (logxor crc (aref bytes i)) #xFF))
                        (ash crc -8))))
    (logxor crc #xFFFFFFFF)))

(defun org-pad--u32-encode (n)
  "Encode integer N as a 4-byte big-endian unibyte string."
  (unibyte-string (logand (ash n -24) #xFF) (logand (ash n -16) #xFF)
                  (logand (ash n -8) #xFF) (logand n #xFF)))

(defun org-pad--u32-decode (bytes offset)
  "Decode 4 big-endian bytes from unibyte BYTES at OFFSET into an integer."
  (logior (ash (aref bytes offset) 24) (ash (aref bytes (+ offset 1)) 16)
          (ash (aref bytes (+ offset 2)) 8) (aref bytes (+ offset 3))))

(defconst org-pad--png-chunk-type "orPd" "Private PNG chunk type holding drawing bytes.")

(defun org-pad--png-chunks (bytes)
  "Walk PNG unibyte string BYTES, returning a list of chunk descriptor plists:
\(:type STR :data-start INT :data-len INT :chunk-start INT :chunk-end INT).
Signal an error on a bad signature or truncation."
  (unless (and (>= (length bytes) 8)
               (string= (substring bytes 0 8) org-pad--png-signature))
    (error "Org-pad: not a PNG (bad signature)"))
  (let ((pos 8) (len (length bytes)) (chunks '()))
    (while (< pos len)
      (when (> (+ pos 8) len) (error "Org-pad: truncated PNG chunk header at %d" pos))
      (let* ((data-len (org-pad--u32-decode bytes pos))
             (type (substring bytes (+ pos 4) (+ pos 8)))
             (data-start (+ pos 8))
             (chunk-end (+ data-start data-len 4)))
        (when (> chunk-end len) (error "Org-pad: truncated PNG chunk %s at %d" type pos))
        (push (list :type type :data-start data-start :data-len data-len
                    :chunk-start pos :chunk-end chunk-end)
              chunks)
        (setq pos chunk-end)))
    (nreverse chunks)))

(defun org-pad--png-make-chunk (type data)
  "Build a serialized PNG chunk from 4-char TYPE and unibyte DATA."
  (let* ((type-data (concat type data)) (crc (org-pad--crc32 type-data)))
    (concat (org-pad--u32-encode (length data)) type-data (org-pad--u32-encode crc))))

;;;; HTTP server ----------------------------------------------------------

(defvar org-pad--server-process nil "The listening server process, or nil when stopped.")

(defun org-pad--status-text (status)
  "Return the HTTP reason phrase for numeric STATUS."
  (pcase status
    (200 "OK") (204 "No Content") (302 "Found") (400 "Bad Request")
    (401 "Unauthorized") (404 "Not Found") (405 "Method Not Allowed")
    (413 "Payload Too Large") (500 "Internal Server Error") (_ "Status")))

(defun org-pad--safe-delete (proc)
  "Delete PROC if live, ignoring errors."
  (when (process-live-p proc) (ignore-errors (delete-process proc))))

(defun org-pad--respond (proc status content-type body &optional headers keep-alive)
  "Write an HTTP/1.1 response to PROC and (unless KEEP-ALIVE) close it.
STATUS is a number, CONTENT-TYPE a string or nil, BODY a unibyte string or nil,
HEADERS an alist of extra (NAME . VALUE).  Content-Length is always sent."
  (when (process-live-p proc)
    (let* ((body (or body ""))
           (body (if (multibyte-string-p body) (encode-coding-string body 'utf-8) body))
           (parts (list (format "HTTP/1.1 %d %s\r\n" status (org-pad--status-text status)))))
      (when content-type (push (format "Content-Type: %s\r\n" content-type) parts))
      (push (format "Content-Length: %d\r\n" (length body)) parts)
      (push (format "Connection: %s\r\n" (if keep-alive "keep-alive" "close")) parts)
      (dolist (h headers) (push (format "%s: %s\r\n" (car h) (cdr h)) parts))
      (push "\r\n" parts)
      (let ((head (apply #'concat (nreverse parts))))
        (process-send-string proc (concat (string-to-unibyte head) body)))
      (if keep-alive (org-pad--reset-conn-state proc) (org-pad--safe-delete proc)))))

(defvar org-pad--routes nil
  "Alist of ((METHOD . PATH) . HANDLER).  METHOD uppercase string, PATH exact.")

(defun org-pad-route (method path handler)
  "Register HANDLER for METHOD (string) and exact PATH (string)."
  (setf (alist-get (cons (upcase method) path) org-pad--routes nil nil #'equal) handler))

(defun org-pad--parse-request-line (line)
  "Parse HTTP request LINE -> (METHOD PATH QUERY VERSION) or nil."
  (when (string-match
         "\\`\\([A-Za-z]+\\) \\([^ ?]*\\)\\(?:\\?\\([^ ]*\\)\\)? \\(HTTP/[0-9.]+\\)\\'" line)
    (list (upcase (match-string 1 line)) (match-string 2 line)
          (match-string 3 line) (match-string 4 line))))

(defun org-pad--parse-headers (block)
  "Parse header BLOCK (CRLF-separated) -> alist of (LOWERCASE-NAME . VALUE)."
  (let (headers)
    (dolist (line (split-string block "\r\n" t))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line)) headers)))
    (nreverse headers)))

(defun org-pad--header (req name)
  "Value of header NAME (case-insensitive) from REQ, or nil."
  (cdr (assoc (downcase name) (plist-get req :headers))))

(defun org-pad--dispatch (req)
  "Find and call the handler for REQ, or send 404/405/500."
  (let* ((method (plist-get req :method)) (path (plist-get req :path))
         (proc (plist-get req :proc))
         (handler (cdr (assoc (cons method path) org-pad--routes))))
    (cond
     (handler (condition-case err (funcall handler req)
                (error (org-pad--respond proc 500 "text/plain" (format "Internal error: %S" err)))))
     ((cl-some (lambda (r) (equal (cdar r) path)) org-pad--routes)
      (org-pad--respond proc 405 "text/plain" "Method Not Allowed"))
     (t (org-pad--respond proc 404 "text/plain" "Not Found")))))

(defun org-pad--reset-conn-state (proc)
  "Initialise/reset the per-connection parse state on PROC."
  (process-put proc :org-pad-state
               (list :buf "" :phase 'headers :header-end nil :method nil :path nil
                     :query nil :version nil :headers nil :content-length nil)))

(defun org-pad--filter (proc chunk)
  "Process filter: accumulate unibyte CHUNK on PROC and drive parsing."
  (let ((st (process-get proc :org-pad-state)))
    (unless st (org-pad--reset-conn-state proc) (setq st (process-get proc :org-pad-state)))
    (plist-put st :buf (concat (plist-get st :buf) chunk))
    (org-pad--advance proc st)))

(defun org-pad--advance (proc st)
  "Advance the parse state machine for PROC given state ST."
  (pcase (plist-get st :phase)
    ('headers (org-pad--try-parse-headers proc st))
    ('body    (org-pad--try-parse-body proc st))
    ('done    nil)))

(defun org-pad--try-parse-headers (proc st)
  "Parse the header block from ST if complete; advance to body.
PROC is the connection process to respond on."
  (let* ((buf (plist-get st :buf)) (sep (string-search "\r\n\r\n" buf)))
    (when sep
      (let* ((head (substring buf 0 sep)) (nl (string-search "\r\n" head))
             (req-line (if nl (substring head 0 nl) head))
             (hdr-block (if nl (substring head (+ nl 2)) ""))
             (parsed (org-pad--parse-request-line req-line)))
        (if (not parsed)
            (org-pad--respond proc 400 "text/plain" "Bad Request Line")
          (cl-destructuring-bind (method path query version) parsed
            (let* ((headers (org-pad--parse-headers hdr-block))
                   (cl-str (cdr (assoc "content-length" headers)))
                   (clen (and cl-str (string-to-number cl-str))))
              (cond
               ((and cl-str (not (string-match-p "\\`[0-9]+\\'" cl-str)))
                (org-pad--respond proc 400 "text/plain" "Bad Content-Length"))
               ((and clen (> clen org-pad--max-body))
                (org-pad--respond proc 413 "text/plain" "Payload Too Large"))
               (t (plist-put st :method method) (plist-put st :path path)
                  (plist-put st :query query) (plist-put st :version version)
                  (plist-put st :headers headers) (plist-put st :content-length (or clen 0))
                  (plist-put st :header-end (+ sep 4)) (plist-put st :phase 'body)
                  (org-pad--try-parse-body proc st))))))))))

(defun org-pad--try-parse-body (proc st)
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
        (org-pad--dispatch req)))))

;;;; Long-poll: park a connection, answer later; 55s deadline -> 204.

(defun org-pad-park (proc &optional seconds on-timeout)
  "Park connection PROC for a long-poll, arming a deadline timer.
After SECONDS (default `org-pad--longpoll-seconds') with no reply, call
ON-TIMEOUT with PROC, else send 204.  Return the timer."
  (let* ((secs (or seconds org-pad--longpoll-seconds))
         (timer (run-at-time secs nil
                             (lambda ()
                               (process-put proc :org-pad-timer nil)
                               (when (process-live-p proc)
                                 (if on-timeout (funcall on-timeout proc)
                                   (org-pad--respond proc 204 nil nil)))))))
    (process-put proc :org-pad-timer timer)
    (process-put proc :org-pad-parked t)
    timer))

(defun org-pad-answer (proc status content-type body &optional headers)
  "Answer a parked long-poll PROC, disarming its deadline timer.
STATUS, CONTENT-TYPE, BODY and HEADERS are passed through to
`org-pad--respond'."
  (let ((timer (process-get proc :org-pad-timer)))
    (when timer (cancel-timer timer)))
  (process-put proc :org-pad-timer nil)
  (process-put proc :org-pad-parked nil)
  (org-pad--respond proc status content-type body headers))

;; Forward declaration: the waiter list is populated by the /session handler in
;; Milestone 4, but the sentinel references it here, earlier in file order.  A
;; `defvar' only silences free-variable warnings for code that FOLLOWS it, so the
;; real defvar in Task 4.2 does NOT cover this sentinel — declare it here too
;; (a repeated defvar of the same symbol is harmless in Elisp).  Without this,
;; `make compile' fails under `byte-compile-error-on-warn'.
(defvar org-pad--session-waiters nil "Parked long-poll connection processes.")

(defun org-pad--sentinel (proc _event)
  "Connection sentinel: clean up parked timers and waiters on disconnect.
PROC is the connection process whose state is cleaned up."
  (unless (process-live-p proc)
    (let ((timer (process-get proc :org-pad-timer)))
      (when timer (cancel-timer timer) (process-put proc :org-pad-timer nil)))
    (setq org-pad--session-waiters (delq proc org-pad--session-waiters))))

(defun org-pad--log (_server conn _msg)
  "Set up an accepted connection CONN (called by :log on accept)."
  (set-process-coding-system conn 'binary 'binary)
  (set-process-query-on-exit-flag conn nil)
  (org-pad--reset-conn-state conn)
  (set-process-filter conn #'org-pad--filter)
  (set-process-sentinel conn #'org-pad--sentinel))

(defun org-pad--server-start (&optional port)
  "Start the org-pad HTTP server on PORT (default `org-pad-port').  Return the process."
  (when (process-live-p org-pad--server-process) (error "Org-pad server already running"))
  (setq org-pad--server-process
        (make-network-process
         :name "org-pad-server" :server t :host "0.0.0.0" :service (or port org-pad-port)
         :family 'ipv4 :coding 'binary :reuseaddr t :nowait nil :log #'org-pad--log)))

(defun org-pad--server-stop ()
  "Stop the org-pad HTTP server if running.
Also cancel any parked long-poll deadline timers and clear the waiter list so
nothing lingers (bounded to 55s otherwise) after shutdown."
  (dolist (proc org-pad--session-waiters)
    (let ((timer (and (processp proc) (process-get proc :org-pad-timer))))
      (when timer (cancel-timer timer))))
  (setq org-pad--session-waiters nil)
  (when (process-live-p org-pad--server-process) (delete-process org-pad--server-process))
  (setq org-pad--server-process nil))

;;;; Sessions & pairing ---------------------------------------------------

(cl-defstruct (org-pad-session (:constructor org-pad-session--make) (:copier nil))
  id mode name marker file drawing-bytes)

(defvar org-pad--queue nil "FIFO list of `org-pad-session' structs; head is served first.")

(defun org-pad--queue-reset ()
  "Empty the session queue."
  (setq org-pad--queue nil))
(defun org-pad-enqueue (session)
  "Append SESSION to the tail of the queue and return it."
  (setq org-pad--queue (append org-pad--queue (list session))) session)
(defun org-pad-queue-head ()
  "Return the session at the head of the queue, or nil."
  (car org-pad--queue))
(defun org-pad--queue-find (id)
  "Return the queued session whose id is ID, or nil."
  (cl-find id org-pad--queue :key #'org-pad-session-id :test #'equal))
(defun org-pad-queue-complete (id)
  "Remove the session with id ID from the queue and return it."
  (let ((s (org-pad--queue-find id)))
    (when s (setq org-pad--queue (delq s org-pad--queue))) s))
(defun org-pad-queue-cancel (id)
  "Remove the session with id ID from the queue, releasing its marker.
Return the removed session, or nil."
  (let ((s (org-pad--queue-find id)))
    (when s
      (when (markerp (org-pad-session-marker s)) (set-marker (org-pad-session-marker s) nil))
      (setq org-pad--queue (delq s org-pad--queue)))
    s))
(defun org-pad-queue-length ()
  "Return the number of sessions currently in the queue."
  (length org-pad--queue))

(defun org-pad--random-bytes (n)
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

(defun org-pad--hex (bytes)
  "Return the lowercase hexadecimal encoding of unibyte string BYTES."
  (mapconcat (lambda (b) (format "%02x" b)) bytes ""))
(defun org-pad--random-uint (n)
  "Return an unsigned integer built from N random bytes."
  (let ((bytes (org-pad--random-bytes n)) (acc 0))
    (dotimes (i (length bytes)) (setq acc (+ (* acc 256) (aref bytes i)))) acc))
(defun org-pad-generate-id ()
  "Return a fresh random 32-hex-character session id."
  (org-pad--hex (org-pad--random-bytes 16)))
(defun org-pad-generate-token ()
  "Return a fresh random 32-hex-character pairing token."
  (org-pad--hex (org-pad--random-bytes 16)))

(cl-defstruct (org-pad-pairing (:constructor org-pad-pairing--make) (:copier nil))
  code (attempts-left 5) active)
(defvar org-pad--pairing nil "Current `org-pad-pairing' state, or nil.")

(defun org-pad-pairing-start ()
  "Begin pairing: fresh 6-digit code, reset attempts.  Return the code."
  (let ((code (format "%06d" (mod (org-pad--random-uint 3) 1000000))))
    (setq org-pad--pairing (org-pad-pairing--make :code code :attempts-left 5 :active t))
    code))
(defun org-pad-pairing-stop ()
  "End the current pairing session, clearing its state."
  (setq org-pad--pairing nil))

(defun org-pad-pairing-verify (code)
  "Verify CODE.  Return (:ok . TOKEN) / (:bad . N-left) / :closed."
  (let ((p org-pad--pairing))
    (cond
     ((or (null p) (not (org-pad-pairing-active p))) :closed)
     ((equal code (org-pad-pairing-code p))
      (let ((token (org-pad-generate-token)))
        (org-pad--persist-token token) (org-pad-pairing-stop) (cons :ok token)))
     (t (let ((left (1- (org-pad-pairing-attempts-left p))))
          (setf (org-pad-pairing-attempts-left p) left)
          (if (<= left 0) (progn (org-pad-pairing-stop) :closed) (cons :bad left)))))))

(defun org-pad--persist-token (token)
  "Append TOKEN as a line to `org-pad-token-file'."
  (let ((dir (file-name-directory org-pad-token-file)))
    (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat token "\n") nil org-pad-token-file 'append 'silent))
  token)
(defun org-pad--load-tokens ()
  "Return the list of persisted tokens from `org-pad-token-file'."
  (when (file-readable-p org-pad-token-file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents org-pad-token-file))
      (cl-remove-if #'string-empty-p (mapcar #'string-trim (split-string (buffer-string) "\n"))))))
(defun org-pad-token-valid-p (token)
  "Return non-nil if TOKEN is a non-empty string present in the token file."
  (and (stringp token) (not (string-empty-p token)) (member token (org-pad--load-tokens)) t))

;;;; Org integration ------------------------------------------------------

(defun org-pad--link-file-at-point ()
  "Absolute path of the `file:' link at point, or nil."
  (let ((ctx (org-element-context)))
    (when (and ctx (eq (org-element-type ctx) 'link)
               (equal (org-element-property :type ctx) "file"))
      (expand-file-name (org-element-property :path ctx)
                        (file-name-directory (or (buffer-file-name) default-directory))))))

(defun org-pad--make-insertion-marker ()
  "Insertion marker at end of the current line (insertion type t)."
  (copy-marker (line-end-position) t))

(defun org-pad-dwim-at-point ()
  "Classify point for `org-pad-draw': (:edit FILE DRAWING) or (:new MARKER)."
  (unless (derived-mode-p 'org-mode) (user-error "Org-pad: not an Org buffer"))
  (unless (buffer-file-name) (user-error "Org-pad: buffer is not visiting a file"))
  (let* ((file (org-pad--link-file-at-point))
         (drawing (and file (org-pad--file-has-drawing-p file))))
    (if drawing (list :edit file drawing) (list :new (org-pad--make-insertion-marker)))))

(defun org-pad-default-file-name ()
  "Return a timestamped default PNG file name for a new figure."
  (format-time-string "fig-%Y%m%d-%H%M%S.png"))

(defun org-pad-resolve-directory (org-file)
  "Absolute figures dir for ORG-FILE, created on demand."
  (let ((dir (expand-file-name org-pad-directory (file-name-directory org-file))))
    (unless (file-directory-p dir) (make-directory dir t)) dir))

(defun org-pad--link-for (org-file target-file)
  "Return an Org `file:' link to TARGET-FILE relative to ORG-FILE."
  (format "[[file:%s]]" (file-relative-name target-file (file-name-directory org-file))))

(defun org-pad--refresh-inline-images (file)
  "Refresh inline images in every org buffer that displays FILE."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'org-mode) (buffer-file-name)
                 (save-excursion (goto-char (point-min))
                                 (search-forward (file-name-nondirectory file) nil t)))
        (cond ((fboundp 'org-redisplay-inline-images) (org-redisplay-inline-images))
              ((fboundp 'org-display-inline-images) (org-display-inline-images t t)))))))

;;;; Protocol endpoints ---------------------------------------------------

(defun org-pad--parse-json-body (req)
  "Parse REQ's body as JSON, returning a hash-table (string keys) or nil."
  (ignore-errors
    (json-parse-string (decode-coding-string (or (plist-get req :body) "") 'utf-8))))

(defun org-pad--require-token (req)
  "Return t if REQ carries a valid token; else send 401 and return nil."
  (if (org-pad-token-valid-p (org-pad--header req "X-OrgPad-Token")) t
    (org-pad--respond (plist-get req :proc) 401 "text/plain" "Unauthorized") nil))

;; Already forward-declared in Task 2.3 (for the sentinel).  Repeating the defvar
;; here with its docstring is harmless and keeps this section self-contained.
(defvar org-pad--session-waiters nil "Parked long-poll connection processes.")

(defun org-pad--wake-waiters ()
  "Answer any parked pollers with the current queue head."
  (let ((head (org-pad-queue-head)))
    (when head
      (dolist (proc org-pad--session-waiters)
        (when (process-live-p proc)
          (org-pad-answer proc 200 "application/json" (org-pad--session-json head))))
      (setq org-pad--session-waiters nil))))

(defun org-pad--handle-pair (req)
  "POST /pair: verify the 6-digit code, issue a token.
REQ is the parsed request plist."
  (let* ((proc (plist-get req :proc)) (body (org-pad--parse-json-body req))
         (code (and body (gethash "code" body)))
         (result (and code (org-pad-pairing-verify code))))
    (pcase result
      (`(:ok . ,token)
       (org-pad--respond proc 200 "application/json"
                         (encode-coding-string (json-serialize (list :token token)) 'utf-8)))
      (_ (org-pad--respond proc 401 "text/plain" "Pairing failed")))))

(defun org-pad--handle-session (req)
  "GET /session: deliver the head session now, or park until one arrives.
REQ is the parsed request plist."
  (when (org-pad--require-token req)
    (let ((proc (plist-get req :proc)) (head (org-pad-queue-head)))
      (if head
          (org-pad-answer proc 200 "application/json" (org-pad--session-json head))
        (push proc org-pad--session-waiters)
        (org-pad-park proc org-pad--longpoll-seconds
                      (lambda (p)
                        (setq org-pad--session-waiters (delq p org-pad--session-waiters))
                        (org-pad--respond p 204 nil nil)))))))

(defun org-pad--handle-cancel (req)
  "POST /cancel: drop the named session from the queue.
REQ is the parsed request plist."
  (when (org-pad--require-token req)
    (let* ((proc (plist-get req :proc)) (body (org-pad--parse-json-body req))
           (id (and body (gethash "session_id" body))))
      (when id (org-pad-queue-cancel id))
      ;; Deliver the next head to any parked poller (defensive; multi-session).
      (org-pad--wake-waiters)
      (org-pad--respond proc 200 "application/json"
                        (encode-coding-string (json-serialize '(:ok t)) 'utf-8)))))

;;;; Network ---------------------------------------------------------------

(defun org-pad--ipv4-addresses ()
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

(defvar org-pad--local-hostname-cache 'unset
  "Cached result of `org-pad--local-hostname' (computed once per session).")

(defun org-pad--local-hostname ()
  "Return this machine's mDNS hostname (e.g. \"my-mac.local\"), or nil.
Such names resolve via Bonjour/mDNS on macOS and iOS to the machine's CURRENT
IP address, so URLs built from them keep working across DHCP / network changes
-- no IP to track or re-enter.  On macOS the authoritative name comes from
`scutil --get LocalHostName'; elsewhere it falls back to the system name."
  (when (eq org-pad--local-hostname-cache 'unset)
    (setq org-pad--local-hostname-cache
          (let ((name (cond
                       ((executable-find "scutil")
                        (let ((n (string-trim
                                  (shell-command-to-string "scutil --get LocalHostName 2>/dev/null"))))
                          (and (not (string-empty-p n)) n)))
                       (t (car (split-string (system-name) "\\." t))))))
            (and name (not (string-empty-p name))
                 (if (string-suffix-p ".local" name) name (concat name ".local"))))))
  org-pad--local-hostname-cache)

(defun org-pad--host-candidates ()
  "Return the hosts used to build setup and receiver URLs.
The mDNS `.local' name comes FIRST (IP-change-proof), then the LAN IPv4
addresses as fallbacks."
  (let ((local (org-pad--local-hostname)))
    (append (and local (list local)) (org-pad--ipv4-addresses))))

;;;; Package assets -------------------------------------------------------

(defvar org-pad--package-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory the package (and the web canvas) lives in.")

(defun org-pad--read-file-unibyte (path)
  "Return the raw bytes of PATH as a unibyte string (no decoding)."
  (with-temp-buffer (set-buffer-multibyte nil)
                    (let ((coding-system-for-read 'binary)) (insert-file-contents-literally path))
                    (buffer-string)))

;;;; Commands --------------------------------------------------------------

;;;###autoload
(defun org-pad-server-start ()
  "Start the org-pad HTTP server."
  (interactive)
  (unless (process-live-p org-pad--server-process)
    (org-pad--register-routes)
    (org-pad--server-start org-pad-port)
    (message "org-pad: server on port %d" org-pad-port)))

;;;###autoload
(defun org-pad-server-stop ()
  "Stop the org-pad HTTP server."
  (interactive)
  (org-pad--server-stop)
  (message "org-pad: server stopped"))

;;;###autoload
(defun org-pad-edit ()
  "Explicitly re-edit the org-pad figure at point.
Signal a clear error if point is on a foreign PNG (no embedded strokes, so not
re-editable — spec error row) or not on a figure at all."
  (interactive)
  (let ((dwim (org-pad-dwim-at-point)))
    (unless (eq (car dwim) :edit)
      (let ((file (org-pad--link-file-at-point)))
        (if (and file (string-suffix-p ".png" (downcase file)))
            (user-error "Org-pad: %s has no embedded strokes; not re-editable (foreign PNG)"
                        (file-name-nondirectory file))
          (user-error "Org-pad: point is not on an org-pad figure"))))
    (org-pad-draw)))

;;;###autoload
(defun org-pad-setup ()
  "Start the server and show the receiver URL and a pairing code."
  (interactive)
  (org-pad-server-start)
  (let ((code (org-pad-pairing-start)))
    ;; Copy the code so it can be pasted (e.g. via Universal Clipboard) into the
    ;; browser's pairing field.
    (kill-new code)
    (with-current-buffer (get-buffer-create "*org-pad setup*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "OrgPad setup\n\n"
                "Open one of these URLs in a browser on your iPad (or any device).\n"
                "The first (.local) address is best — it keeps working even if your\n"
                "Mac's IP changes, so there's no IP to track:\n\n")
        (dolist (url (org-pad--web-receiver-urls))
          (insert "   " url "\n"))
        (insert (format "\nEnter this pairing code on the page:\n\n      %s\n\n" code)
                "   (copied to your clipboard — with Universal Clipboard you can\n"
                "    paste it on the iPad)\n"
                "   (code expires after 5 wrong attempts; run M-x org-pad-setup again to reset)\n"
                "\nLeave the tab open — it pairs once, then receives every"
                " M-x org-pad-draw.\n"))
      (special-mode)
      (display-buffer (current-buffer)))
    code))


;;;; v2 additions ---------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; 1. Chunk FORMAT byte
;;;; ---------------------------------------------------------------------------

;; Named orPd chunk FORMAT bytes.  0x02 (web) is what this package produces now;
;; 0x01 (legacy Apple PKDrawing, from the retired native app) is still recognised
;; on read so any old figures degrade gracefully rather than erroring.
(defconst org-pad-format-pkdrawing #x01
  "Chunk FORMAT byte identifying legacy Apple PKDrawing stroke bytes (read-only).")
(defconst org-pad-format-web #x02
  "Chunk FORMAT byte identifying web-canvas JSON stroke data (UTF-8).")

(defun org-pad-format-valid-p (format)
  "Return non-nil if FORMAT is a known orPd chunk format byte."
  (memq format (list org-pad-format-pkdrawing org-pad-format-web)))

;; --- Redefinition of org-pad--png-embed: add an optional FORMAT arg. ---
;; Back-compat: called with two args it behaves EXACTLY like v1 (format #x01).
(defun org-pad--png-embed (png-bytes drawing-bytes &optional format)
  "Return PNG-BYTES with an `orPd' chunk (FORMAT byte + DRAWING-BYTES) before IEND.
FORMAT defaults to `org-pad-format-pkdrawing' (#x01) for back-compat, so existing
two-argument callers produce byte-identical output to v1.  Both PNG-BYTES and
DRAWING-BYTES must be unibyte.  Any existing `orPd' chunk is removed first."
  (let ((format (or format org-pad-format-pkdrawing)))
    (unless (and (integerp format) (<= 0 format 255))
      (error "Org-pad: bad orPd format byte: %S" format))
    (let* ((chunks (org-pad--png-chunks png-bytes))
           (iend (seq-find (lambda (c) (string= (plist-get c :type) "IEND")) chunks)))
      (unless iend (error "Org-pad: PNG has no IEND chunk"))
      (let* ((iend-start (plist-get iend :chunk-start))
             (data (concat (unibyte-string format) drawing-bytes))
             (new-chunk (org-pad--png-make-chunk org-pad--png-chunk-type data))
             (orpd (seq-find (lambda (c) (string= (plist-get c :type)
                                                  org-pad--png-chunk-type))
                             chunks))
             (prefix (if orpd
                         (concat (substring png-bytes 0 (plist-get orpd :chunk-start))
                                 (substring png-bytes (plist-get orpd :chunk-end) iend-start))
                       (substring png-bytes 0 iend-start))))
        (concat prefix new-chunk (substring png-bytes iend-start))))))

;; --- Redefinition of org-pad--png-extract: return (FORMAT . BYTES). ---
;; This CHANGES the return type.  A compat shim below preserves the old
;; bytes-only contract for callers that were not updated.
(defun org-pad--png-extract (png-bytes)
  "Return (FORMAT . BYTES) for the embedded orPd chunk in PNG-BYTES, or nil.
FORMAT is the leading format byte (`org-pad-format-pkdrawing' etc.); BYTES is the
remaining stroke payload (\"\" for an embedded-but-empty drawing).  Returns nil
for a foreign PNG with no orPd chunk (distinct from an empty payload).

NOTE: v1 returned BYTES directly.  Callers wanting only the bytes should use
`org-pad--png-extract-bytes'."
  (let* ((chunks (org-pad--png-chunks png-bytes))
         (orpd (seq-find (lambda (c) (string= (plist-get c :type)
                                              org-pad--png-chunk-type))
                         chunks)))
    (when orpd
      (let ((start (plist-get orpd :data-start))
            (dlen (plist-get orpd :data-len)))
        (when (< dlen 1) (error "Org-pad: orPd chunk is empty (no format byte)"))
        (cons (aref png-bytes start)
              (substring png-bytes (1+ start) (+ start dlen)))))))

(defun org-pad--png-extract-bytes (png-bytes)
  "Return only the embedded stroke BYTES from PNG-BYTES, or nil for a foreign PNG.
Compatibility shim reproducing the v1 `org-pad--png-extract' contract on top of
the v2 (FORMAT . BYTES) return."
  (let ((pair (org-pad--png-extract png-bytes)))
    (and pair (cdr pair))))

(defun org-pad--png-extract-format (png-bytes)
  "Return only the embedded FORMAT byte from PNG-BYTES, or nil for a foreign PNG."
  (let ((pair (org-pad--png-extract png-bytes)))
    (and pair (car pair))))

;; --- Update the org-integration reader to preserve the format on edit. ---
;; v1 `org-pad--file-has-drawing-p' returned bytes.  v2 returns (FORMAT . BYTES)
;; so DWIM can route an edit to the right client.  Keep the old name as the
;; bytes-only shim for anything that only wanted a truthy "has strokes?".
(defun org-pad--file-drawing (file)
  "Return (FORMAT . BYTES) for FILE if it is a PNG with an orPd chunk, else nil."
  (and (stringp file) (file-readable-p file)
       (org-pad--png-extract
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((coding-system-for-read 'binary))
            (insert-file-contents-literally file))
          (buffer-string)))))

(defun org-pad--file-has-drawing-p (file)
  "Return embedded stroke BYTES for FILE if it has an orPd chunk, else nil.
Bytes-only compatibility shim over `org-pad--file-drawing'."
  (let ((pair (org-pad--file-drawing file)))
    (and pair (cdr pair))))

;;;; ---------------------------------------------------------------------------
;;;; 2. Figure background
;;;; ---------------------------------------------------------------------------

(defcustom org-pad-figure-background 'transparent
  "Background baked behind a new figure by the drawing client.
One of:
  `transparent' (default) — no fill; the PNG is exported with an alpha channel so
     it adapts to any theme.  Fixes the v1 white-ink-on-white bug.
  `white'  — bake an opaque white background (v1 behaviour).
  `dark'   — bake an opaque dark (near-black) background.
  a string — any CSS/hex colour, e.g. \"#1e1e2e\", baked opaque.
The value travels to the client in the session JSON \"background\" field; the
client decides how to render it (or leaves the PNG transparent)."
  :type '(choice (const :tag "Transparent (theme-adaptive)" transparent)
          (const :tag "White" white)
          (const :tag "Dark" dark)
          (string :tag "Colour (hex/CSS)"))
  :group 'org-pad)

(defun org-pad--background-wire (&optional value)
  "Return the wire string for VALUE (default `org-pad-figure-background').
The `white' symbol maps to \"light\" to match both the web canvas and the native
CanvasBackground enum rawValue; `transparent'/`dark' map to their names; a colour
string passes through verbatim.  Always a non-empty string so the field is stable."
  (let ((v (or value org-pad-figure-background)))
    (cond
     ((eq v 'transparent) "transparent")
     ((eq v 'white) "light")
     ((eq v 'dark) "dark")
     ((and (stringp v) (not (string-empty-p v))) v)
     (t "transparent"))))

;;;; ---------------------------------------------------------------------------
;;;; 3. Web-open behaviour
;;;; ---------------------------------------------------------------------------

(defcustom org-pad-web-open-function nil
  "How to open the per-session /canvas URL on the Emacs host machine.

Default nil: `org-pad-draw' NEVER opens a browser on this machine.  A web
drawing is simply queued, and a receiver tab you keep open on your iPad (or
any browser at the /canvas URL) long-polls and picks it up.

Set this to a function (e.g. `browse-url') ONLY if you also want the drawing
opened on the machine running Emacs.  It is called with the per-session URL."
  :type '(choice (const :tag "Never open on this machine (receiver picks it up)" nil)
                 (function :tag "Also open on this machine (e.g. browse-url)"))
  :group 'org-pad)

;;;; ---------------------------------------------------------------------------
;;;; Session struct extension: format + token, so JSON + routing have them.
;;;; ---------------------------------------------------------------------------
;;
;; The v1 `org-pad-session' struct has no `format' slot.  Rather than redefine
;; the cl-defstruct (which would break byte-compiled accessors elsewhere), we
;; stash the format on the session's plist-free struct by using a wrapper: we
;; keep the format in a parallel weak table keyed by session id.  This keeps the
;; drop-in surface additive.  (In the real merge, add a `format' slot to the
;; struct; see the integrator notes.)

(defvar org-pad--session-format (make-hash-table :test 'equal)
  "Map session-id -> chunk FORMAT byte for the session's target client.")

(defun org-pad--session-set-format (id format)
  "Record FORMAT (chunk byte) for session ID."
  (puthash id format org-pad--session-format))

(defun org-pad--session-get-format (id)
  "Return the recorded chunk FORMAT byte for session ID, defaulting to PKDrawing."
  (or (gethash id org-pad--session-format) org-pad-format-pkdrawing))

;;;; ---------------------------------------------------------------------------
;;;; Session JSON with "background" (and forward-compatible "format").
;;;; ---------------------------------------------------------------------------

(defun org-pad--session-json (session)
  "Serialize SESSION to the wire JSON string (unibyte-safe UTF-8).
v2 shape adds:
  \"background\": the string from `org-pad-figure-background' (theme hint).
  \"format\": \"pkdrawing\"|\"web\" — which client owns this session (from the
     recorded target format), so a client can sanity-check it should handle it."
  (let* ((drawing (org-pad-session-drawing-bytes session))
         (id (org-pad-session-id session))
         (format (org-pad--session-get-format id))
         (fmt-name (if (eql format org-pad-format-web) "web" "pkdrawing")))
    (encode-coding-string
     (json-serialize
      (list :session_id id
            :mode (symbol-name (org-pad-session-mode session))
            :name (or (org-pad-session-name session) "")
            :background (org-pad--background-wire)
            :format fmt-name
            :drawing (if drawing (base64-encode-string drawing t) :null)))
     'utf-8)))

;;;; ---------------------------------------------------------------------------
;;;; 4a. /result web-format handling
;;;; ---------------------------------------------------------------------------
;;
;; v1 /result always embedded with format #x01.  v2 reads an optional "format"
;; field ("web"|"pkdrawing", default pkdrawing) and threads the chosen FORMAT
;; byte through to `org-pad--png-embed'.  The write helpers gain an optional
;; format argument (default #x01 -> byte-identical to v1).

(defun org-pad--result-format (body)
  "Return the chunk FORMAT byte requested by a /result BODY hash-table.
Reads the optional \"format\" string field: \"web\" -> 0x02, else 0x01."
  (let ((f (and (hash-table-p body) (gethash "format" body))))
    (if (and (stringp f) (string= f "web")) org-pad-format-web
      org-pad-format-pkdrawing)))

(defun org-pad--write-png (file png-bytes drawing-bytes &optional format)
  "Embed DRAWING-BYTES (with FORMAT byte) into PNG-BYTES and write FILE (binary).
FORMAT defaults to `org-pad-format-pkdrawing' for byte-identical v1 behaviour."
  (let ((out (org-pad--png-embed png-bytes drawing-bytes
                                 (or format org-pad-format-pkdrawing))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert out)
      (let ((coding-system-for-write 'binary))
        (write-region (point-min) (point-max) file nil 'silent))))
  file)

;; Thread FORMAT through the two result writers (redefinitions add optional arg).
(defun org-pad-insert-new-figure (session png-bytes drawing-bytes &optional format)
  "Handle a `new'-mode SESSION result: write file (with FORMAT), insert link.
PNG-BYTES is the exported PNG and DRAWING-BYTES the strokes to embed.
FORMAT defaults to PKDrawing.  Behaviour otherwise matches v1."
  (let* ((format (or format org-pad-format-pkdrawing))
         (marker (org-pad-session-marker session))
         (buf (and (markerp marker) (marker-buffer marker))))
    (if (not (buffer-live-p buf))
        (let ((file (or (org-pad-session-file session)
                        (expand-file-name (or (org-pad-session-name session)
                                              (org-pad-default-file-name))
                                          default-directory))))
          (org-pad--write-png file png-bytes drawing-bytes format)
          (display-warning 'org-pad
                           (format "Target buffer gone; figure written to %s" file)
                           :warning)
          file)
      (with-current-buffer buf
        (let* ((org-file (buffer-file-name))
               (dir (org-pad-resolve-directory org-file))
               (name (or (org-pad-session-name session)
                         (funcall org-pad-file-name-function)))
               (file (expand-file-name name dir)))
          (org-pad--write-png file png-bytes drawing-bytes format)
          (save-excursion
            (goto-char (marker-position marker))
            (when org-pad-insert-attr-width
              (insert (format "#+ATTR_ORG: :width %d\n" org-pad-insert-attr-width)))
            (insert (org-pad--link-for org-file file)))
          (set-marker marker nil)
          (org-pad--refresh-inline-images file)
          file)))))

(defun org-pad-overwrite-figure (session png-bytes drawing-bytes &optional format)
  "Handle an `edit'-mode SESSION result: overwrite in place with FORMAT.
PNG-BYTES is the exported PNG and DRAWING-BYTES the strokes to embed.
Return path."
  (let ((file (org-pad-session-file session))
        (format (or format org-pad-format-pkdrawing)))
    (org-pad--write-png file png-bytes drawing-bytes format)
    (org-pad--refresh-inline-images file)
    file))

(defun org-pad--handle-result (req)
  "POST /result: decode png+drawing, embed with the requested FORMAT, complete.
REQ is the parsed request plist.
Body: {session_id, png:base64, drawing:base64, format?:\"web\"|\"pkdrawing\"}.
When \"format\" is \"web\", the strokes are embedded as an orPd chunk with the
0x02 format byte so the figure re-edits back to the web canvas."
  (when (org-pad--require-token req)
    (let* ((proc (plist-get req :proc)) (body (org-pad--parse-json-body req)))
      (if (not body)
          (org-pad--respond proc 400 "text/plain" "Bad JSON")
        (let* ((id (gethash "session_id" body))
               (session (org-pad--queue-find id))
               (format (org-pad--result-format body))
               (png (ignore-errors (base64-decode-string (gethash "png" body))))
               (drawing (ignore-errors (base64-decode-string (gethash "drawing" body)))))
          (cond
           ((not session) (org-pad--respond proc 404 "text/plain" "Unknown session"))
           ((or (not png) (not drawing))
            (org-pad--respond proc 400 "text/plain" "Bad payload"))
           (t (condition-case err
                  (progn
                    (if (eq (org-pad-session-mode session) 'edit)
                        (org-pad-overwrite-figure session png drawing format)
                      (org-pad-insert-new-figure session png drawing format))
                    (org-pad-queue-complete id)
                    (remhash id org-pad--session-format)
                    (org-pad--wake-waiters)
                    (org-pad--respond proc 200 "application/json"
                                      (encode-coding-string
                                       (json-serialize '(:ok t)) 'utf-8)))
                (error (org-pad--respond proc 500 "text/plain" (format "%S" err)))))))))))

;;;; ---------------------------------------------------------------------------
;;;; 4b. /canvas (and /web) endpoint
;;;; ---------------------------------------------------------------------------

(defcustom org-pad-web-canvas-file
  (expand-file-name "web/canvas.html" org-pad--package-dir)
  "Path to the shipped web-canvas HTML served by GET /canvas.
The server injects a JSON config block; see `org-pad--canvas-html'."
  :type 'file :group 'org-pad)

(defun org-pad--query-param (query name)
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

(defun org-pad--json-escape (string)
  "Return STRING as a JSON string literal (including the surrounding quotes)."
  (json-serialize string))

(cl-defun org-pad--canvas-config (session-id token mode background web-json
                                             &key name result-url cancel-url)
  "Return the injected JS config block string for the web canvas.
SESSION-ID, TOKEN, MODE, BACKGROUND, WEB-JSON and NAME populate the config
fields.  Defines window.ORGPAD_CONFIG = {...}.  Field names are the exact contract the
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
                     :token_header "X-OrgPad-Token"
                     :format "web"
                     :drawing (if (and web-json (stringp web-json) (not (string-empty-p web-json)))
                                  web-json :null)))))
    ;; Escape < and > so a string field (e.g. a figure name containing the text
    ;; "</script>") cannot break out of this inline <script> element.  These
    ;; become < / >, which JS reads back as the original characters.
    (setq json (replace-regexp-in-string "<" "\\u003c" json t t))
    (setq json (replace-regexp-in-string ">" "\\u003e" json t t))
    (concat "<script>window.ORGPAD_CONFIG=" json ";</script>")))

(defun org-pad--canvas-fallback-html ()
  "Minimal self-contained canvas HTML used when the shipped file is absent.
This is a placeholder so /canvas is never a 404 during integration; the web
agent ships the real full-featured web/canvas.html."
  (concat
   "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
   "<title>OrgPad Canvas</title></head><body>"
   "<p>OrgPad web canvas placeholder. Config injected below; the shipped "
   "web/canvas.html replaces this page.</p>"
   "<pre id=\"cfg\"></pre>"
   "<script>document.getElementById('cfg').textContent="
   "JSON.stringify(window.ORGPAD_CONFIG,null,2);</script>"
   "</body></html>"))

(cl-defun org-pad--canvas-html (session-id token mode background web-json
                                           &key name result-url cancel-url)
  "Return the full web-canvas HTML with the config block injected.
SESSION-ID, TOKEN, MODE, BACKGROUND and WEB-JSON populate the config block.
Reads `org-pad-web-canvas-file' when present, else a placeholder page.  The
config <script> is injected just before </head> (or prepended if no </head>).
NAME/RESULT-URL/CANCEL-URL are threaded into the config block."
  (let* ((config (org-pad--canvas-config session-id token mode background web-json
                                         :name name :result-url result-url
                                         :cancel-url cancel-url))
         (template (if (file-readable-p org-pad-web-canvas-file)
                       (org-pad--read-file-unibyte org-pad-web-canvas-file)
                     (encode-coding-string (org-pad--canvas-fallback-html) 'utf-8)))
         (template (if (multibyte-string-p template)
                       template
                     (decode-coding-string template 'utf-8)))
         (head-end (or (string-search "</head>" template)
                       (string-search "</HEAD>" template))))
    (if head-end
        (concat (substring template 0 head-end) config (substring template head-end))
      (concat config template))))

(defun org-pad--handle-canvas (req)
  "GET /canvas (and /web): serve the web canvas page.
REQ is the parsed request plist.

The page is static (HTML + JS) and is served unconditionally: the sensitive
endpoints it calls (/session, /result, /cancel) stay token-gated server-side,
and /pair is code-gated, so serving the shell to an unauthenticated browser is
safe.  This lets the page act as a RECEIVER — it pairs in-browser (using the
`org-pad-setup' code), stores its token, and long-polls /session.

A SPECIFIC session is injected only for the opt-in host-open flow, i.e. when a
valid ?token= AND ?session= are present in the URL; then the page draws that one
session immediately (and, for an edit, restores its strokes).  Otherwise the
page boots as a receiver."
  (let* ((proc (plist-get req :proc))
         (query (plist-get req :query))
         (q-session (org-pad--query-param query "session"))
         (q-token (org-pad--query-param query "token"))
         (authed (org-pad-token-valid-p q-token))
         (session (and authed q-session (org-pad--queue-find q-session)))
         (session-id (if session q-session ""))
         (token (if authed q-token ""))
         (mode (if session (symbol-name (org-pad-session-mode session)) "new"))
         (name (and session (or (org-pad-session-name session) "")))
         (drawing (and session (org-pad-session-drawing-bytes session)))
         ;; For a web edit the stored strokes ARE the JSON text (unibyte UTF-8).
         ;; The canvas restore() base64-decodes CONFIG.drawing (atob) — so inject
         ;; base64 of the raw web-JSON bytes, not the raw JSON text.
         (web-json (and drawing (base64-encode-string drawing t)))
         (background (org-pad--background-wire))
         ;; Absolute POST targets on the request's own Host so a browser reaching
         ;; us by IP/MagicDNS posts back to the same origin.
         (host (or (org-pad--header req "host")
                   (format "127.0.0.1:%d" org-pad-port)))
         (result-url (format "http://%s/result" host))
         (cancel-url (format "http://%s/cancel" host)))
    (org-pad--respond proc 200 "text/html; charset=utf-8"
                      (encode-coding-string
                       (org-pad--canvas-html session-id token mode background web-json
                                             :name name :result-url result-url
                                             :cancel-url cancel-url)
                       'utf-8)
                      ;; Never let the browser cache the canvas shell — otherwise
                      ;; an updated web/canvas.html is masked by a stale copy.
                      '(("Cache-Control" . "no-store, no-cache, must-revalidate")
                        ("Pragma" . "no-cache")))))

(defun org-pad--canvas-url (host session-id token)
  "Build the http://HOST/canvas?session=..&token=.. URL string.
SESSION-ID and TOKEN are URL-encoded into the query."
  (format "http://%s/canvas?session=%s&token=%s"
          host
          (url-hexify-string (or session-id ""))
          (url-hexify-string (or token ""))))

(defun org-pad--first-host ()
  "Return a \"IP:PORT\" reachable host for building client URLs.
Prefers the first non-loopback IPv4; falls back to 127.0.0.1."
  (let ((ip (car (org-pad--ipv4-addresses))))
    (format "%s:%d" (or ip "127.0.0.1") org-pad-port)))

;;;; ---------------------------------------------------------------------------
;;;; Route registration (re-register including the new endpoints).
;;;; ---------------------------------------------------------------------------

(defun org-pad--register-routes ()
  "Register all protocol + asset routes (idempotent)."
  (org-pad-route "POST" "/pair" #'org-pad--handle-pair)
  (org-pad-route "GET" "/session" #'org-pad--handle-session)
  (org-pad-route "POST" "/result" #'org-pad--handle-result)
  (org-pad-route "POST" "/cancel" #'org-pad--handle-cancel)
  (org-pad-route "GET" "/canvas" #'org-pad--handle-canvas)
  (org-pad-route "GET" "/web" #'org-pad--handle-canvas))

;;;; ---------------------------------------------------------------------------
;;;; org-pad-draw routing (opens the web canvas; edit reads the chunk FORMAT).
;;;; ---------------------------------------------------------------------------

(defun org-pad--web-receiver-urls ()
  "List of http://HOST:PORT/canvas receiver URLs; the IP-stable `.local' host first.
Open one ONCE on the iPad (or any browser); it pairs in-page and then long-polls
for drawings queued by `org-pad-draw'.  Prefer the `.local' URL — it survives
the Mac's IP changing, so you never have to re-open a new address."
  (mapcar (lambda (h) (format "http://%s:%d/canvas" h org-pad-port))
          (org-pad--host-candidates)))

(defun org-pad--open-web-session (session-id &optional token)
  "Announce that web SESSION-ID was queued for the receiver.
By default this does NOT open a browser on the Emacs host — a receiver tab kept
open on the iPad long-polls and picks the session up.  When
`org-pad-web-open-function' is non-nil (opt-in), ALSO open the per-session URL
on this machine using TOKEN (or the first token on file when TOKEN is nil)."
  (when (and org-pad-web-open-function (functionp org-pad-web-open-function))
    (let ((tok (or token (car (last (org-pad--load-tokens))))))
      (when (and (stringp tok) (not (string-empty-p tok)))
        (funcall org-pad-web-open-function
                 (org-pad--canvas-url (org-pad--first-host) session-id tok)))))
  (let ((receiver (car (org-pad--web-receiver-urls))))
    (message "org-pad: web drawing queued%s"
             (if receiver
                 (format " — draw it in your OrgPad browser tab (open %s on the iPad if none)"
                         receiver)
               " — open a /canvas tab to receive it")))
  session-id)

;;;###autoload
(defun org-pad-draw ()
  "Draw into the org buffer at point.  On an org-pad figure link, re-edit it.
Both new figures and edits open the web canvas at GET /canvas: a receiver tab
you keep open long-polls and picks up the request (or set
`org-pad-web-open-function' to also open it on the machine running Emacs)."
  (interactive)
  (org-pad-server-start)
  (let ((dwim (org-pad-dwim-at-point)))
    (pcase dwim
      (`(:edit ,file ,drawing)
       ;; DRAWING may be bare bytes (v1 dwim) or (FORMAT . BYTES) (v2 dwim).
       (let* ((pair (org-pad--file-drawing file))
              (format (if pair (car pair) org-pad-format-web))
              (bytes (if pair (cdr pair) drawing))
              (id (org-pad-generate-id)))
         (org-pad--session-set-format id format)
         (org-pad-enqueue (org-pad-session--make
                           :id id :mode 'edit
                           :name (file-name-nondirectory file)
                           :file file :drawing-bytes bytes))
         (org-pad--wake-waiters)
         (org-pad--open-web-session id nil)))
      (`(:new ,marker)
       (let ((id (org-pad-generate-id)))
         (org-pad--session-set-format id org-pad-format-web)
         (org-pad-enqueue (org-pad-session--make
                           :id id :mode 'new
                           :name (funcall org-pad-file-name-function)
                           :marker marker))
         (org-pad--wake-waiters)
         (org-pad--open-web-session id nil))))))

;;;; ---------------------------------------------------------------------------
;;;; 5. Transient menu
;;;; ---------------------------------------------------------------------------

(defun org-pad-set-background (value)
  "Set `org-pad-figure-background' to VALUE interactively."
  (interactive
   (list (let ((choice (completing-read
                        "Figure background: "
                        '("transparent" "white" "dark" "custom colour") nil t)))
           (pcase choice
             ("transparent" 'transparent)
             ("white" 'white)
             ("dark" 'dark)
             (_ (read-string "Hex/CSS colour: " "#1e1e2e"))))))
  (setq org-pad-figure-background value)
  (message "org-pad: figure background is now %s" (org-pad--background-wire)))

(defun org-pad-server-toggle ()
  "Start the server if stopped, else stop it."
  (interactive)
  (if (process-live-p org-pad--server-process)
      (org-pad-server-stop)
    (org-pad-server-start)))

;;;###autoload (autoload 'org-pad-menu "org-pad" nil t)
(transient-define-prefix org-pad-menu ()
  "OrgPad command dispatcher."
  [["Draw"
    ("d" "Draw / edit at point" org-pad-draw)
    ("e" "Edit figure at point" org-pad-edit)]
   ["Setup"
    ("s" "Setup + pair" org-pad-setup)
    ("S" "Toggle server" org-pad-server-toggle)]
   ["Config"
    ("b" "Set figure background" org-pad-set-background
     :transient t)]])

(provide 'org-pad)
;;; org-pad.el ends here
