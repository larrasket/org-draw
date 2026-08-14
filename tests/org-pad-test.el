;;; org-pad-test.el --- ERT tests for org-pad  -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -L . -l tests/org-pad-test.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'org-pad)
(require 'url-util)

(defvar org-pad-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this test file and its fixtures/.")

(defun org-pad-test--read-unibyte (path)
  "Read PATH as a unibyte byte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(ert-deftest org-pad-scaffold-loads ()
  "The package loads and defcustoms exist."
  (should (boundp 'org-pad-port))
  (should (= org-pad-port 8777)))

;;;; PNG: CRC32 + u32
(ert-deftest org-pad-crc32-canonical () (should (= (org-pad--crc32 "123456789") #xCBF43926)))
(ert-deftest org-pad-crc32-empty () (should (= (org-pad--crc32 "") 0)))
(ert-deftest org-pad-crc32-iend () (should (= (org-pad--crc32 "IEND") #xAE426082)))
(ert-deftest org-pad-crc32-all-bytes ()
  (should (= (org-pad--crc32 (apply #'unibyte-string (number-sequence 0 255))) #x29058C73)))
(ert-deftest org-pad-u32-roundtrip ()
  (dolist (n (list 0 1 255 256 65535 65536 16777215 #xFFFFFFFF #x0A1B2C3D))
    (should (= (org-pad--u32-decode (org-pad--u32-encode n) 0) n))))
(ert-deftest org-pad-u32-big-endian ()
  (should (string= (org-pad--u32-encode #x0A1B2C3D) (unibyte-string #x0A #x1B #x2C #x3D))))

;;;; PNG: chunks / embed / extract
(defun org-pad-test--fixture-png () (org-pad-test--read-unibyte (expand-file-name "fixtures/fixture.png" org-pad-test-dir)))
(defun org-pad-test--fixture-drawing () (org-pad-test--read-unibyte (expand-file-name "fixtures/fixture.drawing" org-pad-test-dir)))

(ert-deftest org-pad-chunks-fixture ()
  (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-pad--png-chunks (org-pad-test--fixture-png)))
                 '("IHDR" "IDAT" "IEND"))))
(ert-deftest org-pad-chunks-bad-signature () (should-error (org-pad--png-chunks "not a png at all!!")))
(ert-deftest org-pad-chunks-truncated ()
  (let ((png (org-pad-test--fixture-png)))
    (should-error (org-pad--png-chunks (substring png 0 (- (length png) 3))))))
(ert-deftest org-pad-embed-extract-roundtrip ()
  (let* ((png (org-pad-test--fixture-png)) (drawing (org-pad-test--fixture-drawing))
         (embedded (org-pad--png-embed png drawing)))
    (should-not (multibyte-string-p embedded))
    (should (string= (org-pad--png-extract-bytes embedded) drawing))))
(ert-deftest org-pad-embed-orpd-before-iend ()
  (let* ((embedded (org-pad--png-embed (org-pad-test--fixture-png) (org-pad-test--fixture-drawing))))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-pad--png-chunks embedded))
                   '("IHDR" "IDAT" "orPd" "IEND")))))
(ert-deftest org-pad-embed-crc-valid ()
  (let ((embedded (org-pad--png-embed (org-pad-test--fixture-png) (org-pad-test--fixture-drawing))))
    (dolist (c (org-pad--png-chunks embedded))
      (let* ((ds (plist-get c :data-start)) (dl (plist-get c :data-len))
             (data (substring embedded ds (+ ds dl))) (stored (org-pad--u32-decode embedded (+ ds dl))))
        (should (= stored (org-pad--crc32 (concat (plist-get c :type) data))))))))
(ert-deftest org-pad-extract-no-orpd-nil () (should (null (org-pad--png-extract (org-pad-test--fixture-png)))))
(ert-deftest org-pad-embed-empty-drawing ()
  (let ((got (org-pad--png-extract-bytes (org-pad--png-embed (org-pad-test--fixture-png) ""))))
    (should (stringp got)) (should (string= got ""))))
(ert-deftest org-pad-embed-idempotent ()
  (let* ((png (org-pad-test--fixture-png)) (d1 (org-pad-test--fixture-drawing))
         (once (org-pad--png-embed png d1)) (d2 (concat d1 (unibyte-string #x42 #x42)))
         (twice (org-pad--png-embed once d2)))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-pad--png-chunks twice))
                   '("IHDR" "IDAT" "orPd" "IEND")))
    (should (string= (org-pad--png-extract-bytes twice) d2))))

;;;; HTTP: response writer
(ert-deftest org-pad-respond-shape ()
  "org-pad--respond writes a well-formed unibyte HTTP response."
  (let* ((captured "")
         (proc (make-pipe-process :name "op-cap" :noquery t
                                  :filter (lambda (_p s) (setq captured (concat captured s))))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_p s) (setq captured (concat captured s))))
                    ((symbol-function 'org-pad--safe-delete) #'ignore)
                    ((symbol-function 'process-live-p) (lambda (_p) t)))
            (org-pad--respond proc 200 "application/json" (unibyte-string ?{ ?} ) nil t))
          (should (string-prefix-p "HTTP/1.1 200 OK\r\n" captured))
          (should (string-match-p "Content-Type: application/json\r\n" captured))
          (should (string-match-p "Content-Length: 2\r\n" captured))
          (should (string-match-p "Connection: keep-alive\r\n" captured))
          (should (string-suffix-p "\r\n\r\n{}" captured))
          (should-not (multibyte-string-p captured)))
      (ignore-errors (delete-process proc)))))

;;;; HTTP: parsing + routing
(ert-deftest org-pad-parse-request-line ()
  (should (equal (org-pad--parse-request-line "POST /result?x=1 HTTP/1.1") '("POST" "/result" "x=1" "HTTP/1.1")))
  (should (equal (org-pad--parse-request-line "GET /session HTTP/1.1") '("GET" "/session" nil "HTTP/1.1")))
  (should (null (org-pad--parse-request-line "GARBAGE"))))
(ert-deftest org-pad-parse-headers ()
  (let ((h (org-pad--parse-headers "Content-Length: 5\r\nX-OrgPad-Token: abc ")))
    (should (equal (cdr (assoc "content-length" h)) "5"))
    (should (equal (cdr (assoc "x-orgpad-token" h)) "abc"))))
(ert-deftest org-pad-header-ci ()
  (should (equal (org-pad--header '(:headers (("x-orgpad-token" . "t"))) "X-OrgPad-Token") "t")))
(ert-deftest org-pad-routing ()
  (let ((org-pad--routes nil) (hit nil))
    (org-pad-route "GET" "/x" (lambda (_r) (setq hit t)))
    ;; Capture the numeric STATUS, which is the 2nd arg to org-pad--respond.
    (cl-letf (((symbol-function 'org-pad--respond) (lambda (&rest a) (setq hit (nth 1 a)))))
      (org-pad--dispatch (list :proc nil :method "GET" :path "/x"))
      (should (eq hit t))                                             ; handler ran (sets hit)
      (org-pad--dispatch (list :proc nil :method "POST" :path "/x"))  ; wrong method -> 405
      (should (equal hit 405))
      (org-pad--dispatch (list :proc nil :method "GET" :path "/nope")) ; unknown -> 404
      (should (equal hit 404)))))

;;;; HTTP: filter state machine + long-poll
(defmacro org-pad-test--with-captured-dispatch (var &rest body)
  "Run BODY with org-pad--dispatch capturing the request plist into VAR."
  (declare (indent 1))
  ;; NOTE: the stub lambdas' own parameter names are deliberately distinct from
  ;; any plausible caller VAR (e.g. `req') to avoid the lambda parameter
  ;; shadowing the `,var' splice -- `(lambda (req) (setq req req))' would be a
  ;; silent no-op if VAR were also named `req'.
  `(let ((,var nil))
     (cl-letf (((symbol-function 'org-pad--dispatch)
                (lambda (org-pad-test--captured-req) (setq ,var org-pad-test--captured-req)))
               ((symbol-function 'org-pad--respond)
                (lambda (&rest org-pad-test--captured-args) (setq ,var (cons 'response org-pad-test--captured-args)))))
       ,@body)))

(ert-deftest org-pad-filter-whole-request ()
  (let ((proc (make-pipe-process :name "op-f1" :noquery t)))
    (unwind-protect
        (org-pad-test--with-captured-dispatch req
          (org-pad--reset-conn-state proc)
          (org-pad--filter proc "POST /result HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello")
          (should (equal (plist-get req :method) "POST"))
          (should (equal (plist-get req :path) "/result"))
          (should (equal (plist-get req :body) "hello")))
      (ignore-errors (delete-process proc)))))

(ert-deftest org-pad-filter-fragmented ()
  "A request dribbled one byte at a time reassembles byte-exact."
  (let ((proc (make-pipe-process :name "op-f2" :noquery t))
        (raw "POST /r HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc"))
    (unwind-protect
        (org-pad-test--with-captured-dispatch req
          (org-pad--reset-conn-state proc)
          (dotimes (i (length raw)) (org-pad--filter proc (substring raw i (1+ i))))
          (should (equal (plist-get req :body) "abc")))
      (ignore-errors (delete-process proc)))))

(ert-deftest org-pad-filter-413 ()
  (let ((proc (make-pipe-process :name "op-f3" :noquery t)))
    (unwind-protect
        (org-pad-test--with-captured-dispatch req
          (org-pad--reset-conn-state proc)
          (org-pad--filter proc (format "POST /r HTTP/1.1\r\nContent-Length: %d\r\n\r\n" (1+ org-pad--max-body)))
          (should (equal req (list 'response proc 413 "text/plain" "Payload Too Large"))))
      (ignore-errors (delete-process proc)))))

(ert-deftest org-pad-filter-400-bad-clen ()
  (let ((proc (make-pipe-process :name "op-f4" :noquery t)))
    (unwind-protect
        (org-pad-test--with-captured-dispatch req
          (org-pad--reset-conn-state proc)
          (org-pad--filter proc "POST /r HTTP/1.1\r\nContent-Length: abc\r\n\r\n")
          (should (equal (nth 2 req) 400)))
      (ignore-errors (delete-process proc)))))

;;;; HTTP: live server integration
(ert-deftest org-pad-server-roundtrip ()
  "Start a real server, hit /ping over TCP, assert a 200 body, then stop."
  (let ((org-pad--routes nil) (org-pad-port 18799) (org-pad--server-process nil))
    (org-pad-route "GET" "/ping" (lambda (req) (org-pad--respond (plist-get req :proc) 200 "text/plain" "pong")))
    (org-pad--server-start 18799)
    (unwind-protect
        (let ((buf "")
              (client (make-network-process :name "op-client" :host "127.0.0.1" :service 18799
                                            :coding 'binary :nowait nil)))
          (set-process-filter client (lambda (_p s) (setq buf (concat buf s))))
          (process-send-string client "GET /ping HTTP/1.1\r\nHost: x\r\n\r\n")
          (let ((deadline (+ (float-time) 3)))
            (while (and (< (float-time) deadline) (not (string-match-p "pong" buf)))
              (accept-process-output client 0.1)))
          (should (string-match-p "\\`HTTP/1.1 200 OK" buf))
          (should (string-match-p "pong\\'" buf))
          (ignore-errors (delete-process client)))
      (org-pad--server-stop))))

;;;; Sessions: queue
(defun org-pad-test--sess (id) (org-pad-session--make :id id :mode 'new))
(ert-deftest org-pad-queue-fifo ()
  (org-pad--queue-reset)
  (mapc (lambda (id) (org-pad-enqueue (org-pad-test--sess id))) '("a" "b" "c"))
  (should (equal (org-pad-session-id (org-pad-queue-head)) "a"))
  (should (= (org-pad-queue-length) 3))
  (should (equal (org-pad-session-id (org-pad-queue-head)) "a")) ; non-destructive
  (org-pad-queue-complete "a")
  (should (equal (org-pad-session-id (org-pad-queue-head)) "b")))
(ert-deftest org-pad-queue-cancel ()
  (org-pad--queue-reset)
  (mapc (lambda (id) (org-pad-enqueue (org-pad-test--sess id))) '("a" "b" "c"))
  (should (org-pad-queue-cancel "b"))
  (should (equal (mapcar #'org-pad-session-id (list (org-pad-queue-head))) '("a")))
  (should (= (org-pad-queue-length) 2))
  (should (null (org-pad-queue-cancel "zz"))))

;;;; Sessions: id/token + pairing + auth
(ert-deftest org-pad-id-shape ()
  (let ((id (org-pad-generate-id)))
    (should (= (length id) 32)) (should (string-match-p "\\`[0-9a-f]+\\'" id))
    (should-not (equal id (org-pad-generate-id)))))
(ert-deftest org-pad-pairing-cap ()
  (org-pad-pairing-start)
  (setf (org-pad-pairing-code org-pad--pairing) "000000") ; deterministic
  (should (equal (org-pad-pairing-verify "111111") '(:bad . 4)))
  (should (equal (org-pad-pairing-verify "111111") '(:bad . 3)))
  (should (equal (org-pad-pairing-verify "111111") '(:bad . 2)))
  (should (equal (org-pad-pairing-verify "111111") '(:bad . 1)))
  (should (eq (org-pad-pairing-verify "111111") :closed))
  (should (eq (org-pad-pairing-verify "000000") :closed))) ; closed stays closed
(ert-deftest org-pad-pairing-success-and-auth ()
  (let ((org-pad-token-file (make-temp-file "org-pad-tok")))
    (unwind-protect
        (progn
          (org-pad-pairing-start)
          (setf (org-pad-pairing-code org-pad--pairing) "424242")
          (let ((r (org-pad-pairing-verify "424242")))
            (should (eq (car r) :ok))
            (should (org-pad-token-valid-p (cdr r)))
            (should-not (org-pad-token-valid-p "deadbeef"))
            (should-not (org-pad-token-valid-p ""))
            (should (eq (org-pad-pairing-verify "424242") :closed)))) ; pairing closed after success
      (delete-file org-pad-token-file))))

;;;; Org integration: DWIM + result handling
(ert-deftest org-pad-dwim-new-vs-edit ()
  (let* ((dir (make-temp-file "org-pad-t" t))
         (org (expand-file-name "n.org" dir))
         (foreign (expand-file-name "foreign.png" dir))
         (fig (expand-file-name "fig.png" dir)))
    (unwind-protect
        (progn
          ;; a foreign png (no orPd) and an org-pad png (with orPd)
          (with-temp-buffer (set-buffer-multibyte nil) (insert (org-pad-test--fixture-png))
                            (let ((coding-system-for-write 'binary)) (write-region nil nil foreign nil 'silent)))
          (org-pad--write-png fig (org-pad-test--fixture-png) (org-pad-test--fixture-drawing))
          (with-temp-file org (insert "* head\n[[file:foreign.png]]\n[[file:fig.png]]\nplain\n"))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min)) (search-forward "plain")
                  (should (eq (car (org-pad-dwim-at-point)) :new))
                  (goto-char (point-min)) (search-forward "fig.png") (backward-char 3)
                  (let ((d (org-pad-dwim-at-point)))
                    (should (eq (car d) :edit))
                    (should (string= (nth 2 d) (org-pad-test--fixture-drawing))))
                  (goto-char (point-min)) (search-forward "foreign.png") (backward-char 3)
                  (should (eq (car (org-pad-dwim-at-point)) :new)))
              (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest org-pad-insert-new-figure ()
  (let* ((dir (make-temp-file "org-pad-i" t)) (org (expand-file-name "n.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file org (insert "* head\n"))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-max))
                  (let* ((marker (org-pad--make-insertion-marker))
                         (sess (org-pad-session--make :id "s1" :mode 'new :name "fig-x.png" :marker marker)))
                    (org-pad-insert-new-figure sess (org-pad-test--fixture-png) (org-pad-test--fixture-drawing))
                    (should (string-match-p "\\[\\[file:figures/fig-x.png\\]\\]" (buffer-string)))
                    (should (string= (org-pad--png-extract-bytes
                                      (org-pad-test--read-unibyte (expand-file-name "figures/fig-x.png" dir)))
                                     (org-pad-test--fixture-drawing)))))
              (kill-buffer buf))))
      (delete-directory dir t))))

;;;; Protocol: json + auth
(ert-deftest org-pad-session-json-new ()
  (let* ((s (org-pad-session--make :id "s1" :mode 'new :name "fig.png"))
         (j (json-parse-string (org-pad--session-json s))))
    (should (equal (gethash "session_id" j) "s1"))
    (should (equal (gethash "mode" j) "new"))
    (should (eq (gethash "drawing" j) :null))))
(ert-deftest org-pad-session-json-edit ()
  (let* ((s (org-pad-session--make :id "s2" :mode 'edit :name "fig.png"
                                   :drawing-bytes (unibyte-string 1 2 3)))
         (j (json-parse-string (org-pad--session-json s))))
    (should (equal (gethash "mode" j) "edit"))
    (should (string= (base64-decode-string (gethash "drawing" j)) (unibyte-string 1 2 3)))))
(ert-deftest org-pad-require-token ()
  (let ((org-pad-token-file (make-temp-file "opt")) (sent nil))
    (unwind-protect
        (progn
          (org-pad--persist-token "good")
          (cl-letf (((symbol-function 'org-pad--respond) (lambda (&rest a) (setq sent (nth 1 a)))))
            (should (org-pad--require-token '(:headers (("x-orgpad-token" . "good")))))
            (should (null (org-pad--require-token '(:headers (("x-orgpad-token" . "bad"))))))
            (should (= sent 401))))
      (delete-file org-pad-token-file))))

;;;; Protocol: endpoint handlers
(ert-deftest org-pad-handle-pair ()
  (let ((org-pad-token-file (make-temp-file "opt")) (resp nil))
    (unwind-protect
        (progn
          (org-pad-pairing-start) (setf (org-pad-pairing-code org-pad--pairing) "424242")
          (cl-letf (((symbol-function 'org-pad--respond)
                     (lambda (&rest a) (setq resp a))))
            (org-pad--handle-pair (list :proc nil :body (json-serialize '(:code "424242"))))
            (should (= (nth 1 resp) 200))
            (let ((tok (gethash "token" (json-parse-string (nth 3 resp)))))
              (should (org-pad-token-valid-p tok)))))
      (delete-file org-pad-token-file))))

(ert-deftest org-pad-handle-session-immediate ()
  "A poll with a queued head answers immediately."
  (org-pad--queue-reset)
  (org-pad-enqueue (org-pad-session--make :id "s9" :mode 'new :name "f.png"))
  (let ((resp nil) (org-pad-token-file (make-temp-file "opt")))
    (unwind-protect
        (progn
          (org-pad--persist-token "good")
          (cl-letf (((symbol-function 'org-pad-answer) (lambda (&rest a) (setq resp a))))
            (org-pad--handle-session (list :proc 'P :headers '(("x-orgpad-token" . "good"))))
            (should (= (nth 1 resp) 200))
            (should (equal (gethash "session_id" (json-parse-string (nth 3 resp))) "s9"))))
      (delete-file org-pad-token-file))))

;;;; Infra: interfaces
(ert-deftest org-pad-ipv4-addresses ()
  (let ((addrs (org-pad--ipv4-addresses)))
    (should (listp addrs))
    (dolist (a addrs)
      (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" a))
      (should-not (string-prefix-p "127." a)))))
;;;; Commands
(ert-deftest org-pad-register-routes ()
  (let ((org-pad--routes nil))
    (org-pad--register-routes)
    (dolist (key '(("POST" . "/pair") ("GET" . "/session") ("POST" . "/result")
                   ("POST" . "/cancel") ("GET" . "/canvas") ("GET" . "/web")))
      (should (cdr (assoc key org-pad--routes))))))
(ert-deftest org-pad-draw-enqueues ()
  (let* ((dir (make-temp-file "opd" t)) (org (expand-file-name "n.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file org (insert "* h\nplain\n"))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (org-pad--queue-reset)
                  (goto-char (point-min)) (search-forward "plain")
                  (cl-letf (((symbol-function 'org-pad-server-start) #'ignore)
                            ((symbol-function 'org-pad--wake-waiters) #'ignore))
                    (org-pad-draw))
                  (should (= (org-pad-queue-length) 1))
                  (should (eq (org-pad-session-mode (org-pad-queue-head)) 'new)))
              (kill-buffer buf))))
      (delete-directory dir t))))

;;;; ======== v2 tests ========
(defvar orgpad-v2-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this test file and its fixtures/.")

(defun orgpad-v2-test--read-unibyte (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun orgpad-v2-test--png () (orgpad-v2-test--read-unibyte
                               (expand-file-name "fixtures/fixture.png" orgpad-v2-test-dir)))
(defun orgpad-v2-test--drawing () (orgpad-v2-test--read-unibyte
                                   (expand-file-name "fixtures/fixture.drawing" orgpad-v2-test-dir)))
(defun orgpad-v2-test--legacy () (orgpad-v2-test--read-unibyte
                                  (expand-file-name "fixtures/legacy-0x01.png" orgpad-v2-test-dir)))

;;;; ---------------------------------------------------------------------------
;;;; Chunk FORMAT byte
;;;; ---------------------------------------------------------------------------

(ert-deftest orgpad-v2-embed-default-is-pkdrawing ()
  "Two-arg embed defaults to format 0x01 -> extract returns (0x01 . bytes)."
  (let* ((png (orgpad-v2-test--png)) (d (orgpad-v2-test--drawing))
         (embedded (org-pad--png-embed png d))
         (pair (org-pad--png-extract embedded)))
    (should-not (multibyte-string-p embedded))
    (should (consp pair))
    (should (eql (car pair) org-pad-format-pkdrawing))
    (should (eql (car pair) #x01))
    (should (string= (cdr pair) d))))

(ert-deftest orgpad-v2-embed-web-format-roundtrip ()
  "Embed with 0x02 -> extract returns (0x02 . json-bytes) byte-exact."
  (let* ((png (orgpad-v2-test--png))
         (json "{\"v\":1,\"strokes\":[[[1,2,0.5],[3,4,0.6]]]}")
         (jbytes (encode-coding-string json 'utf-8))
         (embedded (org-pad--png-embed png jbytes org-pad-format-web))
         (pair (org-pad--png-extract embedded)))
    (should (eql (car pair) org-pad-format-web))
    (should (eql (car pair) #x02))
    (should (string= (cdr pair) jbytes))
    (should (string= (decode-coding-string (cdr pair) 'utf-8) json))))

(ert-deftest orgpad-v2-embed-byte-identical-to-v1 ()
  "A default (2-arg) v2 embed is byte-for-byte what an external v1 embed produced.
Compares against the independently-generated legacy-0x01.png fixture."
  (let* ((png (orgpad-v2-test--png)) (d (orgpad-v2-test--drawing))
         (v2out (org-pad--png-embed png d))          ; default format
         (legacy (orgpad-v2-test--legacy)))
    (should (string= v2out legacy))))

(ert-deftest orgpad-v2-backcompat-legacy-extract ()
  "A real legacy 0x01 figure (written by an external tool) still extracts."
  (let* ((legacy (orgpad-v2-test--legacy))
         (pair (org-pad--png-extract legacy)))
    (should (eql (car pair) #x01))
    (should (string= (cdr pair) (orgpad-v2-test--drawing)))
    ;; and the bytes-only compat shim reproduces the v1 return contract exactly.
    (should (string= (org-pad--png-extract-bytes legacy) (orgpad-v2-test--drawing)))))

(ert-deftest orgpad-v2-extract-shims ()
  "Bytes/format shims behave; foreign PNG -> nil for all extractors."
  (let* ((png (orgpad-v2-test--png))
         (embedded (org-pad--png-embed png "abc" org-pad-format-web)))
    (should (string= (org-pad--png-extract-bytes embedded) "abc"))
    (should (eql (org-pad--png-extract-format embedded) org-pad-format-web))
    ;; foreign PNG (no orPd)
    (should (null (org-pad--png-extract png)))
    (should (null (org-pad--png-extract-bytes png)))
    (should (null (org-pad--png-extract-format png)))))

(ert-deftest orgpad-v2-empty-drawing-still-distinct ()
  "An embedded-but-empty payload extracts to (FORMAT . \"\"), not nil."
  (let* ((png (orgpad-v2-test--png))
         (embedded (org-pad--png-embed png "" org-pad-format-web))
         (pair (org-pad--png-extract embedded)))
    (should (consp pair))
    (should (eql (car pair) org-pad-format-web))
    (should (stringp (cdr pair)))
    (should (string= (cdr pair) ""))))

(ert-deftest orgpad-v2-embed-replaces-existing-format ()
  "Re-embedding switches the format byte in place (0x01 figure -> 0x02)."
  (let* ((png (orgpad-v2-test--png))
         (once (org-pad--png-embed png "pk" org-pad-format-pkdrawing))
         (twice (org-pad--png-embed once "web" org-pad-format-web))
         (pair (org-pad--png-extract twice)))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-pad--png-chunks twice))
                   '("IHDR" "IDAT" "orPd" "IEND")))
    (should (eql (car pair) org-pad-format-web))
    (should (string= (cdr pair) "web"))))

(ert-deftest orgpad-v2-embed-crc-valid ()
  "Every chunk in a web-format embed has a self-consistent CRC."
  (let ((embedded (org-pad--png-embed (orgpad-v2-test--png) "web-json" org-pad-format-web)))
    (dolist (c (org-pad--png-chunks embedded))
      (let* ((ds (plist-get c :data-start)) (dl (plist-get c :data-len))
             (data (substring embedded ds (+ ds dl)))
             (stored (org-pad--u32-decode embedded (+ ds dl))))
        (should (= stored (org-pad--crc32 (concat (plist-get c :type) data))))))))

(ert-deftest orgpad-v2-format-helpers ()
  (should (org-pad-format-valid-p #x01))
  (should (org-pad-format-valid-p #x02))
  (should-not (org-pad-format-valid-p #x03)))

;;;; ---------------------------------------------------------------------------
;;;; File-level (FORMAT . BYTES) reader + write round-trip
;;;; ---------------------------------------------------------------------------

(ert-deftest orgpad-v2-write-read-web-file ()
  "org-pad--write-png with web format writes a re-editable web figure."
  (let* ((dir (make-temp-file "opv2" t))
         (file (expand-file-name "f.png" dir)))
    (unwind-protect
        (progn
          (org-pad--write-png file (orgpad-v2-test--png) "webjson" org-pad-format-web)
          (let ((pair (org-pad--file-drawing file)))
            (should (eql (car pair) org-pad-format-web))
            (should (string= (cdr pair) "webjson")))
          (should (string= (org-pad--file-has-drawing-p file) "webjson")))
      (delete-directory dir t))))

(ert-deftest orgpad-v2-write-default-file-is-pkdrawing ()
  "Three-arg write (no format) defaults to 0x01 for back-compat."
  (let* ((dir (make-temp-file "opv2" t))
         (file (expand-file-name "f.png" dir)))
    (unwind-protect
        (progn
          (org-pad--write-png file (orgpad-v2-test--png) (orgpad-v2-test--drawing))
          (let ((pair (org-pad--file-drawing file)))
            (should (eql (car pair) org-pad-format-pkdrawing))
            (should (string= (cdr pair) (orgpad-v2-test--drawing)))))
      (delete-directory dir t))))

;;;; ---------------------------------------------------------------------------
;;;; Background field in session JSON
;;;; ---------------------------------------------------------------------------

(ert-deftest orgpad-v2-session-json-background-transparent ()
  (let* ((org-pad-figure-background 'transparent)
         (s (org-pad-session--make :id "s1" :mode 'new :name "f.png"))
         (j (json-parse-string (org-pad--session-json s))))
    (should (equal (gethash "background" j) "transparent"))
    (should (equal (gethash "session_id" j) "s1"))
    (should (equal (gethash "mode" j) "new"))
    (should (eq (gethash "drawing" j) :null))
    (should (equal (gethash "format" j) "pkdrawing"))))

(ert-deftest orgpad-v2-session-json-background-variants ()
  ;; `white' maps to the wire word "light" (matches the web canvas + Swift enum).
  (dolist (case '((white . "light") (dark . "dark") ("#1e1e2e" . "#1e1e2e")))
    (let* ((org-pad-figure-background (car case))
           (s (org-pad-session--make :id "s" :mode 'new :name "f"))
           (j (json-parse-string (org-pad--session-json s))))
      (should (equal (gethash "background" j) (cdr case))))))

(ert-deftest orgpad-v2-session-json-format-web ()
  "When a session's recorded target format is web, JSON \"format\" is \"web\"."
  (org-pad--session-set-format "sweb" org-pad-format-web)
  (unwind-protect
      (let* ((s (org-pad-session--make :id "sweb" :mode 'new :name "f"))
             (j (json-parse-string (org-pad--session-json s))))
        (should (equal (gethash "format" j) "web")))
    (remhash "sweb" org-pad--session-format)))

(ert-deftest orgpad-v2-session-json-edit-drawing ()
  "Edit-mode session still base64-encodes its stroke bytes into \"drawing\"."
  (let* ((s (org-pad-session--make :id "se" :mode 'edit :name "f.png"
                                   :drawing-bytes (unibyte-string 1 2 3)))
         (j (json-parse-string (org-pad--session-json s))))
    (should (equal (gethash "mode" j) "edit"))
    (should (string= (base64-decode-string (gethash "drawing" j))
                     (unibyte-string 1 2 3)))))

;;;; ---------------------------------------------------------------------------
;;;; Client routing by format (draw DWIM)
;;;; ---------------------------------------------------------------------------

(defmacro orgpad-v2-test--with-stubbed-draw (&rest body)
  "Run BODY with server start, wake, and web-open stubbed; capture opened URL.
Binds `opened' (URL passed to the web opener) and `msgs' (messages)."
  `(let ((opened nil) (msgs '()))
     (cl-letf (((symbol-function 'org-pad-server-start) #'ignore)
               ((symbol-function 'org-pad--wake-waiters) #'ignore)
               ((symbol-function 'org-pad--open-web-session)
                (lambda (id _token) (setq opened id) (format "url:%s" id)))
               ((symbol-function 'message)
                (lambda (fmt &rest a) (push (apply #'format fmt a) msgs) nil)))
       ,@body)))

(ert-deftest orgpad-v2-draw-new-web ()
  "A new figure enqueues a 0x02 session and opens the web canvas."
  (org-pad--queue-reset)
  (clrhash org-pad--session-format)
  (orgpad-v2-test--with-stubbed-draw
   (cl-letf (((symbol-function 'org-pad-dwim-at-point)
              (lambda () (list :new (copy-marker (point-min))))))
     (org-pad-draw)
     (let ((head (org-pad-queue-head)))
       (should (eq (org-pad-session-mode head) 'new))
       (should (equal opened (org-pad-session-id head)))
       (should (eql (org-pad--session-get-format (org-pad-session-id head))
                    org-pad-format-web))))))

(ert-deftest orgpad-v2-draw-edit-opens-web ()
  "Editing a web figure opens the web canvas; the session records its format."
  (let* ((dir (make-temp-file "opv2edit" t))
         (web-file (expand-file-name "web.png" dir)))
    (unwind-protect
        (progn
          (org-pad--write-png web-file (orgpad-v2-test--png)
                              "webjson" org-pad-format-web)
          (org-pad--queue-reset) (clrhash org-pad--session-format)
          (orgpad-v2-test--with-stubbed-draw
           (cl-letf (((symbol-function 'org-pad-dwim-at-point)
                      (lambda () (list :edit web-file
                                       (encode-coding-string "webjson" 'utf-8)))))
             (org-pad-draw)
             (should (equal opened (org-pad-session-id (org-pad-queue-head))))
             (should (eql (org-pad--session-get-format
                           (org-pad-session-id (org-pad-queue-head)))
                          org-pad-format-web)))))
      (delete-directory dir t))))

;;;; ---------------------------------------------------------------------------
;;;; /result web-format handling
;;;; ---------------------------------------------------------------------------

(defun orgpad-v2-test--result-body (id png-b64 drawing-b64 &optional format)
  (let ((h (make-hash-table :test 'equal)))
    (puthash "session_id" id h)
    (puthash "png" png-b64 h)
    (puthash "drawing" drawing-b64 h)
    (when format (puthash "format" format h))
    h))

(ert-deftest orgpad-v2-result-format-reader ()
  (should (eql (org-pad--result-format (orgpad-v2-test--result-body "i" "" "" "web"))
               org-pad-format-web))
  (should (eql (org-pad--result-format (orgpad-v2-test--result-body "i" "" "" "pkdrawing"))
               org-pad-format-pkdrawing))
  (should (eql (org-pad--result-format (orgpad-v2-test--result-body "i" "" "" nil))
               org-pad-format-pkdrawing)))

(ert-deftest orgpad-v2-handle-result-web-embeds-0x02 ()
  "POST /result with format:web overwrites an edit figure with a 0x02 chunk."
  (let* ((dir (make-temp-file "opv2res" t))
         (file (expand-file-name "f.png" dir))
         (tokfile (make-temp-file "opv2tok"))
         (png-b64 (base64-encode-string (orgpad-v2-test--png) t))
         (json "{\"v\":1}")
         (drawing-b64 (base64-encode-string (encode-coding-string json 'utf-8) t))
         (resp nil))
    (unwind-protect
        (let ((org-pad-token-file tokfile))
          (org-pad--persist-token "tok")
          ;; existing figure so overwrite has a target
          (org-pad--write-png file (orgpad-v2-test--png) "old" org-pad-format-web)
          (org-pad--queue-reset) (clrhash org-pad--session-format)
          (org-pad-enqueue (org-pad-session--make :id "r1" :mode 'edit
                                                  :name "f.png" :file file))
          (cl-letf (((symbol-function 'org-pad--respond)
                     (lambda (&rest a) (setq resp a)))
                    ((symbol-function 'org-pad--refresh-inline-images) #'ignore)
                    ((symbol-function 'org-pad--wake-waiters) #'ignore))
            (org-pad--handle-result
             (list :proc 'P
                   :headers '(("x-orgpad-token" . "tok"))
                   :body (json-serialize
                          (list :session_id "r1" :png png-b64
                                :drawing drawing-b64 :format "web")))))
          (should (= (nth 1 resp) 200))
          ;; The file now carries a 0x02 chunk whose payload is the JSON.
          (let ((pair (org-pad--file-drawing file)))
            (should (eql (car pair) org-pad-format-web))
            (should (string= (decode-coding-string (cdr pair) 'utf-8) json))))
      (ignore-errors (delete-file tokfile))
      (delete-directory dir t))))

(ert-deftest orgpad-v2-handle-result-default-embeds-0x01 ()
  "POST /result with no format field embeds 0x01 (byte-compat with v1 clients)."
  (let* ((dir (make-temp-file "opv2res" t))
         (file (expand-file-name "f.png" dir))
         (tokfile (make-temp-file "opv2tok"))
         (png-b64 (base64-encode-string (orgpad-v2-test--png) t))
         (drawing-b64 (base64-encode-string (orgpad-v2-test--drawing) t))
         (resp nil))
    (unwind-protect
        (let ((org-pad-token-file tokfile))
          (org-pad--persist-token "tok")
          (org-pad--write-png file (orgpad-v2-test--png) "old" org-pad-format-pkdrawing)
          (org-pad--queue-reset) (clrhash org-pad--session-format)
          (org-pad-enqueue (org-pad-session--make :id "r2" :mode 'edit
                                                  :name "f.png" :file file))
          (cl-letf (((symbol-function 'org-pad--respond)
                     (lambda (&rest a) (setq resp a)))
                    ((symbol-function 'org-pad--refresh-inline-images) #'ignore)
                    ((symbol-function 'org-pad--wake-waiters) #'ignore))
            (org-pad--handle-result
             (list :proc 'P
                   :headers '(("x-orgpad-token" . "tok"))
                   :body (json-serialize
                          (list :session_id "r2" :png png-b64 :drawing drawing-b64)))))
          (should (= (nth 1 resp) 200))
          (let ((pair (org-pad--file-drawing file)))
            (should (eql (car pair) org-pad-format-pkdrawing))
            (should (string= (cdr pair) (orgpad-v2-test--drawing)))))
      (ignore-errors (delete-file tokfile))
      (delete-directory dir t))))

;;;; ---------------------------------------------------------------------------
;;;; /canvas endpoint
;;;; ---------------------------------------------------------------------------

(ert-deftest orgpad-v2-query-param ()
  (should (equal (org-pad--query-param "session=abc&token=xyz" "session") "abc"))
  (should (equal (org-pad--query-param "session=abc&token=xyz" "token") "xyz"))
  (should (equal (org-pad--query-param "a=1&b=hello%20world" "b") "hello world"))
  (should (null (org-pad--query-param "a=1" "missing")))
  (should (null (org-pad--query-param nil "x"))))

(ert-deftest orgpad-v2-canvas-config-shape ()
  (let* ((block (org-pad--canvas-config "sid" "tok" "new" "transparent" nil)))
    (should (string-prefix-p "<script>window.ORGPAD_CONFIG=" block))
    (should (string-suffix-p ";</script>" block))
    (let* ((json (substring block (length "<script>window.ORGPAD_CONFIG=")
                            (- (length block) (length ";</script>"))))
           (h (json-parse-string json)))
      (should (equal (gethash "session_id" h) "sid"))
      (should (equal (gethash "token" h) "tok"))
      (should (equal (gethash "mode" h) "new"))
      (should (equal (gethash "background" h) "transparent"))
      (should (equal (gethash "result_path" h) "/result"))
      ;; camelCase resultUrl is the key the shipped canvas.html actually reads
      (should (equal (gethash "resultUrl" h) "/result"))
      (should (equal (gethash "name" h) ""))
      (should (equal (gethash "token_header" h) "X-OrgPad-Token"))
      (should (equal (gethash "format" h) "web"))
      (should (eq (gethash "drawing" h) :null)))))

(ert-deftest orgpad-v2-canvas-config-absolute-urls ()
  "resultUrl/cancel_path can be absolute (built from the request Host)."
  (let* ((block (org-pad--canvas-config
                 "s" "t" "edit" "transparent" "{\"v\":1}"
                 :name "fig.png"
                 :result-url "http://192.168.1.5:8777/result"
                 :cancel-url "http://192.168.1.5:8777/cancel"))
         (json (substring block (length "<script>window.ORGPAD_CONFIG=")
                          (- (length block) (length ";</script>"))))
         (h (json-parse-string json)))
    (should (equal (gethash "resultUrl" h) "http://192.168.1.5:8777/result"))
    (should (equal (gethash "cancel_path" h) "http://192.168.1.5:8777/cancel"))
    (should (equal (gethash "name" h) "fig.png"))
    (should (equal (gethash "drawing" h) "{\"v\":1}"))))

(ert-deftest orgpad-v2-canvas-html-injects-before-head ()
  (let ((org-pad-web-canvas-file "/nonexistent/canvas.html"))  ; force fallback
    (let ((html (org-pad--canvas-html "s" "t" "new" "transparent" nil)))
      (should (string-search "window.ORGPAD_CONFIG" html))
      ;; config appears before </head> when the template has one (fallback does).
      (let ((cfg (string-search "window.ORGPAD_CONFIG" html))
            (head (string-search "</head>" html)))
        (should (and cfg head (< cfg head)))))))

(defun org-pad-test--canvas-config (html)
  "Extract + parse the window.ORGPAD_CONFIG JSON object from HTML."
  (let* ((start (string-search "window.ORGPAD_CONFIG=" html))
         (rest (substring html (+ start (length "window.ORGPAD_CONFIG="))))
         (semi (string-search ";</script>" rest)))
    (json-parse-string (substring rest 0 semi))))

(ert-deftest orgpad-v2-handle-canvas-auth ()
  "The /canvas page is served unconditionally (receiver mode); a SPECIFIC
session is injected only with a valid token AND a queued session."
  (let* ((tokfile (make-temp-file "opv2tok")) (resp nil))
    (unwind-protect
        (let ((org-pad-token-file tokfile)
              (org-pad-web-canvas-file "/nonexistent/canvas.html"))
          (org-pad--persist-token "good")
          (org-pad--queue-reset) (clrhash org-pad--session-format)
          (org-pad-enqueue (org-pad-session--make :id "s1" :mode 'new :name "f.png"))
          (cl-letf (((symbol-function 'org-pad--respond)
                     (lambda (&rest a) (setq resp a))))
            ;; no token -> 200 receiver page, no session/token injected
            (org-pad--handle-canvas (list :proc 'P :query "session=s1"))
            (should (= (nth 1 resp) 200))
            (let ((cfg (org-pad-test--canvas-config (nth 3 resp))))
              (should (equal (gethash "session_id" cfg) ""))
              (should (equal (gethash "token" cfg) "")))
            ;; bad token -> still a receiver page, nothing injected
            (org-pad--handle-canvas (list :proc 'P :query "session=s1&token=bad"))
            (should (= (nth 1 resp) 200))
            (should (equal (gethash "session_id" (org-pad-test--canvas-config (nth 3 resp))) ""))
            ;; valid token + queued session -> per-session config injected
            (org-pad--handle-canvas (list :proc 'P :query "session=s1&token=good"))
            (should (= (nth 1 resp) 200))
            (should (equal (nth 2 resp) "text/html; charset=utf-8"))
            (let ((cfg (org-pad-test--canvas-config (nth 3 resp))))
              (should (equal (gethash "session_id" cfg) "s1"))
              (should (equal (gethash "token" cfg) "good")))))
      (ignore-errors (delete-file tokfile)))))

(ert-deftest orgpad-v2-handle-canvas-edit-injects-existing-json ()
  "For a queued web edit session, the existing JSON is injected into the config."
  (let* ((tokfile (make-temp-file "opv2tok")) (resp nil)
         (json "{\"v\":1,\"strokes\":[]}"))
    (unwind-protect
        (let ((org-pad-token-file tokfile)
              (org-pad-web-canvas-file "/nonexistent/canvas.html"))
          (org-pad--persist-token "good")
          (org-pad--queue-reset) (clrhash org-pad--session-format)
          (org-pad-enqueue (org-pad-session--make
                            :id "e1" :mode 'edit :name "f.png"
                            :drawing-bytes (encode-coding-string json 'utf-8)))
          (cl-letf (((symbol-function 'org-pad--respond)
                     (lambda (&rest a) (setq resp a))))
            (org-pad--handle-canvas (list :proc 'P :query "session=e1&token=good")))
          (should (= (nth 1 resp) 200))
          (let* ((html (nth 3 resp))
                 (start (string-search "window.ORGPAD_CONFIG=" html))
                 (rest (substring html (+ start (length "window.ORGPAD_CONFIG="))))
                 (semi (string-search ";</script>" rest))
                 (cfg (json-parse-string (substring rest 0 semi))))
            (should (equal (gethash "mode" cfg) "edit"))
            ;; CONFIG.drawing is base64 of the raw web-JSON bytes (canvas restore
            ;; base64-decodes it), matching the ?drawing= + session-json contract.
            (should (equal (gethash "drawing" cfg)
                           (base64-encode-string (encode-coding-string json 'utf-8) t)))))
      (ignore-errors (delete-file tokfile)))))

(ert-deftest orgpad-v2-canvas-url-builder ()
  (let ((url (org-pad--canvas-url "192.168.1.5:8777" "sid abc" "tok/xyz")))
    (should (string-prefix-p "http://192.168.1.5:8777/canvas?session=" url))
    (should (string-search "token=" url))
    ;; params are URL-encoded
    (should (string-search "sid%20abc" url))))

(ert-deftest orgpad-v2-routes-registered ()
  "org-pad--register-routes wires /canvas and /web to the canvas handler."
  (let ((org-pad--routes nil))
    (org-pad--register-routes)
    (should (eq (cdr (assoc '("GET" . "/canvas") org-pad--routes))
                #'org-pad--handle-canvas))
    (should (eq (cdr (assoc '("GET" . "/web") org-pad--routes))
                #'org-pad--handle-canvas))
    ;; existing routes still present
    (should (cdr (assoc '("POST" . "/result") org-pad--routes)))
    (should (cdr (assoc '("GET" . "/session") org-pad--routes)))))

;;;; ---------------------------------------------------------------------------
;;;; Transient menu is defined
;;;; ---------------------------------------------------------------------------

(ert-deftest orgpad-v2-menu-defined ()
  (should (commandp 'org-pad-menu))
  (should (commandp 'org-pad-set-background))
  (should (commandp 'org-pad-server-toggle)))


(provide 'org-pad-test)
;;; org-pad-test.el ends here
