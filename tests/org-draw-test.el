;;; org-draw-test.el --- ERT tests for org-draw  -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -L . -l tests/org-draw-test.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'org-draw)
(require 'url-util)

(defvar org-draw-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this test file and its fixtures/.")

(defun org-draw-test--read-unibyte (path)
  "Read PATH as a unibyte byte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(ert-deftest org-draw-scaffold-loads ()
  "The package loads and defcustoms exist."
  (should (boundp 'org-draw-port))
  (should (= org-draw-port 8777)))

;;;; PNG: CRC32 + u32
(ert-deftest org-draw-crc32-canonical () (should (= (org-draw--crc32 "123456789") #xCBF43926)))
(ert-deftest org-draw-crc32-empty () (should (= (org-draw--crc32 "") 0)))
(ert-deftest org-draw-crc32-iend () (should (= (org-draw--crc32 "IEND") #xAE426082)))
(ert-deftest org-draw-crc32-all-bytes ()
  (should (= (org-draw--crc32 (apply #'unibyte-string (number-sequence 0 255))) #x29058C73)))
(ert-deftest org-draw-u32-roundtrip ()
  (dolist (n (list 0 1 255 256 65535 65536 16777215 #xFFFFFFFF #x0A1B2C3D))
    (should (= (org-draw--u32-decode (org-draw--u32-encode n) 0) n))))
(ert-deftest org-draw-u32-big-endian ()
  (should (string= (org-draw--u32-encode #x0A1B2C3D) (unibyte-string #x0A #x1B #x2C #x3D))))

;;;; PNG: chunks / embed / extract
(defun org-draw-test--fixture-png () (org-draw-test--read-unibyte (expand-file-name "fixtures/fixture.png" org-draw-test-dir)))
(defun org-draw-test--fixture-drawing () (org-draw-test--read-unibyte (expand-file-name "fixtures/fixture.drawing" org-draw-test-dir)))

(ert-deftest org-draw-chunks-fixture ()
  (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-draw--png-chunks (org-draw-test--fixture-png)))
                 '("IHDR" "IDAT" "IEND"))))
(ert-deftest org-draw-chunks-bad-signature () (should-error (org-draw--png-chunks "not a png at all!!")))
(ert-deftest org-draw-chunks-truncated ()
  (let ((png (org-draw-test--fixture-png)))
    (should-error (org-draw--png-chunks (substring png 0 (- (length png) 3))))))
(ert-deftest org-draw-embed-extract-roundtrip ()
  (let* ((png (org-draw-test--fixture-png)) (drawing (org-draw-test--fixture-drawing))
         (embedded (org-draw--png-embed png drawing)))
    (should-not (multibyte-string-p embedded))
    (should (string= (org-draw--png-extract-bytes embedded) drawing))))
(ert-deftest org-draw-embed-orpd-before-iend ()
  (let* ((embedded (org-draw--png-embed (org-draw-test--fixture-png) (org-draw-test--fixture-drawing))))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-draw--png-chunks embedded))
                   '("IHDR" "IDAT" "orPd" "IEND")))))
(ert-deftest org-draw-embed-crc-valid ()
  (let ((embedded (org-draw--png-embed (org-draw-test--fixture-png) (org-draw-test--fixture-drawing))))
    (dolist (c (org-draw--png-chunks embedded))
      (let* ((ds (plist-get c :data-start)) (dl (plist-get c :data-len))
             (data (substring embedded ds (+ ds dl))) (stored (org-draw--u32-decode embedded (+ ds dl))))
        (should (= stored (org-draw--crc32 (concat (plist-get c :type) data))))))))
(ert-deftest org-draw-extract-no-orpd-nil () (should (null (org-draw--png-extract (org-draw-test--fixture-png)))))
(ert-deftest org-draw-embed-empty-drawing ()
  (let ((got (org-draw--png-extract-bytes (org-draw--png-embed (org-draw-test--fixture-png) ""))))
    (should (stringp got)) (should (string= got ""))))
(ert-deftest org-draw-embed-idempotent ()
  (let* ((png (org-draw-test--fixture-png)) (d1 (org-draw-test--fixture-drawing))
         (once (org-draw--png-embed png d1)) (d2 (concat d1 (unibyte-string #x42 #x42)))
         (twice (org-draw--png-embed once d2)))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-draw--png-chunks twice))
                   '("IHDR" "IDAT" "orPd" "IEND")))
    (should (string= (org-draw--png-extract-bytes twice) d2))))

;;;; HTTP: response writer
(ert-deftest org-draw-respond-shape ()
  "org-draw--respond writes a well-formed unibyte HTTP response."
  (let* ((captured "")
         (proc (make-pipe-process :name "op-cap" :noquery t
                                  :filter (lambda (_p s) (setq captured (concat captured s))))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_p s) (setq captured (concat captured s))))
                    ((symbol-function 'org-draw--safe-delete) #'ignore)
                    ((symbol-function 'process-live-p) (lambda (_p) t)))
            (org-draw--respond proc 200 "application/json" (unibyte-string ?{ ?} ) nil t))
          (should (string-prefix-p "HTTP/1.1 200 OK\r\n" captured))
          (should (string-match-p "Content-Type: application/json\r\n" captured))
          (should (string-match-p "Content-Length: 2\r\n" captured))
          (should (string-match-p "Connection: keep-alive\r\n" captured))
          (should (string-suffix-p "\r\n\r\n{}" captured))
          (should-not (multibyte-string-p captured)))
      (ignore-errors (delete-process proc)))))

;;;; HTTP: parsing + routing
(ert-deftest org-draw-parse-request-line ()
  (should (equal (org-draw--parse-request-line "POST /result?x=1 HTTP/1.1") '("POST" "/result" "x=1" "HTTP/1.1")))
  (should (equal (org-draw--parse-request-line "GET /session HTTP/1.1") '("GET" "/session" nil "HTTP/1.1")))
  (should (null (org-draw--parse-request-line "GARBAGE"))))
(ert-deftest org-draw-parse-headers ()
  (let ((h (org-draw--parse-headers "Content-Length: 5\r\nX-OrgDraw-Token: abc ")))
    (should (equal (cdr (assoc "content-length" h)) "5"))
    (should (equal (cdr (assoc "x-orgdraw-token" h)) "abc"))))
(ert-deftest org-draw-header-ci ()
  (should (equal (org-draw--header '(:headers (("x-orgdraw-token" . "t"))) "X-OrgDraw-Token") "t")))
(ert-deftest org-draw-routing ()
  (let ((org-draw--routes nil) (hit nil))
    (org-draw-route "GET" "/x" (lambda (_r) (setq hit t)))
    ;; Capture the numeric STATUS, which is the 2nd arg to org-draw--respond.
    (cl-letf (((symbol-function 'org-draw--respond) (lambda (&rest a) (setq hit (nth 1 a)))))
      (org-draw--dispatch (list :proc nil :method "GET" :path "/x"))
      (should (eq hit t))                                             ; handler ran (sets hit)
      (org-draw--dispatch (list :proc nil :method "POST" :path "/x"))  ; wrong method -> 405
      (should (equal hit 405))
      (org-draw--dispatch (list :proc nil :method "GET" :path "/nope")) ; unknown -> 404
      (should (equal hit 404)))))

;;;; HTTP: filter state machine + long-poll
(defmacro org-draw-test--with-captured-dispatch (var &rest body)
  "Run BODY with org-draw--dispatch capturing the request plist into VAR."
  (declare (indent 1))
  ;; NOTE: the stub lambdas' own parameter names are deliberately distinct from
  ;; any plausible caller VAR (e.g. `req') to avoid the lambda parameter
  ;; shadowing the `,var' splice -- `(lambda (req) (setq req req))' would be a
  ;; silent no-op if VAR were also named `req'.
  `(let ((,var nil))
     (cl-letf (((symbol-function 'org-draw--dispatch)
                (lambda (org-draw-test--captured-req) (setq ,var org-draw-test--captured-req)))
               ((symbol-function 'org-draw--respond)
                (lambda (&rest org-draw-test--captured-args) (setq ,var (cons 'response org-draw-test--captured-args)))))
       ,@body)))

(ert-deftest org-draw-filter-whole-request ()
  (let ((proc (make-pipe-process :name "op-f1" :noquery t)))
    (unwind-protect
        (org-draw-test--with-captured-dispatch req
          (org-draw--reset-conn-state proc)
          (org-draw--filter proc "POST /result HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello")
          (should (equal (plist-get req :method) "POST"))
          (should (equal (plist-get req :path) "/result"))
          (should (equal (plist-get req :body) "hello")))
      (ignore-errors (delete-process proc)))))

(ert-deftest org-draw-filter-fragmented ()
  "A request dribbled one byte at a time reassembles byte-exact."
  (let ((proc (make-pipe-process :name "op-f2" :noquery t))
        (raw "POST /r HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc"))
    (unwind-protect
        (org-draw-test--with-captured-dispatch req
          (org-draw--reset-conn-state proc)
          (dotimes (i (length raw)) (org-draw--filter proc (substring raw i (1+ i))))
          (should (equal (plist-get req :body) "abc")))
      (ignore-errors (delete-process proc)))))

(ert-deftest org-draw-filter-413 ()
  (let ((proc (make-pipe-process :name "op-f3" :noquery t)))
    (unwind-protect
        (org-draw-test--with-captured-dispatch req
          (org-draw--reset-conn-state proc)
          (org-draw--filter proc (format "POST /r HTTP/1.1\r\nContent-Length: %d\r\n\r\n" (1+ org-draw--max-body)))
          (should (equal req (list 'response proc 413 "text/plain" "Payload Too Large"))))
      (ignore-errors (delete-process proc)))))

(ert-deftest org-draw-filter-400-bad-clen ()
  (let ((proc (make-pipe-process :name "op-f4" :noquery t)))
    (unwind-protect
        (org-draw-test--with-captured-dispatch req
          (org-draw--reset-conn-state proc)
          (org-draw--filter proc "POST /r HTTP/1.1\r\nContent-Length: abc\r\n\r\n")
          (should (equal (nth 2 req) 400)))
      (ignore-errors (delete-process proc)))))

;;;; HTTP: live server integration
(ert-deftest org-draw-server-roundtrip ()
  "Start a real server, hit /ping over TCP, assert a 200 body, then stop."
  (let ((org-draw--routes nil) (org-draw-port 18799) (org-draw--server-process nil))
    (org-draw-route "GET" "/ping" (lambda (req) (org-draw--respond (plist-get req :proc) 200 "text/plain" "pong")))
    (org-draw--server-start 18799)
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
      (org-draw--server-stop))))

;;;; Sessions: queue
(defun org-draw-test--sess (id) (org-draw-session--make :id id :mode 'new))
(ert-deftest org-draw-queue-fifo ()
  (org-draw--queue-reset)
  (mapc (lambda (id) (org-draw-enqueue (org-draw-test--sess id))) '("a" "b" "c"))
  (should (equal (org-draw-session-id (org-draw-queue-head)) "a"))
  (should (= (org-draw-queue-length) 3))
  (should (equal (org-draw-session-id (org-draw-queue-head)) "a")) ; non-destructive
  (org-draw-queue-complete "a")
  (should (equal (org-draw-session-id (org-draw-queue-head)) "b")))
(ert-deftest org-draw-queue-cancel ()
  (org-draw--queue-reset)
  (mapc (lambda (id) (org-draw-enqueue (org-draw-test--sess id))) '("a" "b" "c"))
  (should (org-draw-queue-cancel "b"))
  (should (equal (mapcar #'org-draw-session-id (list (org-draw-queue-head))) '("a")))
  (should (= (org-draw-queue-length) 2))
  (should (null (org-draw-queue-cancel "zz"))))

;;;; Sessions: id/token + pairing + auth
(ert-deftest org-draw-id-shape ()
  (let ((id (org-draw-generate-id)))
    (should (= (length id) 32)) (should (string-match-p "\\`[0-9a-f]+\\'" id))
    (should-not (equal id (org-draw-generate-id)))))
(ert-deftest org-draw-pairing-cap ()
  (org-draw-pairing-start)
  (setf (org-draw-pairing-code org-draw--pairing) "000000") ; deterministic
  (should (equal (org-draw-pairing-verify "111111") '(:bad . 4)))
  (should (equal (org-draw-pairing-verify "111111") '(:bad . 3)))
  (should (equal (org-draw-pairing-verify "111111") '(:bad . 2)))
  (should (equal (org-draw-pairing-verify "111111") '(:bad . 1)))
  (should (eq (org-draw-pairing-verify "111111") :closed))
  (should (eq (org-draw-pairing-verify "000000") :closed))) ; closed stays closed
(ert-deftest org-draw-pairing-success-and-auth ()
  (let ((org-draw-token-file (make-temp-file "org-draw-tok")))
    (unwind-protect
        (progn
          (org-draw-pairing-start)
          (setf (org-draw-pairing-code org-draw--pairing) "424242")
          (let ((r (org-draw-pairing-verify "424242")))
            (should (eq (car r) :ok))
            (should (org-draw-token-valid-p (cdr r)))
            (should-not (org-draw-token-valid-p "deadbeef"))
            (should-not (org-draw-token-valid-p ""))
            (should (eq (org-draw-pairing-verify "424242") :closed)))) ; pairing closed after success
      (delete-file org-draw-token-file))))

;;;; Org integration: DWIM + result handling
(ert-deftest org-draw-dwim-new-vs-edit ()
  (let* ((dir (make-temp-file "org-draw-t" t))
         (org (expand-file-name "n.org" dir))
         (foreign (expand-file-name "foreign.png" dir))
         (fig (expand-file-name "fig.png" dir)))
    (unwind-protect
        (progn
          ;; a foreign png (no orPd) and an org-draw png (with orPd)
          (with-temp-buffer (set-buffer-multibyte nil) (insert (org-draw-test--fixture-png))
                            (let ((coding-system-for-write 'binary)) (write-region nil nil foreign nil 'silent)))
          (org-draw--write-png fig (org-draw-test--fixture-png) (org-draw-test--fixture-drawing))
          (with-temp-file org (insert "* head\n[[file:foreign.png]]\n[[file:fig.png]]\nplain\n"))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min)) (search-forward "plain")
                  (should (eq (car (org-draw-dwim-at-point)) :new))
                  (goto-char (point-min)) (search-forward "fig.png") (backward-char 3)
                  (let ((d (org-draw-dwim-at-point)))
                    (should (eq (car d) :edit))
                    (should (string= (nth 2 d) (org-draw-test--fixture-drawing))))
                  (goto-char (point-min)) (search-forward "foreign.png") (backward-char 3)
                  (should (eq (car (org-draw-dwim-at-point)) :new)))
              (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest org-draw-insert-new-figure ()
  (let* ((dir (make-temp-file "org-draw-i" t)) (org (expand-file-name "n.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file org (insert "* head\n"))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-max))
                  (let* ((marker (org-draw--make-insertion-marker))
                         (sess (org-draw-session--make :id "s1" :mode 'new :name "fig-x.png" :marker marker)))
                    (org-draw-insert-new-figure sess (org-draw-test--fixture-png) (org-draw-test--fixture-drawing))
                    (should (string-match-p "\\[\\[file:figures/fig-x.png\\]\\]" (buffer-string)))
                    (should (string= (org-draw--png-extract-bytes
                                      (org-draw-test--read-unibyte (expand-file-name "figures/fig-x.png" dir)))
                                     (org-draw-test--fixture-drawing)))))
              (kill-buffer buf))))
      (delete-directory dir t))))

;;;; Protocol: json + auth
(ert-deftest org-draw-session-json-new ()
  (let* ((s (org-draw-session--make :id "s1" :mode 'new :name "fig.png"))
         (j (json-parse-string (org-draw--session-json s))))
    (should (equal (gethash "session_id" j) "s1"))
    (should (equal (gethash "mode" j) "new"))
    (should (eq (gethash "drawing" j) :null))))
(ert-deftest org-draw-session-json-edit ()
  (let* ((s (org-draw-session--make :id "s2" :mode 'edit :name "fig.png"
                                   :drawing-bytes (unibyte-string 1 2 3)))
         (j (json-parse-string (org-draw--session-json s))))
    (should (equal (gethash "mode" j) "edit"))
    (should (string= (base64-decode-string (gethash "drawing" j)) (unibyte-string 1 2 3)))))
(ert-deftest org-draw-require-token ()
  (let ((org-draw-token-file (make-temp-file "opt")) (sent nil))
    (unwind-protect
        (progn
          (org-draw--persist-token "good")
          (cl-letf (((symbol-function 'org-draw--respond) (lambda (&rest a) (setq sent (nth 1 a)))))
            ;; With pairing required, a valid token passes and a bad one 401s.
            (let ((org-draw-require-pairing t))
              (should (org-draw--require-token '(:headers (("x-orgdraw-token" . "good")))))
              (should (null (org-draw--require-token '(:headers (("x-orgdraw-token" . "bad"))))))
              (should (= sent 401)))
            ;; Default (pairing off): every request is authorized, even a bad token.
            (let ((org-draw-require-pairing nil))
              (should (org-draw--require-token '(:headers (("x-orgdraw-token" . "bad"))))))))
      (delete-file org-draw-token-file))))

;;;; Protocol: endpoint handlers
(ert-deftest org-draw-handle-pair ()
  (let ((org-draw-token-file (make-temp-file "opt")) (resp nil))
    (unwind-protect
        (progn
          (org-draw-pairing-start) (setf (org-draw-pairing-code org-draw--pairing) "424242")
          (cl-letf (((symbol-function 'org-draw--respond)
                     (lambda (&rest a) (setq resp a))))
            (org-draw--handle-pair (list :proc nil :body (json-serialize '(:code "424242"))))
            (should (= (nth 1 resp) 200))
            (let ((tok (gethash "token" (json-parse-string (nth 3 resp)))))
              (should (org-draw-token-valid-p tok)))))
      (delete-file org-draw-token-file))))

(ert-deftest org-draw-handle-session-immediate ()
  "A poll with a queued head answers immediately."
  (org-draw--queue-reset)
  (org-draw-enqueue (org-draw-session--make :id "s9" :mode 'new :name "f.png"))
  (let ((resp nil) (org-draw-token-file (make-temp-file "opt")))
    (unwind-protect
        (progn
          (org-draw--persist-token "good")
          (cl-letf (((symbol-function 'org-draw-answer) (lambda (&rest a) (setq resp a))))
            (org-draw--handle-session (list :proc 'P :headers '(("x-orgdraw-token" . "good"))))
            (should (= (nth 1 resp) 200))
            (should (equal (gethash "session_id" (json-parse-string (nth 3 resp))) "s9"))))
      (delete-file org-draw-token-file))))

;;;; Infra: interfaces
(ert-deftest org-draw-ipv4-addresses ()
  (let ((addrs (org-draw--ipv4-addresses)))
    (should (listp addrs))
    (dolist (a addrs)
      (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" a))
      (should-not (string-prefix-p "127." a)))))
;;;; Commands
(ert-deftest org-draw-register-routes ()
  (let ((org-draw--routes nil))
    (org-draw--register-routes)
    (dolist (key '(("POST" . "/pair") ("GET" . "/session") ("POST" . "/result")
                   ("POST" . "/cancel") ("GET" . "/canvas") ("GET" . "/web")))
      (should (cdr (assoc key org-draw--routes))))))
(ert-deftest org-draw-enqueues ()
  (let* ((dir (make-temp-file "opd" t)) (org (expand-file-name "n.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file org (insert "* h\nplain\n"))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (org-draw--queue-reset)
                  (goto-char (point-min)) (search-forward "plain")
                  (cl-letf (((symbol-function 'org-draw-server-start) #'ignore)
                            ((symbol-function 'org-draw--wake-waiters) #'ignore))
                    (org-draw))
                  (should (= (org-draw-queue-length) 1))
                  (should (eq (org-draw-session-mode (org-draw-queue-head)) 'new)))
              (kill-buffer buf))))
      (delete-directory dir t))))

;;;; v2 tests
(defvar orgdraw-v2-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this test file and its fixtures/.")

(defun orgdraw-v2-test--read-unibyte (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun orgdraw-v2-test--png () (orgdraw-v2-test--read-unibyte
                               (expand-file-name "fixtures/fixture.png" orgdraw-v2-test-dir)))
(defun orgdraw-v2-test--drawing () (orgdraw-v2-test--read-unibyte
                                   (expand-file-name "fixtures/fixture.drawing" orgdraw-v2-test-dir)))
(defun orgdraw-v2-test--legacy () (orgdraw-v2-test--read-unibyte
                                  (expand-file-name "fixtures/legacy-0x01.png" orgdraw-v2-test-dir)))

;;;; Chunk FORMAT byte

(ert-deftest orgdraw-v2-embed-default-is-pkdrawing ()
  "Two-arg embed defaults to format 0x01 -> extract returns (0x01 . bytes)."
  (let* ((png (orgdraw-v2-test--png)) (d (orgdraw-v2-test--drawing))
         (embedded (org-draw--png-embed png d))
         (pair (org-draw--png-extract embedded)))
    (should-not (multibyte-string-p embedded))
    (should (consp pair))
    (should (eql (car pair) org-draw-format-pkdrawing))
    (should (eql (car pair) #x01))
    (should (string= (cdr pair) d))))

(ert-deftest orgdraw-v2-embed-web-format-roundtrip ()
  "Embed with 0x02 -> extract returns (0x02 . json-bytes) byte-exact."
  (let* ((png (orgdraw-v2-test--png))
         (json "{\"v\":1,\"strokes\":[[[1,2,0.5],[3,4,0.6]]]}")
         (jbytes (encode-coding-string json 'utf-8))
         (embedded (org-draw--png-embed png jbytes org-draw-format-web))
         (pair (org-draw--png-extract embedded)))
    (should (eql (car pair) org-draw-format-web))
    (should (eql (car pair) #x02))
    (should (string= (cdr pair) jbytes))
    (should (string= (decode-coding-string (cdr pair) 'utf-8) json))))

(ert-deftest orgdraw-v2-embed-byte-identical-to-v1 ()
  "A default (2-arg) v2 embed is byte-for-byte what an external v1 embed produced.
Compares against the independently-generated legacy-0x01.png fixture."
  (let* ((png (orgdraw-v2-test--png)) (d (orgdraw-v2-test--drawing))
         (v2out (org-draw--png-embed png d))
         (legacy (orgdraw-v2-test--legacy)))
    (should (string= v2out legacy))))

(ert-deftest orgdraw-v2-backcompat-legacy-extract ()
  "A real legacy 0x01 figure (written by an external tool) still extracts."
  (let* ((legacy (orgdraw-v2-test--legacy))
         (pair (org-draw--png-extract legacy)))
    (should (eql (car pair) #x01))
    (should (string= (cdr pair) (orgdraw-v2-test--drawing)))
    ;; and the bytes-only compat shim reproduces the v1 return contract exactly.
    (should (string= (org-draw--png-extract-bytes legacy) (orgdraw-v2-test--drawing)))))

(ert-deftest orgdraw-v2-extract-shims ()
  "Bytes/format shims behave; foreign PNG -> nil for all extractors."
  (let* ((png (orgdraw-v2-test--png))
         (embedded (org-draw--png-embed png "abc" org-draw-format-web)))
    (should (string= (org-draw--png-extract-bytes embedded) "abc"))
    (should (eql (org-draw--png-extract-format embedded) org-draw-format-web))
    ;; foreign PNG (no orPd)
    (should (null (org-draw--png-extract png)))
    (should (null (org-draw--png-extract-bytes png)))
    (should (null (org-draw--png-extract-format png)))))

(ert-deftest orgdraw-v2-empty-drawing-still-distinct ()
  "An embedded-but-empty payload extracts to (FORMAT . \"\"), not nil."
  (let* ((png (orgdraw-v2-test--png))
         (embedded (org-draw--png-embed png "" org-draw-format-web))
         (pair (org-draw--png-extract embedded)))
    (should (consp pair))
    (should (eql (car pair) org-draw-format-web))
    (should (stringp (cdr pair)))
    (should (string= (cdr pair) ""))))

(ert-deftest orgdraw-v2-embed-replaces-existing-format ()
  "Re-embedding switches the format byte in place (0x01 figure -> 0x02)."
  (let* ((png (orgdraw-v2-test--png))
         (once (org-draw--png-embed png "pk" org-draw-format-pkdrawing))
         (twice (org-draw--png-embed once "web" org-draw-format-web))
         (pair (org-draw--png-extract twice)))
    (should (equal (mapcar (lambda (c) (plist-get c :type)) (org-draw--png-chunks twice))
                   '("IHDR" "IDAT" "orPd" "IEND")))
    (should (eql (car pair) org-draw-format-web))
    (should (string= (cdr pair) "web"))))

(ert-deftest orgdraw-v2-embed-crc-valid ()
  "Every chunk in a web-format embed has a self-consistent CRC."
  (let ((embedded (org-draw--png-embed (orgdraw-v2-test--png) "web-json" org-draw-format-web)))
    (dolist (c (org-draw--png-chunks embedded))
      (let* ((ds (plist-get c :data-start)) (dl (plist-get c :data-len))
             (data (substring embedded ds (+ ds dl)))
             (stored (org-draw--u32-decode embedded (+ ds dl))))
        (should (= stored (org-draw--crc32 (concat (plist-get c :type) data))))))))

(ert-deftest orgdraw-v2-format-helpers ()
  (should (org-draw-format-valid-p #x01))
  (should (org-draw-format-valid-p #x02))
  (should-not (org-draw-format-valid-p #x03)))

;;;; File-level (FORMAT . BYTES) reader + write round-trip

(ert-deftest orgdraw-v2-write-read-web-file ()
  "org-draw--write-png with web format writes a re-editable web figure."
  (let* ((dir (make-temp-file "opv2" t))
         (file (expand-file-name "f.png" dir)))
    (unwind-protect
        (progn
          (org-draw--write-png file (orgdraw-v2-test--png) "webjson" org-draw-format-web)
          (let ((pair (org-draw--file-drawing file)))
            (should (eql (car pair) org-draw-format-web))
            (should (string= (cdr pair) "webjson")))
          (should (string= (org-draw--file-has-drawing-p file) "webjson")))
      (delete-directory dir t))))

(ert-deftest orgdraw-v2-write-default-file-is-pkdrawing ()
  "Three-arg write (no format) defaults to 0x01 for back-compat."
  (let* ((dir (make-temp-file "opv2" t))
         (file (expand-file-name "f.png" dir)))
    (unwind-protect
        (progn
          (org-draw--write-png file (orgdraw-v2-test--png) (orgdraw-v2-test--drawing))
          (let ((pair (org-draw--file-drawing file)))
            (should (eql (car pair) org-draw-format-pkdrawing))
            (should (string= (cdr pair) (orgdraw-v2-test--drawing)))))
      (delete-directory dir t))))

;;;; Background field in session JSON

(ert-deftest orgdraw-v2-session-json-background-transparent ()
  (let* ((org-draw-figure-background 'transparent)
         (s (org-draw-session--make :id "s1" :mode 'new :name "f.png"))
         (j (json-parse-string (org-draw--session-json s))))
    (should (equal (gethash "background" j) "transparent"))
    (should (equal (gethash "session_id" j) "s1"))
    (should (equal (gethash "mode" j) "new"))
    (should (eq (gethash "drawing" j) :null))
    (should (equal (gethash "format" j) "pkdrawing"))))

(ert-deftest orgdraw-v2-session-json-background-variants ()
  ;; `white' maps to the wire word "light" (matches the web canvas + Swift enum).
  (dolist (case '((white . "light") (dark . "dark") ("#1e1e2e" . "#1e1e2e")))
    (let* ((org-draw-figure-background (car case))
           (s (org-draw-session--make :id "s" :mode 'new :name "f"))
           (j (json-parse-string (org-draw--session-json s))))
      (should (equal (gethash "background" j) (cdr case))))))

(ert-deftest orgdraw-v2-session-json-format-web ()
  "When a session's recorded target format is web, JSON \"format\" is \"web\"."
  (org-draw--session-set-format "sweb" org-draw-format-web)
  (unwind-protect
      (let* ((s (org-draw-session--make :id "sweb" :mode 'new :name "f"))
             (j (json-parse-string (org-draw--session-json s))))
        (should (equal (gethash "format" j) "web")))
    (remhash "sweb" org-draw--session-format)))

(ert-deftest orgdraw-v2-session-json-edit-drawing ()
  "Edit-mode session still base64-encodes its stroke bytes into \"drawing\"."
  (let* ((s (org-draw-session--make :id "se" :mode 'edit :name "f.png"
                                   :drawing-bytes (unibyte-string 1 2 3)))
         (j (json-parse-string (org-draw--session-json s))))
    (should (equal (gethash "mode" j) "edit"))
    (should (string= (base64-decode-string (gethash "drawing" j))
                     (unibyte-string 1 2 3)))))

;;;; Client routing by format (draw DWIM)

(defmacro orgdraw-v2-test--with-stubbed-draw (&rest body)
  "Run BODY with server start, wake, and web-open stubbed; capture opened URL.
Binds `opened' (URL passed to the web opener) and `msgs' (messages)."
  `(let ((opened nil) (msgs '()))
     (cl-letf (((symbol-function 'org-draw-server-start) #'ignore)
               ((symbol-function 'org-draw--wake-waiters) #'ignore)
               ((symbol-function 'org-draw--open-web-session)
                (lambda (id _token) (setq opened id) (format "url:%s" id)))
               ((symbol-function 'message)
                (lambda (fmt &rest a) (push (apply #'format fmt a) msgs) nil)))
       ,@body)))

(ert-deftest orgdraw-v2-draw-new-web ()
  "A new figure enqueues a 0x02 session and opens the web canvas."
  (org-draw--queue-reset)
  (clrhash org-draw--session-format)
  (orgdraw-v2-test--with-stubbed-draw
   (cl-letf (((symbol-function 'org-draw-dwim-at-point)
              (lambda () (list :new (copy-marker (point-min))))))
     (org-draw)
     (let ((head (org-draw-queue-head)))
       (should (eq (org-draw-session-mode head) 'new))
       (should (equal opened (org-draw-session-id head)))
       (should (eql (org-draw--session-get-format (org-draw-session-id head))
                    org-draw-format-web))))))

(ert-deftest orgdraw-v2-draw-edit-opens-web ()
  "Editing a web figure opens the web canvas; the session records its format."
  (let* ((dir (make-temp-file "opv2edit" t))
         (web-file (expand-file-name "web.png" dir)))
    (unwind-protect
        (progn
          (org-draw--write-png web-file (orgdraw-v2-test--png)
                              "webjson" org-draw-format-web)
          (org-draw--queue-reset) (clrhash org-draw--session-format)
          (orgdraw-v2-test--with-stubbed-draw
           (cl-letf (((symbol-function 'org-draw-dwim-at-point)
                      (lambda () (list :edit web-file
                                       (encode-coding-string "webjson" 'utf-8)))))
             (org-draw)
             (should (equal opened (org-draw-session-id (org-draw-queue-head))))
             (should (eql (org-draw--session-get-format
                           (org-draw-session-id (org-draw-queue-head)))
                          org-draw-format-web)))))
      (delete-directory dir t))))

;;;; /result web-format handling

(defun orgdraw-v2-test--result-body (id png-b64 drawing-b64 &optional format)
  (let ((h (make-hash-table :test 'equal)))
    (puthash "session_id" id h)
    (puthash "png" png-b64 h)
    (puthash "drawing" drawing-b64 h)
    (when format (puthash "format" format h))
    h))

(ert-deftest orgdraw-v2-result-format-reader ()
  (should (eql (org-draw--result-format (orgdraw-v2-test--result-body "i" "" "" "web"))
               org-draw-format-web))
  (should (eql (org-draw--result-format (orgdraw-v2-test--result-body "i" "" "" "pkdrawing"))
               org-draw-format-pkdrawing))
  (should (eql (org-draw--result-format (orgdraw-v2-test--result-body "i" "" "" nil))
               org-draw-format-pkdrawing)))

(ert-deftest orgdraw-v2-handle-result-web-embeds-0x02 ()
  "POST /result with format:web overwrites an edit figure with a 0x02 chunk."
  (let* ((dir (make-temp-file "opv2res" t))
         (file (expand-file-name "f.png" dir))
         (tokfile (make-temp-file "opv2tok"))
         (png-b64 (base64-encode-string (orgdraw-v2-test--png) t))
         (json "{\"v\":1}")
         (drawing-b64 (base64-encode-string (encode-coding-string json 'utf-8) t))
         (resp nil))
    (unwind-protect
        (let ((org-draw-token-file tokfile))
          (org-draw--persist-token "tok")
          ;; existing figure so overwrite has a target
          (org-draw--write-png file (orgdraw-v2-test--png) "old" org-draw-format-web)
          (org-draw--queue-reset) (clrhash org-draw--session-format)
          (org-draw-enqueue (org-draw-session--make :id "r1" :mode 'edit
                                                  :name "f.png" :file file))
          (cl-letf (((symbol-function 'org-draw--respond)
                     (lambda (&rest a) (setq resp a)))
                    ((symbol-function 'org-draw--refresh-inline-images) #'ignore)
                    ((symbol-function 'org-draw--wake-waiters) #'ignore))
            (org-draw--handle-result
             (list :proc 'P
                   :headers '(("x-orgdraw-token" . "tok"))
                   :body (json-serialize
                          (list :session_id "r1" :png png-b64
                                :drawing drawing-b64 :format "web")))))
          (should (= (nth 1 resp) 200))
          ;; The file now carries a 0x02 chunk whose payload is the JSON.
          (let ((pair (org-draw--file-drawing file)))
            (should (eql (car pair) org-draw-format-web))
            (should (string= (decode-coding-string (cdr pair) 'utf-8) json))))
      (ignore-errors (delete-file tokfile))
      (delete-directory dir t))))

(ert-deftest orgdraw-v2-handle-result-default-embeds-0x01 ()
  "POST /result with no format field embeds 0x01 (byte-compat with v1 clients)."
  (let* ((dir (make-temp-file "opv2res" t))
         (file (expand-file-name "f.png" dir))
         (tokfile (make-temp-file "opv2tok"))
         (png-b64 (base64-encode-string (orgdraw-v2-test--png) t))
         (drawing-b64 (base64-encode-string (orgdraw-v2-test--drawing) t))
         (resp nil))
    (unwind-protect
        (let ((org-draw-token-file tokfile))
          (org-draw--persist-token "tok")
          (org-draw--write-png file (orgdraw-v2-test--png) "old" org-draw-format-pkdrawing)
          (org-draw--queue-reset) (clrhash org-draw--session-format)
          (org-draw-enqueue (org-draw-session--make :id "r2" :mode 'edit
                                                  :name "f.png" :file file))
          (cl-letf (((symbol-function 'org-draw--respond)
                     (lambda (&rest a) (setq resp a)))
                    ((symbol-function 'org-draw--refresh-inline-images) #'ignore)
                    ((symbol-function 'org-draw--wake-waiters) #'ignore))
            (org-draw--handle-result
             (list :proc 'P
                   :headers '(("x-orgdraw-token" . "tok"))
                   :body (json-serialize
                          (list :session_id "r2" :png png-b64 :drawing drawing-b64)))))
          (should (= (nth 1 resp) 200))
          (let ((pair (org-draw--file-drawing file)))
            (should (eql (car pair) org-draw-format-pkdrawing))
            (should (string= (cdr pair) (orgdraw-v2-test--drawing)))))
      (ignore-errors (delete-file tokfile))
      (delete-directory dir t))))

;;;; /canvas endpoint

(ert-deftest orgdraw-v2-query-param ()
  (should (equal (org-draw--query-param "session=abc&token=xyz" "session") "abc"))
  (should (equal (org-draw--query-param "session=abc&token=xyz" "token") "xyz"))
  (should (equal (org-draw--query-param "a=1&b=hello%20world" "b") "hello world"))
  (should (null (org-draw--query-param "a=1" "missing")))
  (should (null (org-draw--query-param nil "x"))))

(ert-deftest orgdraw-v2-canvas-config-shape ()
  (let* ((block (org-draw--canvas-config "sid" "tok" "new" "transparent" nil)))
    (should (string-prefix-p "<script>window.ORGDRAW_CONFIG=" block))
    (should (string-suffix-p ";</script>" block))
    (let* ((json (substring block (length "<script>window.ORGDRAW_CONFIG=")
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
      (should (equal (gethash "token_header" h) "X-OrgDraw-Token"))
      (should (equal (gethash "format" h) "web"))
      (should (eq (gethash "drawing" h) :null)))))

(ert-deftest orgdraw-v2-canvas-config-absolute-urls ()
  "resultUrl/cancel_path can be absolute (built from the request Host)."
  (let* ((block (org-draw--canvas-config
                 "s" "t" "edit" "transparent" "{\"v\":1}"
                 :name "fig.png"
                 :result-url "http://192.168.1.5:8777/result"
                 :cancel-url "http://192.168.1.5:8777/cancel"))
         (json (substring block (length "<script>window.ORGDRAW_CONFIG=")
                          (- (length block) (length ";</script>"))))
         (h (json-parse-string json)))
    (should (equal (gethash "resultUrl" h) "http://192.168.1.5:8777/result"))
    (should (equal (gethash "cancel_path" h) "http://192.168.1.5:8777/cancel"))
    (should (equal (gethash "name" h) "fig.png"))
    (should (equal (gethash "drawing" h) "{\"v\":1}"))))

(ert-deftest orgdraw-v2-canvas-html-injects-before-head ()
  (let ((org-draw-web-canvas-file "/nonexistent/canvas.html"))  ; force fallback
    (let ((html (org-draw--canvas-html "s" "t" "new" "transparent" nil)))
      (should (string-search "window.ORGDRAW_CONFIG" html))
      ;; config appears before </head> when the template has one (fallback does).
      (let ((cfg (string-search "window.ORGDRAW_CONFIG" html))
            (head (string-search "</head>" html)))
        (should (and cfg head (< cfg head)))))))

(defun org-draw-test--canvas-config (html)
  "Extract + parse the window.ORGDRAW_CONFIG JSON object from HTML."
  (let* ((start (string-search "window.ORGDRAW_CONFIG=" html))
         (rest (substring html (+ start (length "window.ORGDRAW_CONFIG="))))
         (semi (string-search ";</script>" rest)))
    (json-parse-string (substring rest 0 semi))))

(ert-deftest orgdraw-v2-handle-canvas-auth ()
  "With pairing required, a session is injected only for a valid token; with
pairing off (default), a queued session is injected without any token."
  (let* ((tokfile (make-temp-file "opv2tok")) (resp nil))
    (unwind-protect
        (let ((org-draw-token-file tokfile)
              (org-draw-web-canvas-file "/nonexistent/canvas.html"))
          (org-draw--persist-token "good")
          (org-draw--queue-reset) (clrhash org-draw--session-format)
          (org-draw-enqueue (org-draw-session--make :id "s1" :mode 'new :name "f.png"))
          (cl-letf (((symbol-function 'org-draw--respond)
                     (lambda (&rest a) (setq resp a))))
            (let ((org-draw-require-pairing t))
              ;; no token -> receiver page, nothing injected
              (org-draw--handle-canvas (list :proc 'P :query "session=s1"))
              (should (= (nth 1 resp) 200))
              (let ((cfg (org-draw-test--canvas-config (nth 3 resp))))
                (should (equal (gethash "session_id" cfg) ""))
                (should (equal (gethash "token" cfg) "")))
              ;; bad token -> nothing injected
              (org-draw--handle-canvas (list :proc 'P :query "session=s1&token=bad"))
              (should (equal (gethash "session_id" (org-draw-test--canvas-config (nth 3 resp))) ""))
              ;; valid token -> per-session config injected
              (org-draw--handle-canvas (list :proc 'P :query "session=s1&token=good"))
              (should (equal (nth 2 resp) "text/html; charset=utf-8"))
              (let ((cfg (org-draw-test--canvas-config (nth 3 resp))))
                (should (equal (gethash "session_id" cfg) "s1"))
                (should (equal (gethash "token" cfg) "good"))))
            ;; pairing off (default): queued session injected with no token
            (let ((org-draw-require-pairing nil))
              (org-draw--handle-canvas (list :proc 'P :query "session=s1"))
              (should (= (nth 1 resp) 200))
              (should (equal (gethash "session_id" (org-draw-test--canvas-config (nth 3 resp))) "s1")))))
      (ignore-errors (delete-file tokfile)))))

(ert-deftest orgdraw-v2-handle-canvas-edit-injects-existing-json ()
  "For a queued web edit session, the existing JSON is injected into the config."
  (let* ((tokfile (make-temp-file "opv2tok")) (resp nil)
         (json "{\"v\":1,\"strokes\":[]}"))
    (unwind-protect
        (let ((org-draw-token-file tokfile)
              (org-draw-web-canvas-file "/nonexistent/canvas.html"))
          (org-draw--persist-token "good")
          (org-draw--queue-reset) (clrhash org-draw--session-format)
          (org-draw-enqueue (org-draw-session--make
                            :id "e1" :mode 'edit :name "f.png"
                            :drawing-bytes (encode-coding-string json 'utf-8)))
          (cl-letf (((symbol-function 'org-draw--respond)
                     (lambda (&rest a) (setq resp a))))
            (org-draw--handle-canvas (list :proc 'P :query "session=e1&token=good")))
          (should (= (nth 1 resp) 200))
          (let* ((html (nth 3 resp))
                 (start (string-search "window.ORGDRAW_CONFIG=" html))
                 (rest (substring html (+ start (length "window.ORGDRAW_CONFIG="))))
                 (semi (string-search ";</script>" rest))
                 (cfg (json-parse-string (substring rest 0 semi))))
            (should (equal (gethash "mode" cfg) "edit"))
            ;; CONFIG.drawing is base64 of the raw web-JSON bytes (canvas restore
            ;; base64-decodes it), matching the ?drawing= + session-json contract.
            (should (equal (gethash "drawing" cfg)
                           (base64-encode-string (encode-coding-string json 'utf-8) t)))))
      (ignore-errors (delete-file tokfile)))))

(ert-deftest orgdraw-v2-canvas-url-builder ()
  (let ((url (org-draw--canvas-url "192.168.1.5:8777" "sid abc" "tok/xyz")))
    (should (string-prefix-p "http://192.168.1.5:8777/canvas?session=" url))
    (should (string-search "token=" url))
    (should (string-search "sid%20abc" url)))
  ;; empty/nil token -> no &token= at all
  (should-not (string-search "token=" (org-draw--canvas-url "h:8777" "s1" "")))
  (should-not (string-search "token=" (org-draw--canvas-url "h:8777" "s1" nil))))

(ert-deftest orgdraw-v2-url-opener ()
  "`org-draw--url-opener' is off by default, browse-url when opted in, or a custom fn."
  (let ((org-draw-open-browser nil) (org-draw-web-open-function nil))
    (should (null (org-draw--url-opener))))
  (let ((org-draw-open-browser t) (org-draw-web-open-function nil))
    (should (eq (org-draw--url-opener) #'browse-url)))
  (let ((org-draw-open-browser t) (org-draw-web-open-function #'ignore))
    (should (eq (org-draw--url-opener) #'ignore))))

(ert-deftest orgdraw-v2-routes-registered ()
  "org-draw--register-routes wires /canvas and /web to the canvas handler."
  (let ((org-draw--routes nil))
    (org-draw--register-routes)
    (should (eq (cdr (assoc '("GET" . "/canvas") org-draw--routes))
                #'org-draw--handle-canvas))
    (should (eq (cdr (assoc '("GET" . "/web") org-draw--routes))
                #'org-draw--handle-canvas))
    ;; existing routes still present
    (should (cdr (assoc '("POST" . "/result") org-draw--routes)))
    (should (cdr (assoc '("GET" . "/session") org-draw--routes)))))

;;;; Transient menu is defined

(ert-deftest orgdraw-v2-menu-defined ()
  (should (commandp 'org-draw-menu))
  (should (commandp 'org-draw-set-background))
  (should (commandp 'org-draw-server-toggle)))


(provide 'org-draw-test)
;;; org-draw-test.el ends here
