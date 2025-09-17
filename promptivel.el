;;; promptivel.el --- Emacs client for promptivd -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Miguel Guedes

;; Author: Miguel Guedes <miguel@softgeist.com>
;; URL: https://github.com/midsbie/promptivel
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: tools, convenience
;; URL: https://github.com/softgeist/promptivel

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Minimal, conformant client for sending buffer/region snippets to promptivd.

;;; Code:

(require 'json)
(require 'url)
(require 'subr-x)

(defgroup promptivel nil
  "Client for promptivd relay."
  :group 'tools
  :prefix "promptivel-")

(defcustom promptivel-server-url "http://127.0.0.1:8787"
  "HTTP/1.1 endpoint for promptivd /v1/insert."
  :type 'string)

(defcustom promptivel-timeout-seconds 15
  "Synchronous HTTP request timeout in seconds."
  :type 'integer)

(defcustom promptivel-include-file-path-p t
  "When non-nil, prefix the snippet with a location line.
If visiting a file, include the absolute file path.
Otherwise, include the buffer name."
  :type 'boolean)

(defcustom promptivel-fence-enabled-p t
  "When non-nil, wrap the snippet in fence delimiters."
  :type 'boolean)

(defcustom promptivel-fence-begin-string "```"
  "Fence opener used when `promptivel-fence-enabled-p' is non-nil."
  :type 'string)

(defcustom promptivel-fence-end-string "```\n---"
  "Fence closer used when `promptivel-fence-enabled-p' is non-nil."
  :type 'string)

(defcustom promptivel-trim-whitespace-p nil
  "When non-nil, trim leading/trailing whitespace from the snippet before sending.
Default is nil to avoid mutating user content."
  :type 'boolean)

(defcustom promptivel-default-placement 'bottom
  "Default placement advertised to the sink.
One of `top', `bottom', or `cursor'."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Cursor" cursor)))

(defvar-local promptivel-placement promptivel-default-placement
  "Current placement selection for this buffer.")

(defvar promptivel-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'promptivel-select-placement)
    (define-key map (kbd "t") #'promptivel-insert)
    map)
  "Prefix keymap for Promptivel commands.")

(defvar promptivel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") promptivel-prefix-map)
    map)
  "Keymap for `promptivel-mode'.")

;;;###autoload
(define-minor-mode promptivel-mode
  "Global minor mode for sending buffer/region to promptivd.

Bindings (global when mode is enabled):
  C-c t i  Select insertion placement
  C-c t t  Send region or buffer to promptivd"
  :init-value nil
  :lighter " Promptivel"
  :keymap promptivel-mode-map
  :global t)

;;;###autoload
(defun promptivel-select-placement (placement)
  "Set `promptivel-placement' interactively to PLACEMENT.

PLACEMENT is one of the symbols `top', `bottom', or `cursor'."
  (interactive
   (list (pcase (completing-read
                 "Placement: "
                 '("cursor" "top" "bottom") nil t
                 (symbol-name promptivel-placement))
           ("top" 'top)
           ("bottom" 'bottom)
           (_ 'cursor))))
  (setq-local promptivel-placement placement)
  (message "promptivel placement: %s" placement))

;;;###autoload
(defun promptivel-insert (&optional beg end)
  "Send region or buffer to promptivd with selected placement.

When region is active, send text between BEG and END; otherwise send the entire buffer.
Respects user options including fencing, path line, server URL, and timeout."
  (interactive (list (when (use-region-p) (region-beginning))
                     (when (use-region-p) (region-end))))
  (let* ((raw (if (and beg end)
                  (buffer-substring-no-properties beg end)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (when (and (not promptivel-trim-whitespace-p)
               (= (length raw) 0))
      (user-error "Empty snippet"))
    (let* ((text (if promptivel-trim-whitespace-p (string-trim raw) raw))
           (_check (when (string-empty-p text)
                     (user-error "Empty snippet")))
           (snippet (promptivel--format-snippet text))
           (url (promptivel--build-url "/v1/insert"))
           (payload (promptivel--build-payload snippet promptivel-placement))
           (resp (promptivel--http-post-json url payload)))
      (pcase resp
        (`(,code . ,body-str)
         (if (and (integerp code) (<= 200 code) (< code 300))
             (message "promptivel sent: %d bytes, placement=%s, status=%s"
                      (string-bytes snippet) promptivel-placement code)
           (error "promptivel failed: HTTP %s, body: %s" code body-str)))))))

(defun promptivel--format-snippet (text)
  "Return TEXT transformed per user config."
  (let* ((fenced (if promptivel-fence-enabled-p
                     (format "%s\n%s\n%s"
                             promptivel-fence-begin-string
                             text
                             promptivel-fence-end-string)
                   text))
         (loc
          (when promptivel-include-file-path-p
            (cond
             (buffer-file-name buffer-file-name)
             (t (buffer-name))))))
    (if loc
        (format "In %s:\n%s" loc fenced)
      fenced)))

(defun promptivel--build-payload (snippet placement)
  "Build JSON-ready payload with SNIPPET and PLACEMENT symbol."
  (let* ((placement-type (pcase placement
                           ('top "top") ('bottom "bottom") (_ "cursor")))
         (src (let ((h (make-hash-table :test 'equal)))
                (puthash "client" "promptivel" h)
                (puthash "label" "Emacs client" h)
                (when buffer-file-name
                  (puthash "path" buffer-file-name h))
                h))
         (payload (make-hash-table :test 'equal)))
    (puthash "schema_version" "1.0" payload)
    (puthash "source" src payload)
    (puthash "text" snippet payload)
    (puthash "metadata" nil payload)
    (puthash "placement"
             (let ((ph (make-hash-table :test 'equal)))
               (puthash "type" placement-type ph)
               ph)
             payload)
    payload))

(defun promptivel--build-url (&rest paths)
  "Join `promptivel-server-url' URL with PATHS segments, ensuring single slashes."
  (let ((url (replace-regexp-in-string "/+$" "" promptivel-server-url)))
    (dolist (p paths)
      (setq url (concat url "/" (replace-regexp-in-string "^/+" "" p))))
    url))

(defun promptivel--http-post-json (url data)
  "POST DATA as JSON to URL.
Return cons cell (STATUS-CODE . BODY-STRING)."
  (let* ((json-payload (json-encode data))
         ;; Ensure UTF-8, avoid mojibake on non-ASCII
         (payload-utf8 (encode-coding-string json-payload 'utf-8))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json; charset=utf-8")
            ("Accept" . "application/json")))
         (url-request-data payload-utf8)
         (buf (url-retrieve-synchronously url t t promptivel-timeout-seconds)))
    (unless buf
      (error "promptivel: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (let* ((status
                  (or (and (boundp 'url-http-response-status)
                           url-http-response-status)
                      (progn
                        (goto-char (point-min))
                        (when (re-search-forward "^HTTP/1\\.1 \\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1))))))
                 (body-start
                  (or (and (boundp 'url-http-end-of-headers)
                           (integerp url-http-end-of-headers)
                           (1+ url-http-end-of-headers))
                      (progn
                        (goto-char (point-min))
                        (if (re-search-forward "\r?\n\r?\n" nil t)
                            (point)
                          (point-max)))))
                 (body (buffer-substring-no-properties body-start (point-max))))
            (unless (integerp status)
              (error "promptivel: malformed HTTP response"))
            (cons status body)))
      (kill-buffer buf))))

(provide 'promptivel)

;;; promptivel.el ends here
