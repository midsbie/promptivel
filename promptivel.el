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
(require 'project)

(defgroup promptivel nil
  "Client for promptivd relay."
  :group 'tools
  :prefix "promptivel-")

(defconst promptivel--session-policy-alist
  '(("Reuse or create" . reuse_or_create)
    ("Reuse only"      . reuse_only)
    ("Start fresh"     . start_fresh)))

(defcustom promptivel-server-url "http://127.0.0.1:8787"
  "HTTP/1.1 endpoint for promptivd /v1/insert."
  :type 'string)

(defcustom promptivel-timeout-seconds 15
  "Synchronous HTTP request timeout in seconds."
  :type 'integer)

(defcustom promptivel-include-file-path-p t
  "When non-nil, prefix the snippet with a location line.
If visiting a file, include the file path according to
`promptivel-use-relative-path-p'.  Otherwise, include the buffer name."
  :type 'boolean)

(defcustom promptivel-use-relative-path-p t
  "When non-nil, use relative file paths when file path is enabled.
Relative paths are computed from the project root using `project.el'.
If no project root can be determined, falls back to buffer name."
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

(defcustom promptivel-default-session-policy 'reuse_or_create
  "Default session policy advertised to the sink.
One of `reuse_or_create', `reuse_only', or `start_fresh'."
  :type '(choice (const :tag "Reuse or create" reuse_or_create)
                 (const :tag "Reuse only" reuse_only)
                 (const :tag "Start fresh" start_fresh)))

(defcustom promptivel-selected-provider nil
  "Selected provider for targeting specific sinks.
When nil, user will be prompted to select from available providers."
  :type '(choice (const :tag "None selected" nil)
                 (string :tag "Provider name")))

(defcustom promptivel-providers-cache-seconds 30
  "How long to cache provider list before refetching."
  :type 'integer)

(defvar-local promptivel-placement promptivel-default-placement
  "Current placement selection for this buffer.")

(defvar-local promptivel-session-policy promptivel-default-session-policy
  "Current session policy for this buffer.")

(defvar promptivel--available-providers nil
  "Cached list of providers from last successful /v1/providers request.")

(defvar promptivel--providers-cache-time nil
  "Timestamp of last successful providers fetch.")

(defvar promptivel-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'promptivel-select-placement)
    (define-key map (kbd "s") #'promptivel-select-session-policy)
    (define-key map (kbd "p") #'promptivel-select-provider)
    (define-key map (kbd "i") #'promptivel-insert)
    map)
  "Prefix keymap for Promptivel commands.")

(defvar promptivel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") promptivel-prefix-map)
    map)
  "Keymap for `promptivel-mode'.")

;;;###autoload
(define-minor-mode promptivel-global-mode
  "Global minor mode for sending buffer/region to promptivd.

Bindings (global when mode is enabled):
  C-c t l  Select insertion placement
  C-c t s  Select session policy
  C-c t p  Select provider
  C-c t i  Send region or buffer to promptivd"
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
                 (format "Placement [%s]: " promptivel-placement)
                 '("cursor" "top" "bottom") nil t nil)
           ("top" 'top)
           ("bottom" 'bottom)
           (_ 'cursor))))
  (setq-local promptivel-placement placement)
  (message "promptivel placement: %s" placement))

;;;###autoload
(defun promptivel-select-session-policy (policy)
  "Set `promptivel-session-policy' interactively to POLICY."
  (interactive
   (let* ((choice (completing-read
                   (format "Session policy [%s]: "
                           (promptivel--policy->label promptivel-session-policy))
                   (mapcar #'car promptivel--session-policy-alist)
                   nil t nil)))
     (list (promptivel--label->policy choice))))
  (setq-local promptivel-session-policy policy)
  (message "promptivel session policy: %s" policy))

;;;###autoload
(defun promptivel-select-provider (provider)
  "Set `promptivel-selected-provider' interactively to PROVIDER.

PROVIDER is a string matching one of the available providers from the sink."
  (interactive
   (list (let ((providers (promptivel--get-providers)))
           (if providers
               (completing-read
                (format "Provider [%s]: " promptivel-selected-provider)
                providers nil t nil)
             (user-error "No providers available - is the sink connected?")))))
  (setq promptivel-selected-provider provider)
  (message "promptivel provider: %s" provider))

;;;###autoload
(defun promptivel-insert (&optional beg end)
  "Send region or buffer to promptivd.

When region is active, send text between BEG and END; otherwise send the
entire buffer.  Respects user options including fencing, path line, server
URL, and timeout."
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
           (_provider (promptivel--ensure-provider))
           (snippet (promptivel--format-snippet text))
           (url (promptivel--build-url "/v1/insert"))
           (payload (promptivel--build-payload snippet))
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
             (buffer-file-name (promptivel--get-file-path))
             (t (buffer-name))))))
    (if loc
        (format "In %s:\n%s" loc fenced)
      fenced)))

(defun promptivel--get-file-path ()
  "Return the file path for the current buffer according to user preferences.
Returns absolute path by default, or relative path from project root
when `promptivel-use-relative-path-p' is non-nil."
  (if (and promptivel-use-relative-path-p buffer-file-name)
      (let ((project-root (promptivel--get-project-root)))
        (if project-root
            (file-relative-name buffer-file-name project-root)
          ;; or should we return buffer-file-name instead?
          (buffer-name)))
    buffer-file-name))

(defun promptivel--get-project-root ()
  "Return the project root directory or nil if not in a project."
  (when-let ((project (project-current)))
    (project-root project)))

(defun promptivel--placement-type-string (placement)
  "Return PLACEMENT as a JSON-ready string or nil."
  (pcase placement
    ('top "top")
    ('bottom "bottom")
    ;; Deliberately not handling invalid values
    (_ "cursor")))

(defun promptivel--session-policy-string (policy)
  "Return POLICY as a JSON-ready string or nil."
  (pcase policy
    ('reuse_only "reuse_only")
    ('start_fresh "start_fresh")
    ('reuse_or_create "reuse_or_create")
    (_ (error "promptivel: invalid session policy: %S" policy))))

(defun promptivel--policy->label (sym)
  (car (rassoc sym promptivel--session-policy-alist)))

(defun promptivel--label->policy (label)
  (or (cdr (assoc label promptivel--session-policy-alist))
      (error "promptivel: unknown session policy label: %S" label)))

(defun promptivel--build-payload (snippet)
  "Build JSON-ready payload with SNIPPET."
  (let* ((src (let ((h (make-hash-table :test 'equal)))
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
             (let ((ph (make-hash-table :test 'equal))
                   (placement-type (promptivel--placement-type-string promptivel-placement)))
               (puthash "type" placement-type ph)
               ph)
             payload)
    (let* ((policy-str (promptivel--session-policy-string promptivel-session-policy))
           (nondefault-policy (not (string= policy-str "reuse_or_create")))
           (need-target (or promptivel-selected-provider nondefault-policy)))
      (when need-target
        (let ((th (make-hash-table :test 'equal)))
          (when promptivel-selected-provider
            (puthash "provider" promptivel-selected-provider th))
          (when nondefault-policy
            (puthash "session_policy" policy-str th))
          (puthash "target" th payload))))
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

(defun promptivel--get-providers ()
  "Fetch list of available providers from the daemon.
Returns cached list if recent, otherwise fetches fresh data."
  (let ((now (float-time)))
    (when (or (null promptivel--providers-cache-time)
              (> (- now promptivel--providers-cache-time) promptivel-providers-cache-seconds))
      (condition-case err
          (let ((resp (promptivel--http-get-json (promptivel--build-url "/v1/providers"))))
            (pcase resp
              (`(,code . ,body-str)
               (if (and (integerp code) (<= 200 code) (< code 300))
                   (let* ((parsed (json-parse-string body-str :object-type 'hash-table))
                          (providers (gethash "providers" parsed)))
                     (setq promptivel--available-providers (append providers nil))
                     (setq promptivel--providers-cache-time now))
                 (message "promptivel: failed to fetch providers: HTTP %s" code)))))
        (error
         (message "promptivel: error fetching providers: %s" (error-message-string err)))))
    promptivel--available-providers))

(defun promptivel--ensure-provider ()
  "Ensure a provider is selected, prompting user if necessary.
Returns the selected provider string or nil if none available."
  (cond
   (promptivel-selected-provider promptivel-selected-provider)
   (t
    (let ((providers (promptivel--get-providers)))
      (when providers
        (let ((selected (completing-read "Select provider: " providers nil t)))
          (setq promptivel-selected-provider selected)
          selected))))))

(defun promptivel--http-get-json (url)
  "GET JSON from URL.
Return cons cell (STATUS-CODE . BODY-STRING)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          '(("Accept" . "application/json")))
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
