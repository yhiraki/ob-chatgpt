;;; ob-chatgpt.el --- An org-babel plugin that enables conversing with chatgpt.

;; Author: yhiraki <coffexpr@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/yhiraki/ob-chatgpt
;; Keywords: chatgpt openai
;; Package-Requires: ((emacs "28.1"))
;; License: MIT

;;; Commentary:

;; `ob-chatgpt.el' is a package for using chatgpt in org-mode.

;;; Code:

(require 'chatgpt)
(require 'json)
(require 'ob)
(require 's)

(defcustom org-babel-chatgpt-model "gpt-3.5-turbo"
  "Default model."
  :type 'string
  :group 'org-babel-chatgpt)

(defcustom org-babel-chatgpt-api-token chatgpt-api-token
  "OpenAPI token."
  :type 'string
  :group 'org-babel-chatgpt)

(defvar org-babel-chatgpt-aliases nil)

(defconst org-babel-chatgpt-lang "chatgpt")

(defun org-babel-chatgpt-initialize ()
  "Initialize."
  (mapc (lambda (a)
          (org-babel-make-language-alias a org-babel-chatgpt-lang))
        org-babel-chatgpt-aliases))

(defun org-babel-chatgpt--lang-is-chatgpt (lang)
  (or (string= lang org-babel-chatgpt-lang)
      (member lang org-babel-chatgpt-aliases)))

(defcustom org-babel-chatgpt-aliases '()
  "Aliases."
  :type 'list
  :group 'org-babel-chatgpt
  :set (lambda (var val)
         (set var val)
         (org-babel-chatgpt-initialize)))

(defvar org-babel-default-header-args:chatgpt
  '(
    (:eval . "no-export")
    (:exports . "both")
    (:wrap . "src markdown")
    (:role . "user")
    (:thread . "default")
    (:to-org . nil)
    ))

(defun org-babel-chatgpt-add-backticks-spaces (str)
  "Add spaces around backticks in STR."
  (replace-regexp-in-string " ?`\\([^`'\n]+\\)` ?" " `\\1` " str))

(defun org-babel-execute:chatgpt (_body _params)
  "Execute a block of ChatGPT."
  "\n")

(defun org-babel-chatgpt-convert-result-block ()
  "Use this function when point is on result block."
    (progn
      (let* ((in-file (org-babel-temp-file "chatgpt-md-"))
             (out-file (org-babel-temp-file "chatgpt-org-"))
             (cmd (mapconcat
                   #'identity
                   `("pandoc"
                     "-t" "org"
                     "-f" "markdown"
                     ,in-file)
                   " ")))
        (unless (executable-find "pandoc")
          (error "Command \"pandoc\" not found"))
        (with-temp-file in-file (insert (org-babel-chatgpt-add-backticks-spaces result)))
        (message "%s" cmd)
        (org-babel-eval cmd "")
        ))
    result)

(defun org-babel-chatgpt-update-result (info callback)
  "INFO is a result of 'org-babel-get-src-block-info'.
CALLBACK is run after insert result."
  (let* ((params (nth 2 info))
         (current-thread (cdr (assq :thread params)))
         (model (or (cdr (assq :model params)) org-babel-chatgpt-model))
         (to-org (cdr (assq :to-org params)))
         (messages (org-babel-chatgpt-get-chat-thread current-thread)))
    (message "%s" to-org)
    (save-excursion
      (goto-char (org-babel-where-is-src-block-result))
      (forward-line)
      (when (org-babel-when-in-src-block)
        (end-of-line)
        (insert (format " :chatgpt-result t :thread %s" current-thread)))
      (forward-line)
      (chatgpt-response-parse-and-insert
       (buffer-name) (point)
       (chatgpt-request chatgpt-url-chat (chatgpt-request-data messages) callback)))))

(defun org-babel-chatgpt-run-hook ()
  "Run chatgpt request."
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info)))
    (when (org-babel-chatgpt--lang-is-chatgpt lang)
      (org-babel-chatgpt-update-result
       info
       #'(lambda (_status) (org-babel-chatgpt-convert-result-block))))))
(add-hook 'org-babel-after-execute-hook 'org-babel-chatgpt-run-hook)

(defun org-babel-chatgpt-read-src-block-result-value ()
  "Read result block."
  (let ((c (org-babel-where-is-src-block-result)))
    (when c
      (save-excursion
        (goto-char c)
        (org-babel-read-result)))))

(defun org-babel-chatgpt-get-chat-thread (current-thread &optional all)
  "Get all code blocks with CURRENT-THREAD for chat.

if ALL is not nil, try to collect all messages includes after point."
  (let ((blocks '()))
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (block)
        (let* ((info (org-babel-get-src-block-info t block))
               (params (nth 2 info))
               (role (assq :role params))
               (thread (cdr (assq :thread params)))
               (value (org-element-property :value block))
               (lang (org-element-property :language block))
               (is-result (cdr (assq :chatgpt-result params)))
               (body (nth 1 info))
               (begin (org-element-property :begin block)))
          (when (and
                 (and (not all) (> (point) begin))
                 (and thread (string= current-thread thread)))
            (if is-result
                (push `((:role . assistant) (:content . ,value)) blocks)
              (push `(,role (:content . ,value)) blocks))
            (save-excursion
              (goto-char begin)
              (let ((result (org-babel-chatgpt-read-src-block-result-value)))
                (when result
                  (push `((:role . assistant) (:content . ,result)) blocks)
                  )))))))
    (reverse blocks)))

(org-babel-chatgpt-initialize)

(provide 'ob-chatgpt)

;;; ob-chatgpt.el ends here
