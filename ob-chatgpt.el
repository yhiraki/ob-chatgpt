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
(require 'org-element)
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

(defun org-babel-chatgpt-initialize ()
  "Initialize."
  (mapc (lambda (a)
          (org-babel-make-language-alias a "chatgpt"))
        org-babel-chatgpt-aliases))

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

(defun org-babel-execute:chatgpt (body params)
  "Execute a block of ChatGPT."
  "\n")

    ;;; old
    ;; (if to-org
    ;;     (progn
    ;;       (let* ((in-file (org-babel-temp-file "chatgpt-md-"))
    ;;              (out-file (org-babel-temp-file "chatgpt-org-"))
    ;;              (cmd (mapconcat
    ;;                    #'identity
    ;;                    `("pandoc"
    ;;                      "-t" "org"
    ;;                      "-f" "markdown"
    ;;                      ,in-file)
    ;;                    " ")))
    ;;         (unless (executable-find "pandoc")
    ;;           (error "Command \"pandoc\" not found"))
    ;;         (with-temp-file in-file (insert (org-babel-chatgpt-add-backticks-spaces result)))
    ;;         (message "%s" cmd)
    ;;         (org-babel-eval cmd "")
    ;;         ))
    ;;   result)))

(defun org-babel-chatgpt-run-hook ()
  "doc"
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info)))
    (when (or (string= lang "chatgpt")
              (member lang org-babel-chatgpt-aliases))
      (let* ((current-thread (cdr (assq :thread (nth 2 info))))
             (model (or (cdr (assq :model (nth 2 info))) org-babel-chatgpt-model))
             (messages (org-babel-chatgpt-get-chat-thread current-thread)))
        (save-excursion
          (goto-char (org-babel-where-is-src-block-result))
          (forward-line)
          (when (org-babel-when-in-src-block)
            (end-of-line)
            (insert (format " :chatgpt-result t :thread %s" current-thread)))
          (forward-line)
          (chatgpt-response-parse-and-insert
           (buffer-name) (point)
           (chatgpt-request chatgpt-url-chat (chatgpt-request-data messages)))))
      )))
  (add-hook 'org-babel-after-execute-hook 'org-babel-chatgpt-run-hook)

(defun org-babel-chatgpt-read-src-block-result-value ()
  "Read result block."
  (let ((c (org-babel-where-is-src-block-result)))
    (when c
      (save-excursion
        (goto-char c)
        (org-babel-read-result)))))

(defun org-babel-chatgpt-get-chat-thread (current-thread &optional current-body)
  "Get all code blocks with CURRENT-THREAD for chat."
  (require 'org-element)
  (let ((blocks '())
        (stop nil))
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (block)
        (unless stop
          (let* ((info (org-babel-get-src-block-info t block))
                 (params (nth 2 info))
                 (role (assq :role params))
                 (thread (cdr (assq :thread params)))
                 (value (org-element-property :value block))
                 (lang (org-element-property :language block))
                 (is-result (cdr (assq :chatgpt-result params)))
                 (body (nth 1 info)))
            (when (and thread
                       (string= current-thread thread))
              (if is-result
                  (push `((:role . assistant) (:content . ,value)) blocks)
                (push `(,role (:content . ,value)) blocks))
              (if (and current-body (string= current-body body))
                  (setq stop t)
                (save-excursion
                  (goto-char (org-element-property :begin block))
                  (let ((result (org-babel-chatgpt-read-src-block-result-value)))
                    (when result
                      (push `((:role . assistant) (:content . ,result)) blocks)
                      )))))))))
    (reverse blocks)))

(org-babel-chatgpt-initialize)

(provide 'ob-chatgpt)

;;; ob-chatgpt.el ends here
