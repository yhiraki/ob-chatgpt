;;; ob-chatgpt.el --- Handle custom path schemes defined in `schemes.txt'

;; Author: yhiraki <coffexpr@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/yhiraki/ob-chatgpt
;; Keywords: chatgpt openai
;; Package-Requires: ((emacs "28.1"))
;; License: MIT

;;; Commentary:

;; `ob-chatgpt.el' is a package for using chatgpt in org-mode.

;;; Code:

(require 'json)
(require 'ob)
(require 's)

(defcustom org-babel-chatgpt-model "gpt-3.5-turbo"
  "Default model."
  :type 'string
  :group 'org-babel-chatgpt)

(defcustom org-babel-chatgpt-api-token nil
  "OpenAPI token."
  :type 'string
  :group 'org-babel-chatgpt)

(defvar org-babel-default-header-args:chatgpt
  '(
	(:eval . "no-export")
	(:exports . "both")
	(:results . "raw")
	(:role . "user")
	(:session . "default")
	))

(defun org-babel-chatgpt-build-command (params)
  "Build a command using BODY for fetch OpenAI API."
  (let* ((info (org-babel-get-src-block-info))
		 (current-session (cdr (assq :session params))))
	(mapcar
	 #'shell-quote-argument
	 `("curl" "https://api.openai.com/v1/chat/completions"
	   "-s"
	   "-H" "Content-Type: application/json"
	   "-H" ,(format "Authorization: Bearer %s" org-babel-chatgpt-api-token)
	   "-d" ,(json-encode-alist
			  `(:model ,org-babel-chatgpt-model :messages ,(org-babel-chatgpt-get-chat-session current-session))))
	 )))

(defun org-babel-chatgpt-execute-command (cmd)
  "Exec CMD and extract response."
  (unless org-babel-chatgpt-api-token
	(error "API TOKEN is not set.  Please set a value for `org-babel-chatgpt-api-token`"))
  (let* ((result (shell-command-to-string cmd))
		 (response (json-read-from-string result)))
	(message "%s" response)
	(cdr (assq 'content (car (aref (cdr (assq 'choices response)) 0))))))

(defun org-babel-execute:chatgpt (body params)
  "Execute a block of ChatGPT."
  (let* ((result (org-babel-chatgpt-execute-command (s-join " " (org-babel-chatgpt-build-command params)))))
	(concat
	 "#+BEGIN_SRC markdown "
	 (format ":session %s :role assistant\n" (cdr (assq :session params)))
	 result "\n"
	 "#+END_SRC\n"
	 )))

(defun org-babel-chatgpt-get-all-code-blocks ()
  "Get all code blocks for chat."
  (interactive)
  (let* ((blocks '())
		 (current-info (org-babel-get-src-block-info))
		 (current-value (nth 1 current-info))
		 (current-session (cdr (assq :session (nth 2 current-info))))
		 (stop nil))
	(org-babel-map-src-blocks (buffer-file-name)
	  (let* ((info (org-babel-get-src-block-info))
			 (role (assq :role (nth 2 info)))
			 (session (cdr (assq :session (nth 2 info))))
			 (value (nth 1 info)))
		(when (and role
				   (not stop)
				   (string= session current-session))
		  (push `(,role (:content . ,value)) blocks))
		(when (string= value current-value)
		  (setq stop t))
		))
	(reverse blocks)))

(defun org-babel-chatgpt-get-chat-session (current-session)
  "Get all code blocks with SESSION for chat."
  (require 'org-element)
  (let ((blocks '())
		(stop nil))
	(org-element-map (org-element-parse-buffer) '(src-block example-block)
	  (lambda (src-block)
		(let* ((info (org-babel-get-src-block-info))
			   (params1 (nth 2 info))
			   (params2 (org-babel-parse-header-arguments (org-element-property :parameters src-block)))  ; TODO: src-block以外の場合はpropertyを取得することができない
			   (params (append params1 params2))
			   (role (assq :role params))
			   (session (cdr (assq :session params)))
			   (value (org-element-property :value src-block)))
		  (when (string= current-session session)
			(push `(,role (:content . ,value)) blocks))
		  (setq current-value (org-element-property :value src-block))
		  ))
	  )
	(reverse blocks)))

(provide 'ob-chatgpt)

;;; ob-chatgpt.el ends here
