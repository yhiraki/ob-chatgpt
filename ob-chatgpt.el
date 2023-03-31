(require 'json)
(require 'ob)

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

(defun org-babel-chatgpt-execute-command (cmd)
  "Exec CMD and extract response."
  (unless org-babel-chatgpt-api-token
	(error "API TOKEN is not set.  Please set a value for `org-babel-chatgpt-api-token`"))
  (let* ((url-request-method "POST")
		 (url-request-extra-headers
		  `(("Content-Type" . "application/json")
			("Authorization" . ,(format "Bearer %s" org-babel-chatgpt-api-token))))
		 (url-request-data (json-encode-alist
							`(:model ,org-babel-chatgpt-model :messages ,(org-babel-chatgpt-get-all-code-blocks)))))
	(message url-request-data)
	(with-current-buffer (url-retrieve-synchronously "https://api.openai.com/v1/chat/completions")
	  (goto-char (point-min))
	  (search-forward-regexp "\n\n")
	  (let* ((response-json (buffer-substring-no-properties (point) (point-max)))
			 (data (json-read-from-string response-json)))
		(message response-json)
		(cdr (assq 'content (car (aref (cdr (assq 'choices data)) 0))))))))

(defun org-babel-execute:chatgpt (body params)
  "Execute a block of ChatGPT."
  (let ((result (org-babel-chatgpt-execute-command (org-babel-chatgpt-build-command body))))
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
