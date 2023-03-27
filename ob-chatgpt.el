(defun ob-chatgpt-build-command (body)
  (mapconcat
   #'shell-quote-argument
   `("curl" "https://api.openai.com/v1/chat/completions"
	 "-s"
	 "-H" "Content-Type: application/json"
	 "-H" ,(format "Authorization: Bearer %s" org-ai-openai-api-token)
	 "-d" ,(json-encode-alist
			`(:model "gpt-3.5-turbo" :messages ,(ob-chatgpt-get-all-code-blocks)))
 ) " ")
  )

(defun ob-chatgpt-execute-command (cmd)
  "Exec CMD and extract response."
  (let* ((result (shell-command-to-string (ob-chatgpt-build-command body)))
		 (response (json-read-from-string result)))
	(cdr (assq 'content (car (aref (cdr (assq 'choices response)) 0))))
	))

(defun org-babel-execute:chatgpt (body params)
  "Execute a block of Chat."
  (ob-chatgpt-execute-command (ob-chatgpt-build-command body)))

(defun ob-chatgpt-get-all-code-blocks ()
  "Get all code blocks for chat."
  (interactive)
  (let ((blocks '()))
	(org-babel-map-src-blocks (buffer-file-name)
	  (let* ((info (org-babel-get-src-block-info))
			 (role (assq :role (nth 2 info)))
			 (value (nth 1 info)))
		(when role
		  (push `(,role (:content . ,value)) blocks))))
	blocks))
