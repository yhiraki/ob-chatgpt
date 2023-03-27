(defun ob-chat-ai-build-command (body)
  (mapconcat
   #'shell-quote-argument
   `("curl" "https://api.openai.com/v1/chat/completions"
	 "-s"
	 "-H" "Content-Type: application/json"
	 "-H" ,(format "Authorization: Bearer %s" org-ai-openai-api-token)
	 "-d" ,(json-encode-alist
			`(:model "gpt-3.5-turbo" :messages [(:role "user" :content ,body)])
			)
 ) " ")
  )

(defun ob-chat-ai-execute-command (cmd)
  "Exec CMD and extract response."
  (let* ((result (shell-command-to-string (ob-chat-ai-build-command body)))
		 (response (json-read-from-string result)))
	(cdr (assq 'content (car (aref (cdr (assq 'choices response)) 0))))
	))

(defun org-babel-execute:chat-ai (body params)
  "Execute a block of Python code with org-babel."
  (ob-chat-ai-execute-command (ob-chat-ai-build-command body)))
