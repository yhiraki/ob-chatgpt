(defun ob-chat-ai-build-command (body)
  (mapconcat
   #'shell-quote-argument
   `("curl" "https://api.openai.com/v1/chat/completions"
	 "-H" "Content-Type: application/json"
	 "-H" ,(format "Authorization: Bearer %s" org-ai-openai-api-token)
	 "-d" "'{ \"model\": \"gpt-3.5-turbo\",\"messages\": [{\"role\": \"user\", \"content\": \"Server-Sent Eventについて教えて\"}] }'"
 ) " ")
  )
(ob-chat-ai-build-command "a")

(defun org-babel-execute:chat-ai (body params)
  "Execute a block of Python code with org-babel."
  (shell-command-to-string (ob-chat-ai-build-command body)))

(require 'json)
(json-encode-alist '(:model "gpt-3.5-turbo"))
