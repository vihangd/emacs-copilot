(require 'cl)
(require 'request)
(require 'request-deferred)

(defcustom emacs-copilot-api-key ""
  "Your OpenAI API key."
  :type 'string
  :group 'emacs-copilot)

(defun emacs-copilot-api-request (prompt callback)
  "Make an API request to OpenAI with PROMPT and process the response with CALLBACK."
  (lexical-let ((callback callback))
    (request-deferred
     "https://api.openai.com/v1/completions"
     :type "POST"
     :data (json-encode `(("model" . "text-davinci-003")
                          ("prompt" . ,prompt)
                          ("max_tokens" . 2000)
                          ("n" . 1)
                          ("temperature" . 0.5)))
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(format "Bearer %s" emacs-copilot-api-key)))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                ; (message "Response data: %S" data) ; Add this line to 
                 (funcall callback data)))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys&rest _)
               (message "Error generating code: %S" error-thrown))))))

(defun emacs-copilot-process (instruction &optional no-input)
  "Process the entire buffer or the selected region based on the INSTRUCTION provided.
If NO-INPUT is non-nil, do not use the buffer or selection content as input."
  (interactive "sEnter an instruction (or leave empty for default): ")
  (let* ((input (if no-input
                    ""
                  (if (use-region-p)
                      (buffer-substring (region-beginning) (region-end))
                    (buffer-string))))
         (default-instruction "Fix the grammar and rewrite the following text:")
         (final-instruction (if (string-empty-p instruction)
                                (concat default-instruction "\n" input)
                              (concat instruction "\n" input))))
    (emacs-copilot-api-request
     final-instruction
     (lambda (data)
       (let* ((choices (cdr (assoc 'choices data)))
              (choice (elt choices 0))
              (text (cdr (assoc 'text choice))))
         (with-current-buffer (get-buffer-create "*GPT-Rewritten*")
           (erase-buffer)
           (insert text)
           (pop-to-buffer (current-buffer))))))))


(defun emacs-copilot-confirm-and-eval (code)
  "Display the generated CODE and ask for confirmation before evaluating it."
  (with-current-buffer (get-buffer-create "*Emacs Copilot Code*")
    (erase-buffer)
    (insert code)
    (pop-to-buffer (current-buffer))
    (when (y-or-n-p "Evaluate the generated code? ")
      (eval-buffer))
    (kill-buffer)))


(defun emacs-copilot-generate-code (task)
  "Generate Emacs Lisp code using OpenAI API with the given TASK."
  (let ((prompt (concat "Generate emacslisp code which can be evaluated for the following task: " task)))
    (emacs-copilot-api-request
     prompt
     (lambda (data)
       (let* ((choices (cdr (assoc 'choices data)))
              (choice (elt choices 0))
              (text (cdr (assoc 'text choice))))
         (emacs-copilot-confirm-and-eval text))))))

(defun emacs-copilot ()
  "Ask the user for a task and generate Emacs Lisp code using GPT-3.5."
  (interactive)
  (let ((task (read-string "Enter the task you want to generate code for: ")))
    (emacs-copilot-generate-code task)))

(defun emacs-copilot-generate (instruction)
  "Generate content based on given instruction"
  (interactive "sEnter an instruction (or leave empty for default): ")
  (emacs-copilot-process instruction t))
