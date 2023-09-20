;;; emacs-copilot.el --- Emacs Copilot: AI-driven code generation and editing -*- lexical-binding: t -*-

;; Author: Vihang D
;; URL: https://github.com/vihangd/emacs-copilot
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Emacs Copilot is a package that provides AI-driven code generation
;; and editing capabilities using OpenAI's API. It includes
;; functions to generate Emacs Lisp code, rewrite the content of a buffer
;; or region, and insert code based on a given instruction.

;;; Code:

(require 'cl)
(require 'request)
(require 'request-deferred)
(require 'magit)

(defcustom emacs-copilot-api-key ""
  "Your OpenAI API key."
  :type 'string
  :group 'emacs-copilot)

(defun emacs-copilot-api-request (instruction callback)
  "Make an API request to OpenAI with the given INSTRUCTION and CALLBACK using request-deferred."
  (let ((headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " emacs-copilot-api-key))))
        (data (json-encode `(("model" . "gpt-3.5-turbo-instruct")
                             ("prompt" . ,instruction)
                             ("max_tokens" . 2000)
                             ("n" . 1)
                             ("temperature" . 0.3)))))
    (request-deferred "https://api.openai.com/v1/completions"
                      :type "POST"
                      :headers headers
                      :data data
                      :parser 'json-read
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  (let* ((choices (cdr (assoc 'choices data)))
                                         (choice (elt choices 0))
                                         (text (cdr (assoc 'text choice))))
                                    (funcall callback text))))
                      :error (cl-function
                              (lambda (&key error-thrown &allow-other-keys)
                                (message "Error: %S" error-thrown)))
                      )))


(defun emacs-copilot-process (instruction &optional no-input callback)
  "Process the entire buffer or the selected region based on the INSTRUCTION provided.
If NO-INPUT is non-nil, do not use the buffer or selection content as input.
When CALLBACK is provided, call it with the resulting text."
  (interactive "sEnter an instruction (or leave empty for default): ")
  (let* ((input (if no-input "" (if (use-region-p) (buffer-substring (region-beginning) (region-end)) (buffer-string))))
         (default-instruction "Fix the grammar and rewrite the following text:")
         (final-instruction (if (string-empty-p instruction) (concat default-instruction "\n" input) (concat instruction "\n" input))))
    (emacs-copilot-api-request final-instruction
                               (lambda (text)
                                 (if callback
                                     (funcall callback text)
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
  (let ((prompt (concat "Generate emacslisp code which can be evaluated as it is for the following task (Note: do not add any quotes): " task)))
    (emacs-copilot-api-request
     prompt
     (lambda (data)
        (emacs-copilot-confirm-and-eval data)))))

(defun emacs-copilot ()
  "Ask the user for a task and generate Emacs Lisp code using OpenAI API."
  (interactive)
  (let ((task (read-string "Enter the task you want to generate code for: ")))
    (emacs-copilot-generate-code task)))

(defun emacs-copilot-generate (instruction)
  "Generate content based on given instruction"
  (interactive "sEnter an instruction (or leave empty for default): ")
  (emacs-copilot-process instruction t))

(defun emacs-copilot-commit ()
  "Generate a commit message for the staged changes in the current project using emacs-copilot-process."
  (interactive)
  (let* (
         (staged-diff (shell-command-to-string "git --no-pager diff --staged"))
         (prompt (concat "Generate a git commit message with first line being a succinct summary of all the changes with no more than 50 characters then separated by 2 new lines give the summary of all the changes in lines where each line is no longer than 72 characters in the following commit :\n\n" staged-diff)))
    (emacs-copilot-process
     prompt
     nil
     (lambda (commit-message)
       (if (stringp commit-message)
           (progn
             (magit-commit-create)
             (let ((counter 0))
               (while (and (not (get-buffer "COMMIT_EDITMSG")) (< counter 100))
                 (sleep-for 0.1)
                 (setq counter (1+ counter))))
             (if (get-buffer "COMMIT_EDITMSG")
                 (with-current-buffer (get-buffer "COMMIT_EDITMSG")
                   (goto-char (point-min))
                   (insert commit-message))
               (error "Error: COMMIT_EDITMSG buffer not available after 10 seconds.")))
         (message "Error: Invalid commit message received from API."))))))


(provide 'emacs-copilot)
