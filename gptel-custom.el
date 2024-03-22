(setq long-prompt "You are an AI assistant named ChatGPT. Please respond concisely and helpfully.")
(setq short-prompt "Please answer the query **as briefly as possible**, without any comments. Your answer will be directly used as a piece of code, or a command. It should work without any postprocessing.")
(setq continue-prompt "Please complete the code in the file you're presented in the context. Don't return any code you've already seen, only the continuation. Your answer will be directly used as a piece of code, don't add any comments, any markdown formatting, just the code itself.")

(defun gptel-send-to-gpt4--general ()
  (interactive)
  (let ((gptel-backend (gptel-make-openai
                         "Custom ChatGPT"
                         :models '("gpt-4-turbo-preview")
                         :key 'gptel-api-key
                         :stream t))
        (gptel-model "gpt-4-turbo-preview")
        (gptel--system-message long-prompt))
    (gptel-send)
    (message "Full buffer or region sent to gpt4")))

(defun gptel-send-to-claude-opus--general ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         "Custom Claude Opus"
                         :models '("claude-3-opus-20240229")
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model "claude-3-opus-20240229")
        (gptel--system-message long-prompt))
    (gptel-send)
    (message "Full buffer or region sent to opus")))

(defun gptel-send-to-claude-haiku--general ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         "Custom Claude Haiku"
                         :models '("claude-3-haiku-20240307")
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model "claude-3-haiku-20240307")
        (gptel--system-message long-prompt))
    (gptel-send)
    (message "Full buffer or region sent to haiku")))

(defun gptel-send-to-gpt4--continue ()
  (interactive)
  (let ((gptel-backend (gptel-make-openai
                         "Custom ChatGPT"
                         :models '("gpt-4-turbo-preview")
                         :key 'gptel-api-key
                         :stream t))
        (gptel-model "gpt-4-turbo-preview")
        (gptel--system-message continue-prompt))
    (gptel-send)
    (message "Continue code with gpt4...")))

(defun gptel-send-to-opus--continue ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         "Custom Claude Opus"
                         :models '("claude-3-opus-20240229")
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model "claude-3-opus-20240229")
        (gptel--system-message continue-prompt))
    (gptel-send)
    (message "Continue code with opus...")))

(defun gptel-send-to-haiku--continue ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         "Custom Claude Haiku"
                         :models '("claude-3-haiku-20240307")
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model "claude-3-haiku-20240307")
        (gptel--system-message continue-prompt))
    (gptel-send)
    (message "Continue code with haiku...")))

(defun gptel-send-to-gpt4--short ()
  (interactive)
  (let ((gptel-backend (gptel-make-openai
                         "Custom ChatGPT"
                         :models '("gpt-4-turbo-preview")
                         :key 'gptel-api-key
                         :stream t))
        (gptel-model "gpt-4-turbo-preview")
        (gptel--system-message short-prompt))
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (gptel-send)
      (delete-region (line-beginning-position) (progn (forward-line 2) (point)))
      )
    (deactivate-mark)
    (message "Sending current line to gpt4...")))

(defun gptel-send-to-claude-opus--short ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         "Custom Claude Opus"
                         :models '("claude-3-opus-20240229")
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model "claude-3-opus-20240229")
        (gptel--system-message short-prompt))
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (gptel-send)
      (delete-region (line-beginning-position) (progn (forward-line 2) (point)))
      )
    (deactivate-mark)
    (message "Sending current line to opus...")))

(defun gptel-send-to-claude-haiku--short ()
  (interactive)
  (let ((gptel-backend (gptel-make-anthropic
                         "Custom Claude Haiku"
                         :models '("claude-3-haiku-20240307")
                         :key 'gptel-anthropic-api-key
                         :stream t))
        (gptel-model "claude-3-haiku-20240307")
        (gptel--system-message short-prompt))
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (gptel-send)
      (delete-region (line-beginning-position) (progn (forward-line 2) (point)))
      )
    (deactivate-mark)
    (message "Sending current line to haiku...")))
