(defun ci--flash-region (start end)
  "This time with a different face"
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'shadow)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))

(define-minor-mode vterm-copy-mode
  "Toggle vterm copy mode."
  :group 'vterm
  :lighter " VTermCopy"
  :keymap vterm-copy-mode-map
  (if vterm-copy-mode
      (progn                            ;enable vterm-copy-mode
        (ryo-modal-on)
        (use-local-map nil)
        (vterm-send-stop)
        (setq vterm--copy-saved-point (point)))
    (if vterm--copy-saved-point
        (goto-char vterm--copy-saved-point))
    (use-local-map vterm-mode-map)
    (vterm-send-start)
    (global-hl-line-mode 1)))

;;;###autoload
(defun vterm (&optional buffer-name)
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer (or buffer-name "vterm"))))
    (with-current-buffer buffer
      (hl-line-mode -1)
      (vterm-mode))
    (switch-to-buffer buffer)))
