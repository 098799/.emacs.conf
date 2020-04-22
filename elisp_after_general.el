(defun ci--flash-region (start end)
  "This time with a different face"
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'shadow)
    (overlay-put overlay 'priority 100)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))
