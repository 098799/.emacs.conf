(defun python-add-breakpoint ()
  "Adding a breakpoint to a python code."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (forward-line)
  (unless (eolp)
    (end-of-line))
  (smart-newline))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (smart-newline))

(defun my-copy-word (arg)
  (interactive "P")
  (save-excursion
    (cua-set-mark)
    (right-word)
    (cua-copy-region arg)
    )
  )

(defun my-backward-copy-word (arg)
  (interactive "P")
  (save-excursion
    (cua-set-mark)
    (left-word)
    (cua-copy-region arg)
    )
  )

(defun copy-whole-line (arg)
  "Copy whole line"
  (interactive "P")
  (save-excursion
    (move-beginning-of-line arg)
    (cua-set-mark)
    (forward-line)
    (cua-copy-region arg)
    )
  )

(defun copy-whole-line-or-region (arg)
  "Copy line or region"
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (cua-copy-region arg)
      (copy-whole-line arg)
      )
    )
   )

(defun kill-whole-line-or-region (arg)
  "Kills line or region"
  (interactive "P")
  (if (region-active-p)
      (cua-cut-region arg)
    (kill-whole-line)
    )
  )

(defun paste-in-new-line (arg)
  "Pasting for full line edits"
  (interactive "P")
  (move-beginning-of-line arg)
  (forward-line)
  (cua-paste arg)
  )

(defun my-backward-word (arg)
  "Go backward by word unless doing so would put you in another line.
  Then, move to the beginning of the line."
  (interactive "P")
  (let ((wrong-flag 0))
    (let ((line-before-move (line-number-at-pos)))
      (save-excursion
        (backward-word)
        (when (/= line-before-move (line-number-at-pos))
          (setq-local wrong-flag 1)
          )
        )
      (if (= wrong-flag 1)
          (progn
            (let ((column-before-move (current-column)))
              (defvar column-after-back-to-indentation)
              (setq-local column-after-back-to-indentation
                          (save-excursion
                            (back-to-indentation)
                            (current-column)
                            )
                          )
              (if (= column-before-move column-after-back-to-indentation)
                  (backward-word)
                (back-to-indentation))))
        (backward-word))))
  )

(defun my-forward-word (arg)
  "Go forward by word unless doing so would put you in another line.
  Then, move to the end of the line."
  (interactive "P")
  (let ((wrong-flag 0))
    (let ((line-before-move (line-number-at-pos)))
      (save-excursion
        (forward-word)
        (when (/= line-before-move (line-number-at-pos))
          (setq-local wrong-flag 1)
          )
        )
      (if (= wrong-flag 1)
          (progn
            (let ((column-before-move (current-column)))
              (defvar column-after-end-of-line)
              (setq-local column-after-end-of-line
                          (save-excursion
                            (move-end-of-line arg)
                            (current-column)
                            )
                          )
              (if (= column-before-move column-after-end-of-line)
                  (forward-word)
                (move-end-of-line arg))))
        (forward-word))))
  )
