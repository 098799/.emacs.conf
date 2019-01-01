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
  (newline-and-indent))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

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
