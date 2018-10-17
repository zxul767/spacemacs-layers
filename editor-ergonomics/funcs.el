(defun kill-buffer-unconditionally ()
  "Kills the current buffer without asking any questions."
  (interactive)
  (kill-buffer nil))
