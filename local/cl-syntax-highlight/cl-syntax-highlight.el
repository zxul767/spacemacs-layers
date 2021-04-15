;; TODO: add a similar function to work with `labels', `flet' and `macrolet'
(defun clsh/match-with-labels (bound)
  (message "clsh with bound %S" bound)
  (when (re-search-forward "(\\(with-labels\\)" bound t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (parse-sexp-ignore-comments t)) ; important to ignore lists within comments
      (message "parsing for local functions")
      ;; skip the first expression in the `with-labels' call (as it is not a function)
      (goto-char (scan-sexps end 1))
      (let ((functions (--clsh/collect-local-functions-bounds)))
        (message "found %S functions" (length functions))
        (message "found functions: %S" functions)
        (set-match-data
         (append
          (list start end start end)
          (nreverse functions)
          (list (current-buffer)))))
      (goto-char end)
      t)))

(defmacro --clsh/until-error-happens (&rest body)
  (declare (indent defun))
  (let ((done (gensym "done")))
    `(catch ',done
       (condition-case e
           (while t
             ,@body)
         (error
          (throw ',done nil))))))

(defun --clsh/collect-local-functions-bounds ()
  (let ((local-functions (list)))
    (--clsh/until-error-happens
      (save-excursion
        ;; go inside the next local function
        (goto-char (scan-lists (point) 1 -1))
        ;; extract the local function name bounds
        (let ((fn-name-start (point))
              (fn-name-end (scan-sexps (point) 1)))
          (push fn-name-start local-functions)
          (push fn-name-end local-functions)))
      ;; advance to the next local function
      (goto-char (scan-sexps (point) 1)))
    local-functions))

(add-hook 'lisp-mode-hook
          (lambda ()
            (setf font-lock-multiline t)
            (add-hook 'font-lock-extend-region-functions
                      '--clsh/font-lock-extend-region-for--with-labels)))

(defun --clsh/font-lock-extend-region-for--with-labels ()
  "Extend the search region to include all functions in a `with-labels' macro"
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (when (re-search-backward "(\\(with-labels\\)" nil t)
      (let* ((start (match-beginning 0))
             (end (scan-sexps start 1))) ; FIXME: what happens if this call fails?
        (setf font-lock-beg (min start font-lock-beg)
              font-lock-end (max end font-lock-end))))))

;; TODO: Figure out how to make this work for an arbitrary number of matches.n
;; This may work in practice (as most functions/macros don't need that many
;; helpers, but it's still a kludge)
(font-lock-add-keywords
 'lisp-mode
 `((clsh/match-with-labels
    (1 font-lock-keyword-face nil)
    (2 font-lock-function-name-face nil t)
    (3 font-lock-function-name-face nil t)
    (4 font-lock-function-name-face nil t)
    (5 font-lock-function-name-face nil t)
    (6 font-lock-function-name-face nil t)
    (7 font-lock-function-name-face nil t)
    (8 font-lock-function-name-face nil t))))

(provide 'cl-syntax-highlight)
