(defmacro until-error-happens (&rest body)
  (let ((done (gensym "done")))
    `(catch ',done
       (condition-case e
           (while t
             ,@body)
         (error
          (throw ',done nil))))))

;; TODO: add a similar function to work with `labels', `flet' and `macrolet'
(defun common-lisp/match-with-labels (bound)
  (when (re-search-forward "(\\<\\(with-labels\\)\\>" bound t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (parse-sexp-ignore-comments t)) ; important to ignore lists within comments
      ;; skip the first expression in the `with-labels' call (as it is not a function)
      (goto-char (scan-sexps end 1))
      (set-match-data
       (append
        (list start end start end)
        (nreverse (collect-local-functions-bounds))
        (list (current-buffer))))
      (goto-char end)
      t)))

(defun --collect-local-functions-bounds ()
  (let ((local-functions (list)))
    (until-error-happens
     (save-excursion
       ;; go inside the next local function
       (goto-char (scan-lists (point) 1 -1))
       ;; extract the local function name bounds
       (let ((fn-name-start (point))
             (fn-name-end (scan-sexps (point) 1)))
         (push fn-name-end local-functions)
         (push fn-name-start local-functions)))
     ;; advance to the next local function
     (goto-char (scan-sexps (point) 1)))
    local-functions))

;; TODO: figure out how to make this work for an arbitrary number of matches
;; this may work in practice (as most functions/macros don't need that
;; many helpers, but it's still a kludge)
(font-lock-add-keywords
 'lisp-mode
 `((common-lisp/match-with-labels
    (1 font-lock-keyword-face nil)
    (2 font-lock-function-name-face nil t)
    (3 font-lock-function-name-face nil t)
    (4 font-lock-function-name-face nil t)
    (5 font-lock-function-name-face nil t)
    (6 font-lock-function-name-face nil t)
    (7 font-lock-function-name-face nil t)
    (8 font-lock-function-name-face nil t))))

(provide 'cl-syntax-highlight)
