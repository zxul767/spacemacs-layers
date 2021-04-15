;; TODO: add a similar function to work with `labels', `flet' and `macrolet'
(defun clsh/match-with-labels (bound)
  (when (re-search-forward "(\\(with-labels\\)" bound t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (parse-sexp-ignore-comments t)) ; important to ignore lists within comments
      ;; skip the first expression in the `with-labels' call (as it is not a function)
      (goto-char (scan-sexps end 1))
      (let ((functions (--clsh/collect-local-functions-bounds)))
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
                      '--clsh/try-extend-fontification-region-for--with-labels)))

(defun --clsh/try-extend-fontification-region-for--with-labels ()
  "Extend the search region to include all functions in a `with-labels' macro"
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  ;; As fontification *usually* happens top to bottom, we can save some time by
  ;; checking first if `font-lock-end' is inside the `with-labels' form.
  ;;
  ;; However, we cannot remove the second condition because a direct jump backwards
  ;; (e.g., via `goto-char') might trigger fontification in a "window" whose end
  ;; is outside the `with-labels' form, but whose beginning is actually inside.
  (let ((bounds (or (--clsh/inside--with-labels font-lock-end :upper-bound font-lock-beg)
                    (--clsh/inside--with-labels font-lock-beg))))
    (if bounds
        (setf font-lock-beg (min (first bounds) font-lock-beg)
              font-lock-end (max (second bounds) font-lock-end)))))

(cl-defun --clsh/inside--with-labels (point &key upper-bound)
  "Return the bounds of a `with-labels' form if `point' happens to be inside it"
  (save-excursion
    (goto-char point)
    ;; TODO: is this fast enough to guarantee we will not slow down fontification
    ;;       in large files?
    (when (re-search-backward "(\\(with-labels\\)" upper-bound t)
      (let* ((start (match-beginning 0))
             ;; TODO: review the failure conditions for `scan-sexps' and handle
             ;; them properly
             (end (scan-sexps start 1)))
        (if (<= start point end)
            (list start end))))))

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
