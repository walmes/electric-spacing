;;----------------------------------------------------------------------

(defvar electric-spacing-rules
  '((?= . electric-spacing-self-insert-command)
    (?| . electric-spacing-self-insert-command)
    (?& . electric-spacing-self-insert-command)
    (?< . electric-spacing-<)
    (?> . electric-spacing->)
    (?+ . electric-spacing-+)
    (?- . electric-spacing--)
    (?* . electric-spacing-*)
    (?! . electric-spacing-!)
    (?~ . electric-spacing-~)
    (?, . electric-spacing-\,)
    (?. . electric-spacing-.)
    (?: . electric-spacing-:)
    (?? . electric-spacing-?)
    (?% . electric-spacing-%)
  ))

(defconst electric-spacing-operators-regexp
  (regexp-opt
   (mapcar (lambda (el) (char-to-string (car el)))
           electric-spacing-rules)))

;;;---------------------------------------------------------------------
;;; Fine Tunings - eletric-spacing-* functions.

(defun electric-spacing-\, ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert "," 'after))

(defun electric-spacing-. ()
  "See `electric-spacing-insert'."
  (cond ((and electric-spacing-double-space-docs
              (electric-spacing-document?))
         (electric-spacing-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (and (derived-mode-p 'ess-mode)
                  (looking-back "[a-z]")))
         (insert "."))
        (t
         (electric-spacing-insert "." 'after)
         (insert " "))))

(defun electric-spacing-: ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         (insert ":"))
        (t
         (electric-spacing-insert ":" 'after))))

(defun electric-spacing-? ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         (electric-spacing-insert "?"))
        (t
         (electric-spacing-insert "?" 'after))))

(defun electric-spacing-! ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a != b
         ;; | x <- !y
         ;; | x = !y
         ;; | x & !y
         ;; | x | !y
         ;; | x + !y
         ;; | x * !y
         ;; | y[!a, !b]
         ;; | if (!x) ...
         ;; `----
         (cond ((looking-back "[([] *")
                (insert "!"))
               ((looking-back "[-,=|&*+] *")
                (electric-spacing-insert "!" 'before))))
        (t
         (electric-spacing-insert "!" 'after))))

(defun electric-spacing-% ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a %*% b
         ;; | a %/% b
         ;; | a %% b
         ;; | a %>% b
         ;; | a %o% b
         ;; | a %in% b
         ;; | sprintf("%d %d\n", a, b)
         ;; `----
         (if (or (looking-back "%[*/>]? *")
                 (looking-back "%\\(o\\|in\\) *")
                 (and (looking-back "\".*")
                      (not (looking-back "\",.*"))))
             (electric-spacing-insert "%" 'after)
           (electric-spacing-insert "%" 'before)))
        ;; If this is a comment or string, we most likely
        ;; want no spaces - probably string formatting
        ((and (derived-mode-p 'ess-mode)
              (electric-spacing-document?))
         (electric-spacing-insert "%" 'after))
        (t
         (electric-spacing-insert "%" 'after))))

(defun electric-spacing-> ()
  "See `electric-spacing-insert'."
  (cond ((and (derived-mode-p 'ess-mode)
              (looking-at " *="))
         (electric-spacing-insert ">" 'before))
        (t
         (electric-spacing-insert ">"))))

(defun electric-spacing-< ()
  "See `electric-spacing-insert'."
  (cond ((and (derived-mode-p 'ess-mode)
              (looking-at " *="))
         (electric-spacing-insert "<" 'before))
        (t
         (electric-spacing-insert "<"))))

(defun electric-spacing-* ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a * b
         ;; | a %*% b
         ;; | a**b = a^b
         ;; `----
         (cond ((looking-back "% *")
                (electric-spacing-insert "*" 'middle))
               ((looking-back " \\* ")
                (delete-char -3)
                (insert "^"))
               (t
                (electric-spacing-insert "*"))))
        (t
         (electric-spacing-insert "*"))))

(defun electric-spacing-+ ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a + b
         ;; | y ~ a + b
         ;; | 10e+5
         ;; | 10E+5
         ;; `----
         (if (looking-back "[0-9.]+[eE]")
             (insert "+")
           (electric-spacing-insert "+")))
        (t
         (electric-spacing-insert "+"))))

(defun electric-spacing-- ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a - b
         ;; | a * (-b)
         ;; | a * -b
         ;; | a + -b
         ;; | a & -5 > b
         ;; | a | -5 > b
         ;; | a < -5
         ;; | a > -5
         ;; | y ~ -1+x
         ;; | a = -5
         ;; | a <- -5
         ;; | a[-b, -x]
         ;; | c(1, -2)
         ;; | 10e-5
         ;; | 10E-5
         ;; | 10^-5
         ;; | 10/-5
         ;; | 10/-5
         ;; |    -5
         ;; `----
         (cond ((or (looking-back "[=~,*+<>&|] *")
                    (looking-back "<- *"))
                (electric-spacing-insert "-" 'before))
               ((looking-back "[([{/^] *")
                (insert "-"))
               ((looking-back "[0-9.]+[eE]")
                (insert "-"))
               ((looking-back "^\\s-*")
                (insert "-"))
               (t
                (electric-spacing-insert "-"))))
        ;; exponent notation, e.g. 1e-10: don't space
        ((looking-back "[0-9.]+[eE]")
         (insert "-"))
        ;; a = -9
        ((and
          (looking-back (concat electric-spacing-operators-regexp " *"))
          (not (looking-back "- *")))
         (electric-spacing-insert "-" 'before))
        (t
         (electric-spacing-insert "-"))))

(defun electric-spacing-~ ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a ~ b
         ;; | ~a + b
         ;; | x <- ~a + b
         ;; | x = ~a + b
         ;; | c(~a + b, ~x + y)
         ;; `----
         (cond ((looking-back "\\(<-\\|[=,]\\) *")
                (electric-spacing-insert "~" 'before))
               ((looking-back "( *")
                (insert "~"))
               ((looking-back "^\\s-*")
                (insert "~"))
               (t
                (electric-spacing-insert "~"))))
        (t
         (insert "~"))))

(provide 'electric-spacing-r)

;;; electric-spacing-r.el ends here
