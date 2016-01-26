;;----------------------------------------------------------------------

(require 'cc-mode)
(require 'thingatpt)

(defun electric-spacing-c-types ()
  (concat c-primitive-type-key "?"))

(defvar electric-spacing-rules
  '((?= . electric-spacing-self-insert-command)
    (?< . electric-spacing-<)
    (?> . electric-spacing->)
    (?% . electric-spacing-%)
    (?+ . electric-spacing-+)
    (?- . electric-spacing--)
    (?* . electric-spacing-*)
    (?/ . electric-spacing-/)
    (?& . electric-spacing-&)
    (?| . electric-spacing-self-insert-command)
    (?: . electric-spacing-:)
    (?? . electric-spacing-?)
    (?, . electric-spacing-\,)
    (?~ . electric-spacing-~)
    (?. . electric-spacing-.)
    (?^ . electric-spacing-self-insert-command)))

(defconst electric-spacing-operators-regexp
  (regexp-opt
   (mapcar (lambda (el) (char-to-string (car el)))
           electric-spacing-rules)))

;;----------------------------------------------------------------------
;;; Fine Tunings

(defun electric-spacing-< ()
  "See `electric-spacing-insert'."
  (cond
   ((or (and c-buffer-is-cc-mode
             (looking-back
              (concat "\\("
                      (regexp-opt
                       '("#include" "vector" "deque" "list" "map" "stack"
                         "multimap" "set" "hash_map" "iterator" "template"
                         "pair" "auto_ptr" "static_cast"
                         "dynmaic_cast" "const_cast" "reintepret_cast"

                         "#import"))
                      "\\)\\ *")
              (line-beginning-position)))
        (derived-mode-p 'sgml-mode))
    (insert "<>")
    (backward-char))
   (t
    (electric-spacing-insert "<"))))

(defun electric-spacing-: ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         (if (looking-back "\\?.+")
             (electric-spacing-insert ":")
           (electric-spacing-insert ":" 'middle)))
        ((derived-mode-p 'haskell-mode)
         (electric-spacing-insert ":"))
        ((derived-mode-p 'python-mode) (electric-spacing-python-:))
        ((derived-mode-p 'ess-mode)
         (insert ":"))
        (t
         (electric-spacing-insert ":" 'after))))

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
             (or (and c-buffer-is-cc-mode
                      (looking-back "[a-z]"))
                 (and
                  (derived-mode-p 'python-mode 'ruby-mode)
                  (looking-back "[a-z\)]"))
                 (and
                  (derived-mode-p 'js-mode 'js2-mode)
                  (looking-back "[a-z\)$]"))))
         (insert "."))
        ((derived-mode-p 'cperl-mode 'perl-mode 'ruby-mode)
         ;; Check for the .. range operator
         (if (looking-back ".")
             (insert ".")
           (insert " . ")))
        (t
         (electric-spacing-insert "." 'after)
         (insert " "))))

(defun electric-spacing-& ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----[ cases ]
         ;; | char &a = b; // FIXME
         ;; | void foo(const int& a);
         ;; | char *a = &b;
         ;; | int c = a & b;
         ;; | a && b;
         ;; `----
         (cond ((looking-back (concat (electric-spacing-c-types) " *" ))
                (electric-spacing-insert "&" 'after))
               ((looking-back "= *")
                (electric-spacing-insert "&" 'before))
               (t
                (electric-spacing-insert "&"))))
        (t
         (electric-spacing-insert "&"))))

(defun electric-spacing-* ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a * b;
         ;; | char *a;
         ;; | char **b;
         ;; | (*a)->func();
         ;; | *p++;
         ;; | *a = *b;
         ;; `----
         (cond ((looking-back (concat (electric-spacing-c-types) " *" ))
                (electric-spacing-insert "*" 'before))
               ((looking-back "\\* *")
                (electric-spacing-insert "*" 'middle))
               ((looking-back "^[ (]*")
                (electric-spacing-insert "*" 'middle)
                (indent-according-to-mode))
               ((looking-back "= *")
                (electric-spacing-insert "*" 'before))
               (t
                (electric-spacing-insert "*"))))

        ;; Handle python *args and **kwargs
        ((derived-mode-p 'python-mode)
         ;; Can only occur after '(' ',' or on a new line, so just check
         ;; for those. If it's just after a comma then also insert a space
         ;; before the *.
         (cond ((looking-back ",") (insert " *"))
               ((looking-back "[(,^)][ \t]*[*]?") (insert "*"))
               ;; Othewise act as normal
               (t (electric-spacing-insert "*"))))
        (t
         (electric-spacing-insert "*"))))

(defun electric-spacing-> ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back " - "))
         (delete-char -3)
         (insert "->"))
        (t
         (electric-spacing-insert ">"))))

(defun electric-spacing-+ ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back "\\+ *"))
         (when (looking-back "[a-zA-Z0-9_] +\\+ *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "+" 'middle)
         (indent-according-to-mode))
        (t
         (electric-spacing-insert "+"))))

(defun electric-spacing-- ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back "\\- *"))
         (when (looking-back "[a-zA-Z0-9_] +\\- *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "-" 'middle)
         (indent-according-to-mode))

        ;; exponent notation, e.g. 1e-10: don't space
        ((looking-back "[0-9.]+[eE]")
         (insert "-"))

        ;; a = -9
        ((and (looking-back (concat electric-spacing-operators-regexp " *"))
              (not (looking-back "- *")))
          (electric-spacing-insert "-" 'before))

        (t
         (electric-spacing-insert "-"))))

(defun electric-spacing-? ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         (electric-spacing-insert "?"))
        (t
         (electric-spacing-insert "?" 'after))))

(defun electric-spacing-% ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a % b;
         ;; | printf("%d %d\n", a % b);
         ;; `----
         (if (and (looking-back "\".*")
                  (not (looking-back "\",.*")))
             (insert "%")
           (electric-spacing-insert "%")))
        ;; If this is a comment or string, we most likely
        ;; want no spaces - probably string formatting
        ((and (derived-mode-p 'python-mode)
              (electric-spacing-document?))
         (insert "%"))
        (t
         (electric-spacing-insert "%"))))

(defun electric-spacing-~ ()
  "See `electric-spacing-insert'."
  ;; First class regex operator =~ langs
  (cond ((derived-mode-p 'ruby-mode 'perl-mode 'cperl-mode)
         (if (looking-back "= ")
             (progn
               (delete-char -2)
               (insert "=~ "))
           (insert "~")))
        (t
         (insert "~"))))

(defun electric-spacing-/ ()
  "See `electric-spacing-insert'."
  ;; *nix shebangs #!
  (cond ((and (eq 1 (line-number-at-pos))
              (save-excursion
                (move-beginning-of-line nil)
                (looking-at "#!")))
         (insert "/"))
        (t
         (electric-spacing-insert "/"))))

(defun electric-spacing-enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil if not inside any parens."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun electric-spacing-python-: ()
  (if (and (not (in-string-p))
           (eq (electric-spacing-enclosing-paren) ?\{))
      (electric-spacing-insert ":" 'after)
    (insert ":")))

(provide 'electric-spacing-c++)

;;; electric-spacing-c++.el ends here
