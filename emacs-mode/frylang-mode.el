(setq frylang-keywords '("let" "be" "and" "in" "if" "then" "else" "object" "extends" "fun") )
(setq frylang-emphasis '("this" "super"))
(setq frylang-builtin '())
(setq frylang-constants '("true" "false"))

(setq frylang-keywords-regexp (regexp-opt frylang-keywords 'words))
(setq frylang-emphasis-regexp (regexp-opt frylang-emphasis 'words))
(setq frylang-builtin-regexp (regexp-opt frylang-builtin 'words))
(setq frylang-constants-regexp (regexp-opt frylang-constants 'words))

(setq frylang-keywords nil)
(setq frylang-emphasis nil)
(setq frylang-builtin nil)
(setq frylang-constants nil)

(setq frylang-font-lock-keywords
  `(
    ("\\([;,]\\)[\s-]*]" . (1 font-lock-warning-face))
    (,frylang-emphasis-regexp . font-lock-builtin-face)
    ;;("\\(\\w+\\)[\s-]*\\(@?\\w+\\)*[\s-]*\=" . (1 font-lock-function-name-face))
    ("[[,][
\s-]*\\(\\w+\\)" . (1 font-lock-function-name-face))
    ("object +\\([^\[\s- ]+\\)" . (1 font-lock-type-face))
    ("more +\\([^\[\s- ]+\\)" . (1 font-lock-type-face))
    ("\\(let\\|and\\) +\\([^\s-]+\\) +be" . (2 font-lock-function-name-face))
    (,frylang-keywords-regexp . font-lock-keyword-face)
    (,frylang-builtin-regexp . font-lock-builtin-face)
    (,frylang-constants-regexp . font-lock-string-face)
    ("“\\([^ ]+?\\)”" . (1 font-lock-string-face))
    ("[0-9]+" . (0 font-lock-string-face))
    ("\?" . (0 font-lock-string-face))
    ("[^<>]\\(\\\\\\|=\\|->\\|\\^\\|@\\|~\\)" . (1 font-lock-variable-name-face))   
 ))


(defvar frylang-syntax-table nil "Syntax table for `frylang-mode'.")
(setq frylang-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; comments 
        (modify-syntax-entry ?% "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))


(define-derived-mode frylang-mode fundamental-mode
  "Fry language mode"
  "Major mode for the Fry language"
  :syntax-table frylang-syntax-table

  ;; code for syntax highlighting
  (setq font-lock-defaults '((frylang-font-lock-keywords)))

  ;; clear memory
  (setq frylang-keywords-regexp nil)
  (setq frylang-emphasis-regexp nil)
  (setq frylang-builtin-regexp nil)
  (setq frylang-constants-regexp nil)
)


(provide 'frylang-mode)
