;;; pine-mode -- Summary
;;; A basic generic-derived mode for editing
;;; Pine files.
(require 'generic-x)

;;; Code:
(define-generic-mode
  'pine-mode
  ;; comments
  '("#")
  ;; keywords
  '("def" "ref" "if" "then" "else" "end"
    "do" "let" "in" "and" "or" "not")
  ;; font-lock identifier highlighting
  '(("[a-zA-Z][a-zA-Z0-9_]*" . 'font-lock-variable-name-face)
    '("[0-9]+" . 'font-lock-constant-face)
    '("[0-9]*\.[0-9]+" . 'font-lock-constant-face)
    '("\"[^\"]\"" . 'font-lock-string-face)
    '("[\+-\*/!><]" . 'font-lock-builtin-face)
    '("(==)|(!=)|(>=)|(<=)|(<-)" . 'font-lock-builtin-face))
  ;; files to load pine-mode
  '("\\.pine\\'")
  ;; functions to run - we have none so far
  nil
  "Major mode for editing Pine files")

(provide 'pine-mode)
;;; pine-mode ends here
