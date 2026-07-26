;;; teyjus-mode.el --- Major mode for Teyjus Lambda Prolog -*- lexical-binding: t; -*-

;; Author: Kamal Aboul-Hosn (original), Andrew Gacek (Teyjus 2.0 reduction)
;; Maintainer: you
;; Version: 3.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, lambda-prolog, teyjus
;; URL: https://github.com/teyjus/teyjus

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A modernized major mode for editing Teyjus Lambda Prolog `.mod' and
;; `.sig' files.  Derived from the original teyjus.el shipped with
;; Teyjus, rewritten to use `define-derived-mode', lexical binding,
;; autoload cookies, customization groups, and the standard
;; `compilation-mode' machinery (so `next-error' / `M-g n' work
;; natively).
;;
;; Rewritten by claude with prompt:
;;
;; Turn the lambda prolog emacs mode at
;; https://raw.githubusercontent.com/teyjus/teyjus/refs/heads/master/emacs/teyjus.el
;; into a modern emacs module that I can load cleanly using doom emacs and
;; integrate correctly into my configuration.
;;
;; Features:
;;   - Syntax highlighting for .mod and .sig files
;;   - `teyjus-compile' (C-c C-c): compile the current module with tjcc
;;     in a compilation buffer with clickable errors
;;   - `teyjus-simulate' (C-c C-z): run tjsim on the module in comint
;;   - next/previous error navigation (C-c C-n / C-c C-p)
;;   - imenu support for type and kind declarations
;;
;; Customize `teyjus-tjcc-executable' / `teyjus-tjsim-executable' if
;; the binaries are not on your PATH.


;;; Code:

(require 'compile)
(require 'comint)
(require 'easymenu)

(defgroup teyjus nil
  "Major mode for Teyjus Lambda Prolog."
  :group 'languages
  :prefix "teyjus-")

(defcustom teyjus-tjcc-executable "tjcc"
  "Path to the Teyjus compiler (tjcc)."
  :type 'string)

(defcustom teyjus-tjsim-executable "tjsim"
  "Path to the Teyjus simulator (tjsim)."
  :type 'string)

(defcustom teyjus-mode-hook nil
  "Hook run after entering `teyjus-mode'."
  :type 'hook)

;;;; Syntax table

(defvar teyjus-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; /* ... */ block comments (style a), % line comments (style b)
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?% "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?< "w" table)
    (modify-syntax-entry ?> "w" table)
    (modify-syntax-entry ?\' "w" table)
    table)
  "Syntax table for `teyjus-mode'.")

;;;; Font lock

(defconst teyjus--builtin-keywords
  '(
    "abs"
    "accum_sig"
    "accumulate"
    "arctan"
    "ceil"
    "chr"
    "close_in"
    "close_out"
    "cos"
    "eof"
    "exportdef"
    "fail"
    "floor"
    "flush"
    "func"
    "import"
    "infix"
    "infixl"
    "infixr"
    "input"
    "input_line"
    "int_to_string"
    "is"
    "kind"
    "ln"
    "local"
    "lookahead"
    "namespace"
    "nil"
    "not"
    "open_append"
    "open_in"
    "open_out"
    "open_string"
    "output"
    "pi"
    "postfix"
    "postfixl"
    "pred"
    "prefix"
    "prefixr"
    "print"
    "printterm"
    "rabs"
    "read"
    "readterm"
    "real_to_string"
    "sigma"
    "sin"
    "size"
    "sqrt"
    "std_err"
    "std_in"
    "std_out"
    "string_to_int"
    "string_to_term"
    "substring"
    "term_to_string"
    "true"
    "truncate"
    "type"
    "useonly"
    )
  "Built-in keywords and functions of Teyjus Lambda Prolog.")

(defconst teyjus--builtin-types
  '("int" "real" "string" "list" "out_stream" "in_stream" "o")
  "Built-in types of Teyjus Lambda Prolog.")

(defvar teyjus-font-lock-keywords
  `(;; Variables (capitalized identifiers)
    ("\\<[A-Z][A-Za-z0-9'/]*\\>" . font-lock-variable-name-face)
    ;; Cut
    ("!" . font-lock-warning-face)
    ;; Module/signature headers
    (,(regexp-opt '("module" "sig") 'words) . font-lock-function-name-face)
    ;; Builtins
    (,(regexp-opt teyjus--builtin-keywords 'words) . font-lock-keyword-face)
    ;; Types
    (,(regexp-opt teyjus--builtin-types 'words) . font-lock-type-face))
  "Font-lock keywords for `teyjus-mode'.")

;;;; Compilation support

(defconst teyjus-error-regexp
  "^\\(.+\\)(\\([0-9]+\\),\\([0-9]+\\)).*\\(?:Error\\|\\(Warning\\)\\)"
  "Regexp matching tjcc error and warning messages.")

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               `(teyjus ,teyjus-error-regexp 1 2 3 (4)))
  (add-to-list 'compilation-error-regexp-alist 'teyjus))

(defun teyjus--module-file ()
  "Return the file name of the current buffer, or signal an error."
  (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
      (user-error "Buffer is not visiting a file")))

(defun teyjus-compile ()
  "Compile the current module with tjcc in a compilation buffer.
Errors are clickable and integrate with `next-error'."
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save %s before compiling? " (buffer-name))))
    (save-buffer))
  (let ((default-directory (file-name-directory buffer-file-name))
        (command (concat teyjus-tjcc-executable " "
                         (shell-quote-argument (teyjus--module-file)))))
    (compilation-start command nil (lambda (_mode) "*tjcc*"))))

(defun teyjus-simulate ()
  "Run the Teyjus simulator (tjsim) on the current module in a comint buffer.
The module must already be compiled with `teyjus-compile'."
  (interactive)
  (let* ((module (file-name-sans-extension (teyjus--module-file)))
         (default-directory (file-name-directory buffer-file-name))
         (buffer (make-comint "tjsim" teyjus-tjsim-executable nil module)))
    (pop-to-buffer buffer)))

;;;; Keymap and menu

(defvar teyjus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'teyjus-compile)
    (define-key map (kbd "C-c C-z") #'teyjus-simulate)
    (define-key map (kbd "C-c C-n") #'next-error)
    (define-key map (kbd "C-c C-p") #'previous-error)
    map)
  "Keymap for `teyjus-mode'.")

(easy-menu-define teyjus-mode-menu teyjus-mode-map
  "Menu for `teyjus-mode'."
  '("Teyjus"
    ["Compile current module" teyjus-compile t]
    ["Run simulator (tjsim)" teyjus-simulate t]
    "---"
    ["Next compilation error" next-error t]
    ["Previous compilation error" previous-error t]))

;;;; Imenu

(defvar teyjus-imenu-generic-expression
  '(("Types" "^\\s-*type\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
    ("Kinds" "^\\s-*kind\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1))
  "Imenu expressions for `teyjus-mode'.")

;;;; Mode definition

;;;###autoload
(define-derived-mode teyjus-mode prog-mode "Teyjus"
  "Major mode for editing Teyjus Lambda Prolog files.

\\{teyjus-mode-map}"
  :syntax-table teyjus-mode-syntax-table
  (setq-local font-lock-defaults '(teyjus-font-lock-keywords))
  (setq-local comment-start "% ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "%+\\s-*\\|/\\*+\\s-*")
  (setq-local imenu-generic-expression teyjus-imenu-generic-expression)
  (when buffer-file-name
    (setq-local compile-command
                (concat teyjus-tjcc-executable " "
                        (shell-quote-argument
                         (file-name-nondirectory buffer-file-name))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mod\\'" . teyjus-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sig\\'" . teyjus-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elpi\\'" . teyjus-mode))

(provide 'teyjus-mode)

;;; teyjus-mode.el ends here
