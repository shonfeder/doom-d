;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;;; Code:

;;;; GENERAL
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence 'true)

(setq doom-font (font-spec :family "Source Code Pro" :size 30))

;;; ELIXIR

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))

(def-package! elixir-mode
  :after (lsp-mode))

(def-package! lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "/Users/shonfeder/opt/elixir-ls-release"))

;; TODO
;; (with-eval-after-load 'elixir-mode
;;   (spacemacs/declare-prefix-for-mode 'elixir-mode
;;                                      "mt" "tests" "testing related functionality")
;;   (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
;;                                             "tb" 'exunit-verify-all
;;                                             "ta" 'exunit-verify
;;                                             "tk" 'exunit-rerun
;;                                             "tt" 'exunit-verify-single))
