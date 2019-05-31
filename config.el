;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;;; Code:

;;;; LOCAL SETTINGS

(let ((local-settings "~/.doom.d/local.el"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))

;;;; GENERAL
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence 'true)
(setq-default doom-localleader-key ",")

;;;; FLYSPELL

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))

(require 'lsp-mode)

(setq flycheck-elixir-mix-executable "/Users/shonfeder/.asdf/shims/mix")
(setq elixir-enable-compilation-checking t)

; (add-hook 'elixir-mode-hook #'lsp)

(add-hook! elixir-mode
  (lsp)
  (setq lsp-ui-flycheck-enable t))

;; TODO elixir-mix checker is not working for some reason failing with
;; Suspicious state from syntax checker elixir-mix: Flycheck checker elixir-mix returned non-zero exit code 1, but its output contained no errors: ** (Mix) The task "elixir" could not be found
;; (require 'flycheck-mix)
;; (flycheck-mix-setup)
;; (flycheck-add-next-checker 'elixir-mix 'elixir-credo)
;; (flycheck-add-next-checker 'elixir-mix '(warning . elixir-credo))

;; TODO
;; (with-eval-after-load 'elixir-mode
;;   (spacemacs/declare-prefix-for-mode 'elixir-mode
;;                                      "mt" "tests" "testing related functionality")
;;   (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
;;                                             "tb" 'exunit-verify-all
;;                                             "ta" 'exunit-verify
;;                                             "tk" 'exunit-rerun
;;                                             "tt" 'exunit-verify-single))

;;;; ORG


(add-hook! org-tree-slide-mode
  (setq +org-present-text-scale 3)
  (org-tree-slide-presentation-profile)
  (setq org-tree-slide-skip-outline-level 5))
