;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;;; Code:

;;;; LOCAL SETTINGS

(let ((local-settings "~/.doom.d/local.el"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))

;;;; GENERAL

(setq my/using-external-monitor 't)

(defun my/toggle-monitor-settings ()
  "Toggle font etc. between external monitors"
  (interactive)
  (setq my/using-external-monitor (not my/using-external-monitor))
  (if my/using-external-monitor
      (setq doom-font (font-spec :family "Fira Code Light" :size 26))
    (setq doom-font (font-spec :family "Fira Code Light" :size 18)))
  ;; (doom/reload)
  ;; (sleep-for 1)
  (doom/reload-theme))
(my/toggle-monitor-settings)

(setq doom-theme 'doom-ephemeral)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence 'true)
(setq-default doom-localleader-key ",")
(setq auth-sources '("~/.authinfo"))

;; TODO Refactor
(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))
(add-to-list 'auto-mode-alist '("dune-project\\'" . dune-mode))

;; Colemak keys for jumping
(setq my/colemak-home-row '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
;; see https://github.com/abo-abo/avy/wiki/defcustom
(setq avy-keys my/colemak-home-row)
(after! ace-window
  ;; https://github.com/abo-abo/ace-window#aw-keys
  (setq aw-keys my/colemak-home-row))

;;;; TEXT MANIPULATION FUNCTIONS
(fset 'surround-word-with-quotes
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ysiW\"" 0 "%d")) arg)))


;; From https://emacs.stackexchange.com/a/34882/293
(defun my/add-visual-replacement (from to)
  "Make `prettify-symbols-mode' replace string FROM with string TO.

Updates `prettify-symbols-alist'.  You may need to toggle
`prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons from (let ((composition nil))
                     (dolist (char (string-to-list to)
                                   (nreverse (cdr composition)))
                       (push char composition)
                       (push '(Br . Bl) composition))))
        prettify-symbols-alist))

;;;; General Writing

(add-hook! text-mode
           ;; Sould be using spell-fu now?
           ;; (flyspell-mode 1)
           (auto-fill-mode 1))

;; This is too slow when loading modes
;; Can I figure out a way to load the hook asyncronously?

;; LSP Mode

(map!
 :map lsp-mode-map
 :leader
 :desc "Find in other window" "c O" #'xref-find-definitions-other-window)

(map!
 :map flycheck-mode-map
 :localleader
 :desc "List errors" "e" #'flycheck-list-errors)

;; Don't automatically format in nxml-mode, since it breaks org-export of htmlized source code
;; (add-to-list '+format-on-save-enabled-modes 'nxml-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'mhtml-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'rjsx-mode t)
;; ;; (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'json-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'js2-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'markdown-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'sh-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'gfm-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'tuareg-mode t)

;;;; ORG

;;;  FIXME WTF for some reason the hook isn't working :(
;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry #'org-roam-capture--get-point
;;          "* Routine

;; - [ ] Exercised
;; - [ ] Mediated

;; * Events

;; - %?

;; * [[file:../20201228235441-joy.org][Joy]]

;; -
;; "
;;          :file-name "daily/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n")))

;; https://github.com/tecosaur/org-pandoc-import
(use-package! org-pandoc-import :after org)

;;;; FIXME Unduing https://github.com/hlissner/doom-emacs/issues/2393
(define-key!
  [remap org-set-tags-command]     #'org-set-tags-command)


(map!
 :map (org-mode-map)
 :localleader :desc "Org Columns" "C" #'org-columns
 :localleader (:prefix ("s" . "subtree")
               :desc "Archive"       "a" #'org-archive-subtree
               :desc "Move up"       "e" #'org-move-subtree-up
               :desc "Move down"     "n" #'org-move-subtree-down
               :desc "Demote"        "i" #'org-demote-subtree
               :desc "Promote"       "m" #'org-promote-subtree
               :desc "Narrow toggle" "t" #'org-toggle-narrow-to-subtree)
 :localleader (:prefix ("v" . "view")
               :desc "Toggle latex" "l" #'org-latex-preview))

(defun my/org-file (f)
  (concat (file-name-as-directory org-directory) f))

(add-hook! org-mode
  (setq my-informal-org "~/Sync/informal-systems/org/informal.org")
  (setq org-directory "~/Dropbox/org")

  ;; Configure org-ref
  (require 'org-ref)
  ;; TODO FIX!
  ;; use ivy as completion engine
  ;; (setq org-ref-completion-library 'org-ref-ivy-cite)
  ;; (require 'org-ref-ivy-cite)
  ;; Enable org-ref cite completion using ivy bound to C-c [
  ;; (org-ref-ivy-cite-completion)

  ;; Workaround for https://github.com/hlissner/doom-emacs/issues/3172
  (electric-indent-local-mode -1)

  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file-other-window)
          (wl . wl-other-frame)))
  ;; EXPORT
  ;; Don't use inline css in exported source code
  (setq org-html-htmlize-output-type 'css)
  (setq org-export-allow-bind-keywords 't)
  (setq org-export-with-sub-superscripts nil)

  ;; CLOCK
  ;; Set default column view headings: Task Priority Effort Clock_Summary
  ;; See https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
  (setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-clock-in-switch-to-state "STRT")
  (setq org-duration-format (quote h:mm))
  (setq org-global-properties '(("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00")))

  ;; AGENDA
  (setq org-agenda-files
        (list
         (my/org-file "notes.org")
         (my/org-file "todo.org" )
         (my/org-file "scheduled.org")
         my-informal-org))

  (setq org-refile-targets
        `((nil :maxlevel . 3)           ; Support refiling in the current file
          (,(my/org-file "notes.org") :maxlevel . 3)
          (,(my/org-file "scheduled.org") :level . 1)
          (,(my/org-file "eventual.org") :level . 1)
          (,my-informal-org :level . 1)))

  (setq org-tag-alist
        '(("@synechepedia" . ?s)
          ("@ocaml" . ?o)
          ("#apalache" . ?a)
          ("#audits" . ?x)
          ("#community" . ?c)
          ("#design". ?d)
          ("#devenv" . ?v)
          ("#implementing" . ?i)
          ("#meeting" . ?m)
          ("#planning" . ?l)
          ("#productivity" . ?p)
          ("#research" . ?r)
          ("#support" . ?u)
          ("#plaintext" . ?t)
          ("#DOVES" . ?f)))

  (setf (alist-get "t" org-capture-templates nil nil 'equal)
        '("Inbox todo" entry
          (file+headline +org-capture-todo-file "Inbox")
          "* TODO %?\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   '("c" "Code note" entry
     (file+headline +org-capture-todo-file "Inbox")
     "* TODO %?\n#+begin_src\n%i\n#+end_src\nfile:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))\n%a")
   't)

  (setq org-format-latex-options
        '(:foreground default
          :background default
          :scale 3
          :html-foreground "Black"
          :html-background "Transparent"
          :html-scale 1.0
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  )

;;;  FIXME?
(add-hook! org-tree-slide-mode
  (setq +org-present-text-scale 3)
  (org-tree-slide-presentation-profile)
  (setq org-tree-slide-skip-outline-level 5))


;; org-clock

(map!
 :map (org-mode-map)
 :localleader
 :desc "org-clock-display" "c D" #'org-clock-display)

(defun org-clock-csv-buffer-to-file ()
  "Export a csv of the org-clock entries in the current buffer

Uses `org-clock-csv-to-file'."
  (interactive)
  (let* ((time-now (format-time-string "%Y-%m-%d"))
         (srcfile (buffer-file-name))
         (basename (file-name-base srcfile))
         (arcfile (concat srcfile "_archive"))
         (fname (expand-file-name
                 (concat basename "-org-clock-export-" time-now ".csv"))))
    (org-clock-csv-to-file fname (list arcfile srcfile))
    (message "Exported timesheet to %s from (%s %s)" fname arcfile srcfile)))

;;;; BIBLIOGRAPHY MANAGEMENT

;;    org-ref settings
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

;;;; SYNECHEPEDIA
(defvar synechepedia-dir
  (file-name-as-directory "~/Dropbox/synechepedia"))
(defvar synechepedia-org-dir
  (file-name-as-directory (concat synechepedia-dir "org")))
(defvar synechepedia-site-dir
  (file-name-as-directory (concat synechepedia-dir "shonfeder.github.io")))
(defvar synechepedia-config-file
  (concat synechepedia-org-dir ".publish.el"))


;; see https://emacs.stackexchange.com/a/32654/293
(defun publish-synechepedia ()
  "org-publish from source and push both repos"
  (interactive)
  (require 'synechepedia "~/Dropbox/synechepedia/org/.publish.el")
  (require 'magit)
  (save-buffer)
  ;; Disbale flyspell mode, cause it makes publishing super slow
  (flyspell-mode-off)
  (remove-hook 'text-mode-hook 'flyspell-mode)
  ;; set `t` to force republish all or `f` to only republish changes
  (org-publish-project "synechepedia")
  (cl-labels
      ((push-repo (dir)
                  (cd dir)
                  (magit-run-git "add" "--all")
                  (magit-run-git "commit" "--all"
                                 (format-time-string "--message=Update %F %R"))
                  (let ((current-branch (magit-get-current-branch)))
                    (magit-git-push current-branch
                                    (concat "origin/" current-branch)
                                    nil))))
    (let ((current-dir default-directory))
      (push-repo synechepedia-org-dir)
      (push-repo synechepedia-site-dir)
      (cd current-dir))))


;; GENRAL KEY BINDINGS

(map!
 ;; TODO Switch to 't' leader
 ;; 't' is for "text"
 ;; 'tt' is for "transpose text"
 :n "ttw" #'transpose-words
 :n "ttl" #'transpose-lines
 :n "ttp" #'transpose-paragraphs
 :nv "ta"  #'align-regexp
;;;; 'tl' is for 'text lookup'
 ;; 'tld is for 'text lokup definition'
 :nv "tld" #'define-word-at-point
 ;; 'tle is for 'text lokup etymology'
 :nv "tle" #'etymology-of-word-at-point
 ;; 's' is for "surround" TODO
 ;; :n "ts\"" '(execute-kbd-macro (symbol-function 'surround-word-with-quotes))

 ;; 'g' is for "go to"
 :n "gw" #'evil-avy-goto-word-or-subword-1
 :n "gl" #'evil-avy-goto-line

 :n "C-;" #'iedit-mode

 :leader "d" #'save-buffer
;;;; SPC is for "space"
 ;; :leader (:prefix ("g" . "git")
 ;;           (:prefix ("y" . "yank")
 ;;             :desc "Yank git link" "l" #'git-link
 ;;             :desc "Yank git commit link" "h" #'git-link-homepage))

 :leader (:prefix ("F". "frame")
          :desc "Switch other frame" "o" #'other-frame
          :desc "Create new frame" "n" #'new-frame)

 :leader "gp" #'magit-push
 :leader "tm" #'my/toggle-monitor-settings

 ;; eww browser launching
 :leader "e" #'eww
 )


;; OCaml
;;

;; TODO Add to tuareg mode
(defun my/jump-to-dune-project-file ()
  (interactive)
  (let*
      ((project-root (locate-dominating-file buffer-file-name "dune-project"))
       (dune-file (concat (file-name-as-directory project-root) "dune-project")))
    (find-file-other-window dune-file)))

(defun my/jump-to-dune-file ()
  (interactive)
  (let*
      (
       (dune-root (locate-dominating-file buffer-file-name "dune"))
       (dune-file (concat (file-name-as-directory dune-root) "dune")))
    (find-file-other-window dune-file)))

(defun my/ocaml-compile (cmd)
  (interactive)
  (save-buffer)
  (let* ((default-directory
           (or (locate-dominating-file buffer-file-name "Makefile") default-directory))
         (compile-command (concat "(cd " default-directory " && opam exec -- dune " cmd ")"))
         (compilation-directory
          (or (locate-dominating-file buffer-file-name "Makefile") nil)))
    (recompile)))

(defun my/ocaml-compile-check ()
  (interactive)
  (my/ocaml-compile "build @check"))

(defun my/ocaml-compile-build ()
  (interactive)
  (my/ocaml-compile "build"))

(defun my/ocaml-compile-test ()
  (interactive)
  (my/ocaml-compile "test"))

;; The same require added by opam user-setup
(if (file-exists-p "~/.emacs.d/opam-user-setup.el")
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))

;; (add-hook! prog-mode
;;   ;; Enable fira-code ligatures
;;   (fira-code-mode)
;;   (prettify-symbols-mode-set-explicitly))


(add-hook! tuareg-mode
  (if (file-exists-p "~/lib/ocaml/dune-watch.el")
      (require 'dune-watch "~/lib/ocaml/dune-watch.el"))

  (my/add-visual-replacement "fun" "λ..")

  ;; Don't insert new comment indicators on new lines
  (setq +evil-want-o/O-to-continue-comments nil)

  (setq dune-watch-minor-mode 't)

  ;; ;; Customization to ocaml font faces
  ;; (custom-set-faces!
  ;;   '(tuareg-font-lock-extension-node-face
  ;;     :background nil
  ;;     :foreground "seagreen")
  ;;   '(tuareg-font-lock-constructor-face
  ;;     :foreground "CadetBlue")
  ;;   '(tuareg-font-lock-module-face
  ;;     :foreground "DarkSalmon"
  ;;     :weight light)
  ;;   '(tuareg-font-lock-governing-face
  ;;     :foreground "MistyRose4"
  ;;     :inherit 'italic)
  ;;   '(tuareg-font-lock-operator-face
  ;;     :foreground "SteelBlue"))

  )


(add-hook! merlin-mode
  (custom-set-faces!
    '(merlin-eldoc-occurrences-face
      :backgrond "grey15")))

(add-hook! dune-watch-minor-mood
  (setq dune-watch-command-format
        "opam exec -- dune %s --watch --terminal-persistence=clear-on-rebuild"))
(map!
 :map (tuareg-mode-map)
 :localleader
 :desc "Check"             :n "c" 'my/ocaml-compile-check
 :desc "Build"             :n "b" 'my/ocaml-compile-build
 :desc "Test"              :n "T" 'my/ocaml-compile-test
 :desc "type enclosing"    :n "t" #'merlin-type-enclosing
 :desc "Dune Watch"        :n "w" 'dune-watch-minor-mode
 :desc "Promote"           :n "P" 'dune-promote
 :desc "Next error"        :n "n" 'merlin-error-next
 :desc "Prev error "       :n "p" 'merlin-error-prev
 :desc "ocamlformat"       :n "f" #'ocamlformat
 :desc "dune-project file" :n "d" #'my/jump-to-dune-file
 :desc "dune-project file" :n "D" #'my/jump-to-dune-project-file
 (:prefix ("y". "yank")
  :desc "Yank type" "t" #'merlin-copy-enclosing))

(map!
 :mode dune-mode
 :localleader
 :desc "Alias stanza"           :n "a" #'dune-insert-alias-form
 :desc "Copy files stanza"      :n "c" #'dune-insert-copyfiles-form
 :desc "Env stanza"             :n "E" #'dune-insert-env-form
 :desc "Executable stanza"      :n "e" #'dune-insert-executable-form
 :desc "Ignored subdirs stanza" :n "u" #'dune-insert-ignored-subdirs-form
 :desc "Install stanza"         :n "i" #'dune-insert-install-form
 :desc "Library stanza"         :n "l" #'dune-insert-library-form
 :desc "Test stanza"            :n "t" #'dune-insert-test-form)

;; F*

(add-hook! fstar-mode
           ;; sync the opam environment to work with sandbocked install of fstar
           (add-hook 'find-file-hook (lambda () (tuareg-opam-update-env nil))))

;; λ-Prolog
(if (file-exists-p "~/lib/teyjus/emacs/teyjus.el")
    (load-file "~/lib/teyjus/emacs/teyjus.el"))

;; QUINT
(let ((quintmode "~/Sync/informal-systems/apalache/quint/editor-plugins/emacs/quint-mode.el"))
  (if (file-exists-p quintmode)
      (load-file quintmode)))

;; MARKDOWN

(map!
 :map (markdown-mode-map)
 :localleader (:prefix ("s". "style")
               :desc "Bold" "b" #'markdown-insert-bold
               :desc "Italic" "i" #'markdown-insert-italic
               :desc "Code" "c" #'markdown-insert-code
               :desc "Code Block" "C" #'markdown-insert-gfm-code-block
               :desc "Quote" "q" #'markdown-insert-blockquote
               :desc "Footenote" "f" #'markdown-insert-footnote
               :desc "Strikethru" "s" #'markdown-insert-strike-through))

;; TYPESCRIPT

(add-hook! typescript-mode
  (setq typescript-indent-level 2))

;; DIRED
(add-hook! dired-mode
  (dired-hide-details-mode)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'"  "xdg-open")
          ("\\.PDF\\'"  "xdg-open")
          ("\\.djvu\\'" "xdg-open")
          ("\\.docx\\'" "xdg-open")
          ("\\.DOCX\\'" "xdg-open") )))

(map!
 :map dired-mode-map
 :localleader "e" #'wdired-change-to-wdired-mode)
;; TLA+


(add-hook! z3-mode
  (setq z3-solver-cmd "/bin/env z3"))

;; SCALA
;; Disable terrible unicode replacements for types
(add-to-list '+ligatures-in-modes 'scala-mode 'append)
(setq +ligatures-extras-in-modes '(not scala-mode))

;; PYTHON
;; (add-hook! python-mode
;;   (poetry-tracking-mode 0))

(map!
 :map (python-mode-map)
 :localleader
 :desc "poetry mode" "p" #'poetry)

;; FIXME is this the result of a regression in doom-emacs?
;; MAGIT
;;
(map!
 :map magit-status-mode-map
 :n "<tab>" 'magit-section-toggle)

;; writegood
(add-hook! writegood-mode
  (writegood-passive-voice-turn-off))

;; Rast
(if (file-exists-p "~/Sync/oss/rast/rast/edit/rast-mode.el")
    (require 'rast "~/Sync/oss/rast/rast/edit/rast-mode.el"))

;; Informal Audits stuff
(let ((audit-snips "~/Sync/informal-systems/mvd/audits/audits-internal/guide/snippets/"))
  (if (file-exists-p audit-snips)
      (add-to-list 'yas-snippet-dirs audit-snips 't)))

;; colemaks
(add-hook! evil-colemak-basics-mode
           ;; (setq evil-coleak-basics-layout-mod 'mod-dh) ; Swap "h" and "m"
           (setq evil-colemak-basics-char-jump-commands 'evil-snipe))

(map!
 :after evil-colemak-basics
 :map evil-colemak-basics-keymap
 :n  "J" #'ace-window

 :nv "j" #'evil-avy-goto-word-or-subword-1
 :nv "gn" #'evil-avy-goto-line-below
 :nv "ge" #'evil-avy-goto-line-above

 ;; fixes for DH
 :nvm "m" #'evil-backward-char
 :nvm "h" #'evil-set-marker
 )

(global-evil-colemak-basics-mode) ; Enable colemak rebinds
