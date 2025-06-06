;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; WARNING for config.el:
;;; This config is generated from ./config.org

;; A mellow, low-contrast, dark theme
(setq doom-theme 'doom-wilmersdorf)

(setq-default doom-localleader-key ",")

;; Make dired use trash instead of removing things
(setq delete-by-moving-to-trash t)

;; Requires installing FiraCode: https://github.com/tonsky/FiraCode
(setq doom-font (font-spec :family "Fira Code Light" :size 22))

;; Workaround for bug preventing bulk edits via wgrep
;; See https://github.com/mhayashi1120/Emacs-wgrep/issues/36
(setq consult-grep-max-columns 1000)

(defun my/doom-config-file (f)
  "A file path relative to the doom user dir"
  (file-name-concat doom-user-dir f))

(defun my/load-if-exists (f)
  "Load the given file, if it exists"
  (if (file-exists-p f)
      (load-file f)))

(setq my/using-external-monitor 't)

(defun my/toggle-monitor-settings ()
  "Toggle font etc. between external monitors"
  (interactive)
  (setq my/using-external-monitor (not my/using-external-monitor))
  (if my/using-external-monitor
      (setq doom-font (font-spec :family "Fira Code Light" :size 24))
    (setq doom-font (font-spec :family "Fira Code Light" :size 18)))
  ;; (doom/reload)
  ;; (sleep-for 1)
  (doom/reload-theme))

;; Colemacs keys for jumping
(global-evil-colemak-basics-mode) ; Enable colemak rebinds

(setq my/colemak-home-row '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
;; see https://github.com/abo-abo/avy/wiki/defcustom
(setq avy-keys my/colemak-home-row)

(after! ace-window
  ;; https://github.com/abo-abo/ace-window#aw-keys
  (setq aw-keys my/colemak-home-row))

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

 :gnvme "C-n" #'next-line
 :gnvme "C-e" #'previous-line

 (:prefix ("t". "text")
  :desc "Align Regexp" :nv "a" #'align-regexp
  (:prefix ("l" . "lookup")
   :desc "Define word" :nv "d" #'define-word-at-point
   :desc "Etymology of word" :nv "e" #'etymology-of-word-at-point)
  (:prefix ("t" . "transpose")
   :desc "Words" :nv "w" #'transpose-words
   :desc "Lines" :nv "l" #'transpose-lines
   :desc "Paras" :nv "p" #'transpose-paragraphs)
  (:prefix ("s" . "surround")
   :desc "Word with \"" :n "'" #'my/surround-word-with-quotes))

 :n "C-;" #'iedit-mode

 :leader "d" #'save-buffer

 :leader (:prefix ("F". "frame")
          :desc "Switch other frame" "o" #'other-frame
          :desc "Create new frame" "n" #'new-frame)

 :leader (:prefix ("j". "jump")
          :desc "Jump Forward" :nv "j" #'evil-jump-forward
          :desc "Jump Backward" :nv "J" #'evil-jump-backward
          :desc "Show Jumps" :nv "s" #'evil-show-jumps)

 :leader (:prefix ("w". "window")
                  "m" #'evil-window-left
                  "i" #'evil-window-right
                  "e" #'evil-window-up
                  "n" #'evil-window-down
                  "M" #'+evil/window-move-left
                  "I" #'+evil/window-move-right
                  "E" #'+evil/window-move-up
                  "N" #'+evil/window-move-down)

 :leader "gp" #'magit-push
 :leader "gM" #'magit-remote
 :leader "tm" #'my/toggle-monitor-settings

 ;; eww browser launching
 :leader "e" #'eww
 ;; RSS reader start
 :leader "r" #'newsticker-show-news)

(add-hook! dired-mode
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'"  "xdg-open")
          ("\\.PDF\\'"  "xdg-open")
          ("\\.djvu\\'" "xdg-open")
          ("\\.docx\\'" "xdg-open")
          ("\\.DOCX\\'" "xdg-open")
          ("\\.csv\\'" "xdg-open"))))

(after! dirvish
  (setq dirvish-hide-details t)
  (setq! dirvish-quick-access-entries
         `(("h" "~/"                          "Home")
           ("e" ,user-emacs-directory         "Emacs user directory")
           ("d" "~/Downloads/"                "Downloads")
           ("t" "~/.local/share/Trash/files/" "Trash"))))

(map!
 :map dired-mode-map
 :localleader "e" #'wdired-change-to-wdired-mode)

(setq newsticker-url-list
      '(("framasoft" "https://rss.framasoft.org")
        ("ocaml.discourse" "https://discuss.ocaml.org/latest.rss")
        ("Igor Konnov" "https://konnov.github.io/protocols-made-fun/feed.xml")
        ("Proof Society - Comments" "https://www.proofsociety.org/comments/feed/")
        ("Proof Society - Entries" "https://www.proofsociety.org/entries/feed/")
        ("Arch News" "https://archlinux.org/feeds/news/")
        ("Pxtl.ca" "https://pxtl.ca/rss.xml")
        ))

(map!
 :map newsticker-treeview-mode-map
 :n "q" 'newsticker-treeview-quit)

(map!
 :mode eww-mode
 :desc "Back"         :n "M" #'eww-back-url
 :desc "Forward"      :n "I" #'eww-next-url)

(add-hook! writegood-mode
           ;; be little
           (writegood-passive-voice-turn-off))

(add-hook! text-mode
           ;; automatic line breaking
           (auto-fill-mode 1))

(defun transform-thing-at-point (thing-type f)
  (let* ((bounds (bounds-of-thing-at-point thing-type))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (newtext (funcall f text)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert newtext))))

(defun my/surround-word-with-quotes ()
  "Surround the symbol at point in quotaton marks."
  (interactive)
  (transform-thing-at-point 'symbol (lambda (word) (format "%s%s%s" "\"" word "\""))))

;; https://github.com/tecosaur/org-pandoc-import
(use-package! org-pandoc-import :after org)

;;;; FIXME Unduing https://github.com/hlissner/doom-emacs/issues/2393
(define-key!
  [remap org-set-tags-command]     #'org-set-tags-command)

;; View the org TODOs but just for the current agenda
(defun my/org-agenda-current-buffer ()
  (interactive)
  (let ((og-org-agenda-files org-agenda-files)
        (tmp-org-agenda-files (list (buffer-file-name (current-buffer)))))
    (setq org-agenda-files tmp-org-agenda-files)
    (org-todo-list)
    (setq org-agenda-files og-org-agenda-files)))

(map!
 :map (org-mode-map)
 :after evil-colemak-basics
 :localleader :desc "Org Columns" "C" #'org-columns
 :localleader :desc "org-clock-display" "c D" #'org-clock-display
 :localleader :desc "Buffer Todo List" "T" #'my/org-agenda-current-buffer
 :localleader (:prefix ("s" . "subtree")
               :desc "Archive"       "a" #'org-archive-subtree
               :desc "Move up"       "e" #'org-move-subtree-up
               :desc "Move down"     "n" #'org-move-subtree-down
               :desc "Demote"        "i" #'org-demote-subtree
               :desc "Promote"       "m" #'org-promote-subtree
               :desc "Narrow toggle" "t" #'org-toggle-narrow-to-subtree
               :desc "Todo Tree"     "T" #'org-show-todo-tree)
 :localleader (:prefix ("v" . "view")
               :desc "Toggle latex" "l" #'org-latex-preview))

(defun my/org-file (f)
  (concat (file-name-as-directory org-directory) f))

(add-hook! org-mode
  (setq my-tarides-org "~/Sync/tarides/notes/notes.org")
  (setq org-directory "~/Dropbox/org")
  (setq org-modules '(ol-bibtex org-collector))


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
         my-tarides-org))

  (setq org-refile-targets
        `((nil :maxlevel . 3)           ; Support refiling in the current file
          (,(my/org-file "notes.org") :maxlevel . 3)
          (,(my/org-file "scheduled.org") :level . 1)
          (,(my/org-file "eventual.org") :level . 1)
          (,my-tarides-org :level . 1)))

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
(setq org-cite-global-bibliography '("~/Dropbox/bibliography/references.bib"))

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

  ;; org-site config
  ;; See https://blog.tecosaur.com/tmio/2021-07-31-citations.html#basic-usage
  (setq org-cite-export-processors '((t csl)))

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

;; https://github.com/nobiot/org-transclusion/issues/126#issuecomment-1694159821
(defun org-transclusion-content-insert-add-overlay (beg end)
  "Add fringe after transclusion."
  (overlay-put (text-clone-make-overlay beg end (current-buffer))
               'line-prefix
               (org-transclusion-propertize-transclusion))
  (overlay-put (text-clone-make-overlay beg end (current-buffer))
               'wrap-prefix
               (org-transclusion-propertize-transclusion)))

;; https://github.com/nobiot/org-transclusion
(use-package! org-transclusion
  :after org
  :init
  (map!
   :map (org-mode-map)
   :localleader
   :prefix ("u" . "transclUde")

   :desc "Mode" "t" #'org-transclusion-mode
   :desc "Deactivate" "D" #'org-transclusion-deactivate
   :desc "Refresh" "f" #'org-transclusion-refresh

   ;; Adding
   :desc "Add" "a" #'org-transclusion-add
   :desc "Add all" "A" #'org-transclusion-add-all
   :desc "Add From link" "l" #'org-transclusion-make-from-link

   ;; Removing
   :desc "Remove all" "r" #'org-transclusion-remove
   :desc "Remove all" "R" #'org-transclusion-remove-all

   ;; Live sync
   :desc "Start live sync" "s" #'org-transclusion-live-sync-start
   :desc "Stop live sync" "S" #'org-transclusion-live-sync-exit

   ;; Navigating
   :desc "Open source" "o" #'org-transclusion-move-to-source

   ;; Subtrees
   :desc "Promote Subtree" "m" #'org-transclusion-promote-subtree
   :desc "Demote Subtree" "i" #'org-transclusion-demote-subtree)
  :config
  (add-hook 'before-save-hook #'org-transclusion-refresh)
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (custom-set-faces! `(org-transclusion-fringe ; the backwards tick as opposed to apostrophe is *crucial*
                       :foreground ,(doom-color 'green)
                       :background ,(doom-color 'green))))

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

(defun md-format-github-url (url)
  (let ((parts
         (string-split
          (string-remove-prefix "https://github.com/" url) ; no op if prefix is not present
          "/")))
    (pcase parts
      (`(,org ,repo ,_ ,id) (format "[%s/%s#%s](%s)" org repo id url))
      (_ (error (format "point was not on a github url, instead found `%s`" url))))))

(defun my/format-github-url ()
  (interactive)
  (transform-thing-at-point 'url 'md-format-github-url))

(defun md-format-github-user (user)
  "Turn a mention like `@someone` into a link to `https://github.com/someone`"
  (let ((name (string-remove-prefix "@" user)))
    (format "[@%s](https://github.com/%s)" name name)))

(defun my/format-github-handel ()
  (interactive)
  (transform-thing-at-point 'symbol 'md-format-github-user))

(add-hook!
 prog-mode
 (which-function-mode 1)
 (after! spell-fu
   ;; Ensure spell-fu works in prog-modes
   (setq spell-fu-faces-include
         '(font-lock-comment-face
           font-lock-doc-face
           font-lock-string-face
           tree-sitter-hl-face:comment
           tree-sitter-hl-face:string
           tree-sitter-hl-face:string.special))))

;; FIXME is this the result of a regression in doom-emacs?
;; MAGIT
;;
(map!
 :map magit-status-mode-map
 :n "<tab>" 'magit-section-toggle
 :n "f" 'magit-fetch
 :n "F" 'magit-pull)

(add-hook! eglot-managed-mode
           ;; disable eglot inlays
           (eglot-inlay-hints-mode -1))

(map!
 :map lsp-mode-map
 :leader
 :desc "Find in other window" "c O" #'xref-find-definitions-other-window)

(add-hook! lsp-mode
           ;; disable lens overlays
           :append (setq lsp-lens-enable nil))

(map!
 :map flycheck-mode-map
 :localleader
 (:prefix ("e" . "error")
  :desc "list" "e" #'flycheck-list-errors))

;; Don't automatically format in nxml-mode, since it breaks org-export of htmlized source code
;; (add-to-list '+format-on-save-enabled-modes 'nxml-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'mhtml-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'rjsx-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'json-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'js2-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'markdown-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'sh-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'gfm-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'tuareg-mode t)
;; (add-to-list '+format-on-save-enabled-modes 'dune-mode t)
;; (setopt
;;  +format-on-save-enabled-modes
;;  )

;; TODO Refactor
(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))
(add-to-list 'auto-mode-alist '("dune-project\\'" . dune-mode))

(my/load-if-exists (my/doom-config-file "ocaml-defaults.el"))

;; (if (file-exists-p (my/doom-config-file "ocaml-defaults.el"))
;;     (load-file "~/.config/doom/ocaml-defaults.el"))

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
         ;; (compilation-directory
         ;;  (or (locate-dominating-file buffer-file-name "Makefile") nil))
         )
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

(defun my/ocaml-eglot-construct ()
  "Construct a term, making a hole first if needed"
  (interactive)
  (if (equal (symbol-at-point) '_)
      (ocaml-eglot-construct)
    (progn (save-excursion (insert "_"))
           (ocaml-eglot-construct))))

;; The same require added by opam user-setup
(if (file-exists-p "~/.emacs.d/opam-user-setup.el")
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))

(add-hook! tuareg-mode

           :local (prettify-symbols-mode -1)

           (opam-update-env (projectile-project-root))

           ;; Don't insert new comment indicators on new lines
           (setq +evil-want-o/O-to-continue-comments nil)

           (setq dune-watch-minor-mode 't)

           (custom-set-variables
            '(indent-tabs-mode nil)
            '(compilation-context-lines 2)
            '(compilation-error-screen-columns nil)
            '(compilation-scroll-output t)
            '(compilation-search-path (quote (nil "src")))
            '(electric-indent-mode nil)
            '(next-line-add-newlines nil)
            '(require-final-newline t)
            '(sentence-end-double-space nil)
            '(show-trailing-whitespace t)
            '(visible-bell t)
            '(show-paren-mode t)
            '(next-error-highlight t)
            '(next-error-highlight-no-select t)
            '(backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
            '(ac-use-fuzzy nil)
            '(line-move-visual t)))


(add-hook! merlin-mode
  (custom-set-faces!
    '(merlin-eldoc-occurrences-face
      :backgrond "grey15")))

(add-hook! dune-watch-minor-mood
  (setq dune-watch-command-format
        "opam exec -- dune %s --watch --terminal-persistence=clear-on-rebuild"))

(use-package! ocaml-eglot
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure)
  :config

  (setq ocaml-eglot-construct-with-local-values 't)

  (add-to-list '+lookup-documentation-functions #'ocaml-eglot-document)
  (add-to-list '+lookup-references-functions #'ocaml-eglot-occurences)
  (add-to-list '+lookup-type-definition-functions #'ocaml-eglot-find-declaration)
  (add-to-list '+lookup-definition-functions #'ocaml-eglot-find-definition))

(map! :after ocaml-eglot
      :map doom-leader-code-map
      :desc "Document identifier" "K" #'ocaml-eglot-document-identifier
      :desc "Rename" "r" #'ocaml-eglot-rename)

(map!
 :map (tuareg-mode-map)
 :after ocaml-eglot

 :localleader
 :desc "Type enclosing"  :n "t" #'ocaml-eglot-type-enclosing
 :desc "Run ocamlformat" :n "f" #'ocamlformat
 :desc "Construct"       :n "c" #'my/ocaml-eglot-construct
 :desc "Deconstruct"     :n "C" #'ocaml-eglot-destruct
 :desc "Search"          :n "s" #'ocaml-eglot-search

 (:prefix ("d" . "dune")
  :desc "Check"                   :n "c" 'my/ocaml-compile-check
  :desc "Build"                   :n "b" 'my/ocaml-compile-build
  :desc "Test"                    :n "T" 'my/ocaml-compile-test
  :desc "Dune Watch"              :n "w" 'dune-watch-minor-mode
  :desc "Visit dune file"         :n "d" #'my/jump-to-dune-file
  :desc "Visit dune-project file" :n "P" #'my/jump-to-dune-project-file
  :desc "Promote"                 :n "p" 'dune-promote)

 (:prefix ("h" . "hole")
  :desc "Next hole" :n "n" 'ocaml-eglot-hole-next
  :desc "Prev hole" :n "p" 'ocaml-eglot-hole-prev)

 (:prefix ("e" . "error")
  :desc "Next error"       :n "n" 'ocaml-eglot-error-next
  :desc "Prev error "      :n "p" 'ocaml-eglot-error-prev)

 (:prefix ("y" . "yank")
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

(add-hook! fstar-mode
           ;; sync the opam environment to work with sandboxed install of fstar
           (add-hook 'mode-local-init-hook (lambda () (tuareg-opam-update-env nil)))
           (add-hook 'find-file-hook (lambda () (tuareg-opam-update-env nil))))

(my/load-if-exists "~/lib/teyjus/emacs/teyjus.el")

(add-hook! z3-mode
  (setq z3-solver-cmd "/bin/env z3"))

;; Disable terrible unicode replacements for types
(add-to-list '+ligatures-in-modes 'scala-mode 'append)
(setq +ligatures-extras-in-modes '(not scala-mode))

(map!
 :map (python-mode-map)
 :localleader
 :desc "poetry mode" "p" #'poetry)

(let ((local-settings (my/doom-config-file "local.el")))
  (if (file-exists-p local-settings)
      (load-file local-settings)))
