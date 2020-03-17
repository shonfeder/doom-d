;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;;; Code:

;;;; LOCAL SETTINGS

(let ((local-settings "~/.doom.d/local.el"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))

;;;; FONT

(add-hook! prettify-symbols-mode
  (push '(":-"    . ?⟸) prettify-symbols-alist)
  (push '("pi"    . ?∀) prettify-symbols-alist)
  (push '("sigma" . ?∃) prettify-symbols-alist))

(add-hook! prog-mode
           fira-code-mode)


;; FIXME Fallback font for unicode
(setq doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 30))

;;;; GENERAL

(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-unordered-key-sequence 'true)
(setq-default doom-localleader-key ",")

;; TODO Refactor
(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))

;;;; TEXT MANIPULATION FUNCTIONS
(fset 'surround-word-with-quotes
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("ysiW\"" 0 "%d")) arg)))

;;;; FLYSPELL

(add-hook! text-mode
  (flyspell-mode 1))

(add-hook! flyspell-mode
  (setq flyspell-issue-message-flag nil)
  (setq +flyspell-immediately nil))

;; This is too slow when loading modes
;; Can I figure out a way to load the hook asyncronously?
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Don't automatically format in nxml-mode, since it breaks org-export of htmlized source code
(add-to-list '+format-on-save-enabled-modes 'nxml-mode t)
(add-to-list '+format-on-save-enabled-modes 'typescript-mode t)

;;;; ORG

;;;; FIXME Unduing https://github.com/hlissner/doom-emacs/issues/2393
(define-key!
  [remap org-set-tags-command]     #'org-set-tags-command)


(map!
 :map (org-mode-map)
 :localleader :desc "Org Columns" "C" #'org-columns
 :localleader (:prefix ("S" . "subtree")
                :desc "Archive"       "a" #'org-archive-subtree
                :desc "Move up"       "k" #'org-move-subtree-up
                :desc "Move down"     "j" #'org-move-subtree-down
                :desc "Narrow toggle" "n" #'org-toggle-narrow-to-subtree))

(defun my/org-file (f)
  (concat (file-name-as-directory org-directory) f))

(add-hook! org-mode
  (setq my-informal-org "~/Sync/informal-systems/org/informal.org")
  (setq org-directory "~/Dropbox/org")

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

  ;; AGENDA
  (setq org-agenda-files
        (list
         (my/org-file "notes.org")
         (my/org-file "todo.org" )
         (my/org-file "scheduled.org")
         my-informal-org))

  (setq org-refile-targets
        `((nil :maxlevel . 3) ; Support refiling in the current file
          (,(my/org-file "notes.org") :maxlevel . 3)
          (,(my/org-file "scheduled.org") :level . 1)
          (,(my/org-file "eventual.org") :level . 1)
          (,my-informal-org :level . 1)))

  (setq org-tag-alist
        '(("@email" . ?e)
          ("@home" . ?h)
          ("@travel" . ?t)
          ("@work" . ?w)
          ("@synechepedia" . ?s)
          ("#community" . ?c)
          ("#design". ?d)
          ("#implementing" . ?i)
          ("#meeting" . ?m)
          ("#planning" . ?l)
          ("#productivity" . ?p)
          ("#research" . ?r)))
  (setf (alist-get "t" org-capture-templates nil nil 'equal)
        '("Inbox todo" entry
          (file+headline +org-capture-todo-file "Inbox")
          "* TODO %?\n%i\n%a")))

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

;; use ivy as completion engine
(setq org-ref-completion-library 'org-ref-ivy-cite)

;;;; SYNECHEPEDIA
(defvar synechepedia-dir
  (file-name-as-directory "~/Dropbox/synechepedia"))
(defvar synechepedia-org-dir
  (file-name-as-directory (concat synechepedia-dir "org")))
(defvar synechepedia-site-dir
  (file-name-as-directory (concat synechepedia-dir "shonfeder.github.io")))
(defvar synechepedia-config-file
  (concat synechepedia-org-dir ".publish.el"))

(load-file "~/Dropbox/synechepedia/org/.publish.el")


;; see https://emacs.stackexchange.com/a/32654/293
(defun publish-synechepedia ()
  "org-publish from source and push both repos"
  (interactive)
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
      (cd current-dir)))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (turn-on-flyspell))


;;;; KEY BINDINGS

(map!
 ;; TODO Switch to 't' leader
 ;;;; 't' is for "text"
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

 ;;;; 'g' is for "go to"
 :n "gw" #'evil-avy-goto-word-or-subword-1
 :n "gl" #'evil-avy-goto-line

 :n "C-;" #'iedit-mode

 ;;;; '/' is for "search"
 :leader (:prefix ("/" . "search")
           :desc "Search for thing at point" "t" #'swiper-thing-at-point)

 :leader "d" #'save-buffer
 ;;;; SPC is for "space"
 ;; :leader (:prefix ("g" . "git")
 ;;           (:prefix ("y" . "yank")
 ;;             :desc "Yank git link" "l" #'git-link
 ;;             :desc "Yank git commit link" "h" #'git-link-homepage))

 :leader (:prefix ("F". "frame")
           :desc "Switch other frame" "o" #'other-frame
           :desc "Create new frame" "n" #'new-frame)
 )


;; OCaml

(add-hook! tuareg-mode
           ;; run dune build in the correct opam environment
           (setq compile-command "opam exec dune build"))

(map!
 :map (merlin-mode-map)
 :localleader (:prefix ("y". "yank")
                :desc "Yank type" "t" #'merlin-copy-enclosing))


;; The same require added by opam user-setup
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

;; F*

(add-hook! fstar-mode
           ;; sync the opam environment to work with sandbocked install of fstar
           (add-hook 'find-file-hook (lambda () (opam-update-env nil))))

;; λ-Prolog
;; TODO Package up properly
(load-file "~/lib/teyjus/emacs/teyjus.el")

(defun teyjus-prettify-symbols-add ()
  (interactive)

  (push '("="  . ?≐) prettify-symbols-alist)
  (push '("i:" . ?ι) prettify-symbols-alist)
  (push '("o:" . ?ο) prettify-symbols-alist)
  (push '("o"  . ?•) prettify-symbols-alist)
  (push '(";"  . ?|) prettify-symbols-alist)
  (push '("in" . ?∈) prettify-symbols-alist))
