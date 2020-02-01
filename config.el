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

(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))


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

;;;; ORG

(add-hook! org-mode
  (setq org-directory "~/Sync/org")
  ;; Don't use inline css in exported source code
  (setq org-html-htmlize-output-type 'css)
  (setq org-export-allow-bind-keywords 't)
  (setq org-export-with-sub-superscripts nil)
  (setq org-agenda-files
        '("~/Sync/org/notes.org"
          "~/Sync/org/todo.org"
          "~/Sync/org/scheduled.org"))
  (setq org-refile-targets
        '((nil :maxlevel . 3) ; Support refiling in the current file
          ("~/Sync/org/notes.org" :maxlevel . 3)
          ("~/Sync/org/scheduled.org" :level . 1)
          ("~/Sync/org/eventual.org" :level . 1)))
  (setq org-tag-alist
        '(("@email" . ?e)
          ("@home" . ?h)
          ("@errands" . ?r)
          ("@phone" . ?p)
          ("@travel" . ?t)
          ("@work" . ?w)
          ("@synechepedia" . ?s)))
  (setf (alist-get "t" org-capture-templates nil nil 'equal)
        '("Inbox todo" entry
          (file+headline +org-capture-todo-file "Inbox")
          "* TODO %?\n%i\n%a")))

(add-hook! org-tree-slide-mode
  (setq +org-present-text-scale 3)
  (org-tree-slide-presentation-profile)
  (setq org-tree-slide-skip-outline-level 5))

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
 ;;;; 't' is for "text"
 ;; 'tt' is for "transpose text"
 :n "ttw" #'transpose-words
 :n "ttl" #'transpose-lines
 :n "ttp" #'transpose-paragraphs
 :nv "ta"  #'align-regexp

 ;;;; 'g' is for "go to"
 :n "gw" #'evil-avy-goto-word-or-subword-1
 :n "gl" #'evil-avy-goto-line

 :n "C-;" #'iedit-mode

 ;;;; '/' is for "search"
 :leader (:prefix ("/" . "search")
           :desc "Search for thing at point" "t" #'swiper-thing-at-point)

 ;;;; SPC is for "space"
 :leader (:prefix ("g" . "git")
           (:prefix ("y" . "yank")
             :desc "Yank git link" "l" #'git-link
             :desc "Yank git commit link" "h" #'git-link-homepage))

 :leader (:prefix ("F". "frame")
           :desc "Switch other frame" "o" #'other-frame
           :desc "Create new frame" "n" #'new-frame)
 )


;; OCaml

(map!
 :map (merlin-mode-map)
 :localleader (:prefix ("y". "yank")
                :desc "Yank type" "t" #'merlin-copy-enclosing))

(map!
 :map (org-mode-map)
 :localleader (:prefix ("S" . "subtree")
                :desc "Archive"   "a" #'org-archive-subtree
                :desc "Move up"   "k" #'org-move-subtree-up
                :desc "Move down" "j" #'org-move-subtree-down))
