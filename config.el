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

(add-hook 'text-mode-hook 'flyspell-mode)
;; This is too slow when loading modes
;; Can I figure out a way to load the hook asyncronously?
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;;; ORG

(add-hook! org-tree-slide-mode
  (setq +org-present-text-scale 3)
  (org-tree-slide-presentation-profile)
  (setq org-tree-slide-skip-outline-level 5))

(add-hook! org-mode
  (map!
   ;;;; 'S' is for "sub-tree"
   :n ",Sa" #'org-archive-subtree
   :n ",Sk" #'org-move-subtree-up
   :n ",Sj" #'org-move-subtree-down))

;;;;;; BIBLIOGRAPHY MANAGEMENT

;;    org-ref settings
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

;; use ivy as completion engine
(setq org-ref-completion-library 'org-ref-ivy-cite)
;;;; KEY BINDINGS

(map!
 ;;;; 't' is for "text"
 ;; 'tt' is for "transpose text"
 :n "ttw" #'transpose-words
 :n "ttl" #'transpose-lines
 :n "ttp" #'transpose-paragraphs

 ;;;; 'g' is for "go to"
 :n "gw" #'evil-avy-goto-word-or-subword-1
 :n "gl" #'evil-avy-goto-line

 :n "C-;" #'iedit-mode

 ;;;; '/' is for "search"
 :leader (:prefix-map ("/" . "search")
           :desc "Search for thing at point" "t" #'swiper-thing-at-point)

 ;;;; SPC is for "space"
 :leader (:prefix-map ("g" . "git")
           (:prefix-map ("y" . "yank")
             :desc "Yank git link" "l" #'git-link
             :desc "Yank git commit link" "h" #'git-link-homepage))

 :leader (:prefix-map ("F". "frame")
           :desc "Switch other frame" "o" #'other-frame
           :desc "Create new frame" "n" #'new-frame)
 )
