;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
                                        ;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
                                        ;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; From elpa/melpa
(package! define-word)
(package! dhall-mode)
(package! elpher)
(package! epresent)
(package! fira-code-mode)
(package! graphviz-dot-mode)
(package! nvm)                          ; Use the right node version
(package! org-clock-csv)
(package! org-contrib)
(package! org-ref)
(package! ox-rss)
(package! poly-markdown)
(package! polymode)
(package! protobuf-mode)
(package! restclient)
(package! systemd)
(package! yaml-mode)
(package! z3-mode)
(package! elpher)
(package! adoc-mode)

;; From source
(package! etymology-of-word :recipe (:type git
                                     :host github
                                     :repo "Camsbury/etymology-of-word"))


(package! maude-mode :recipe (:type git
                              :host github
                              :repo "ssaavedra/maude-mode"))

(package! evil-colemak-basics) ; colemak remaps

;; https://github.com/tecosaur/org-pandoc-import
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))


(package! quint-mode
  :recipe (:type git
           :host github
           :repo "informalsystems/quint"
           :branch "main"
           :files ("editor-plugins/emacs/quint-mode.el")))

(package! lsp-quint
  :recipe (:type git
           :host github
           :repo "informalsystems/quint"
           :branch "main"
           :files ("editor-plugins/emacs/lsp-quint.el")))

(package! ocamlformat
  :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
  :pin "6734dfc1992eb782f0a936ce3cd7c78b7c1d39d3")

(package! tla+-mode
  :recipe (:type git
           :host nil
           :repo "https://git.sdf.org/bch/tlamode"
           :files ("lisp/tla+-mode.el")
           :branch "master"))
