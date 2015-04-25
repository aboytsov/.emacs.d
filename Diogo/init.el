(defvar *emacs-load-start* (float-time))
(require 'package)

;; stable
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)

;; latest dev snapshot
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-ruby
                      clojure-mode
                      clojure-test-mode
                      ;; nrepl
                      cider
                      ac-nrepl
                      ;; midje-mode
                      virtualenv
                      highlight-parentheses
                      smooth-scrolling
                      ;; minimap
                      ;; sr-speedbar
                      ace-jump-mode
                      buffer-move
                      auto-complete
                      undo-tree
                      move-text
                      expand-region
                      multiple-cursors
                      flx-ido
                      highlight-indentation
                      git-gutter-fringe
                      projectile
                      ;; ack-and-a-half
                      helm
                      helm-projectile
                      flycheck
                      haskell-mode
                      scala-mode2
                      ;; key-chord
                      web-mode
                      fill-column-indicator
                      ess
                      yaml-mode
                      markdown-mode
                      ;; elpy
                      ;; clojure-cheatsheet
                      popwin
                      ;; emacs-eclim
                      yasnippet
                      clj-refactor
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load "~/.emacs.d/user.el")

(message ".emacs.d loaded in %fs" (- (float-time) *emacs-load-start*))

(put 'downcase-region 'disabled nil)
