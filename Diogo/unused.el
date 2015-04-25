;; ------
;; global
;; ------

;; set the width and height of emacs when you start it (in characters)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 20) (height . 20)))

;; Place elisp files in this directory. You'll then be able  to load them.
(add-to-list 'load-path "~/.emacs.d/vendor") ;; current

;; always use line number mode
(global-linum-mode)

;; ;; split into verical windows by default
(setq split-width-threshold 0)

;; ;; split horizontally for temporary buffers
(defun split-horizontally-for-temp-buffers ()
       "Split the window horizontally for temp buffers."
       (when (and (one-window-p t)
     	     (not (active-minibuffer-window)))
         (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

;; map escape to C-g
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
(keyboard-translate ?\C-\[ ?\C-g) ; only works on C-[, not escape

;; Disable ESC as meta key; set ESC to C-g, C-ESC to ESC-ESC-ESC
(global-set-key (kbd "C-<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "<escape>") 'keyboard-quit)
;; (global-unset-key (kbd "<escape>"))

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; enable column number mode in text mode
(add-hook 'text-mode-hook (lambda () (column-number-mode 1)))

;; ---------
;; functions
;; ---------

;; Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
;; This is particularly useful under Mac OSX, where GUI apps are not started from a shell.
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; ----------------
;; package specific
;; ----------------

;; sidebar for selecting files
(require 'sr-speedbar)
;; start with speedbar open
(sr-speedbar-open)
;; show all files in speedbar
(speedbar-toggle-show-all-files)
;; disable speedbar icons for folders & files
(setq speedbar-use-images nil)

;; eclipse power + emacs
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

;; for loading julia mode
(require 'ess)
(load "~/.emacs.d/elpa/ess-20140120.43/lisp/ess-site.el")
;; disable ess-electric brace
(eval-after-load "ess"
  '(progn
     (define-key ess-mode-map (kbd "{") nil)))

;; show symbols every few spaces to easily differentiate indentation
(require 'highlight-indentation)
(highlight-indentation-mode)

;; key-chord mode (note: be careful with keyboard macros!)
(require 'key-chord)
(key-chord-mode 1)
;; (key-chord-define-global "//" 'comment-region)
(key-chord-define-global "??" 'uncomment-region)
(key-chord-define-global "zz" 'undo-tree-undo)
(key-chord-define-global "ZZ" 'undo-tree-redo)
;; (key-chord-define-global ";;" "\C-e;")

;; ace jump mode shortcuts
(key-chord-define-global "hh" 'ace-jump-char-mode)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "kk" 'ace-jump-line-mode)

;; easily create docstring in python-mode
(require 'python)
(key-chord-define python-mode-map "\"\"" "\"\"\"\"\"\"\C-b\C-b\C-b")

;; flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; helm (smarter file complete)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)

;; helm w/ projectile
(require 'helm-projectile)

;; ack and a half settings
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; python specific (requires python package)
;; basic elpy with everything enabled
(when (require 'elpy nil t)
  (elpy-enable)
  (elpy-clean-modeline))
