;; This is where your customizations should live

;; ################ Global Settings ###################
;; Some of these may belong to packages (not sure)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; revert files when they change
(global-auto-revert-mode t)

;; disable autosaving (e.g. no #filename#)
(setq auto-save-default nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Increase number of undo
(setq undo-limit 100000)

;; Mouse scrolling settings
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed t)

;; start emacs server so that emacsclient opens windows with existing emacs
(require 'server)
(or (server-running-p)
    (server-start))

;; ?????
(global-visual-line-mode)

;; fill column indicator (show a line at a given character number)
;; by default, fci mode uses the fill-column
;; setting fill-column doesn't work either
;; this is the char number where the fill column indicator line will be shown
(setq default-fill-column 80)

;; disable autofill mode (break lines automatically when too long)
(auto-fill-mode -1)
;; this doesn't only apply to text-mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; ################# Global Package Settings #################

;; ace-mode shortcuts
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; buffer move keybindings
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; enable winner mode C-c <left> or C-c <right> to undo/redo window changes
(winner-mode 1)

;; enable move text w/ default bindings (M-up / M-down)
(move-text-default-bindings)

(eval-after-load "paredit"
  '(progn
     ;; removing some default commands in paredit
     ;; (define-key paredit-mode-map (kbd "C-M-f") nil)
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)
     (define-key paredit-mode-map (kbd "<M-R>") nil)
     (define-key paredit-mode-map (kbd "<C-left>") 'paredit-backward)
     (define-key paredit-mode-map (kbd "<C-right>") 'paredit-forward)
     ))

;; yaml mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(autoload 'undo-tree-undo "undo-tree" t)
;; set C-z and C-S-z to undo and redo respectively
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors: disable with <RET> or C-g, newline with C-j
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; defining a macro to kill until the start of a line
(global-set-key (kbd "<S-backspace>") 'delete-horizontal-space)

;; better fuzzy matching
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; make popups take up their own temporary window instead of taking over one
(require 'popwin)
(popwin-mode 1)

;; enable projectile globally
(projectile-global-mode)

;; show git relevant symbols to changed lines
(global-git-gutter-mode)
;; show them without adding an extra column
(require 'git-gutter-fringe)

;; yasnippet (code templates)
(yas-global-mode 1)

;; ################# Custom functions #################

;; When calling kill-ring-save (M-w) or kill-region (C-w) without a
;; selection, assume it means the current line
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position) (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))

;; (un)comment a single line with no region selected and add a shortcut
(defadvice comment-region (before slick-cut activate compile)
  "When called interactively with no active region, comment a single line."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))
(defadvice uncomment-region (before slick-cut activate compile)
  "When called interactively with no active region, comment a single line."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))
(global-set-key (kbd "C-/") 'comment-region)
(global-set-key (kbd "C-?") 'uncomment-region)

;; cycle through mark ring shortcuts (M-` to cycle) (C-` to add mark)
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun split4 ()
  "split frame into 4 windows"
  (interactive)
  (split-window-below)
  (split-window-horizontally)
  (windmove-down)
  (split-window-horizontally)
  (windmove-up))

(defun asciify-string ()
  "Convert region to ASCII string.
   For example:
   “passé” becomes “passe”"
  ;; Code originally by Teemu Likonen
  ;; http://ergoemacs.org/emacs/emacs_zap_gremlins.html
  (interactive)
  ;; NOTE: iconv might not be cross-platform
  (call-process-region (point-min) (point-max) "iconv" t t nil "--to-code=ASCII//TRANSLIT"))

;; ######################## Mode specific ############################

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Clojure
(add-hook 'clojure-mode-hook 'turn-on-fci-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))
(eval-after-load "clojure"
  '(progn
     (require 'cider)
     (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
     (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
     (setq nrepl-hide-special-buffers nil)
     (setq cider-popup-stacktraces t)
     (setq cider-repl-popup-stacktraces t)
     (setq cider-auto-select-error-buffer t)
     (setq nrepl-buffer-name-show-port t)
     (setq cider-repl-history-size 1000) ; the default is 500
     (add-hook 'nrepl-connected-hook
               (defun pnh-clojure-mode-eldoc-hook ()
                 (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
                 (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
                 (cider-enable-on-existing-clojure-buffers)))
                                        ; enabling CamelCase (for integration w/ Java)
     (add-hook 'cider-mode-hook 'subword-mode)

     ;; ac-nrepl setup
     (require 'ac-nrepl)
     (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
     (add-hook 'cider-mode-hook 'ac-nrepl-setup)
     (add-to-list 'ac-modes 'cider-repl-mode)

     ;; using ac-nrepl's popup documentation in place of nrepl-doc
     (eval-after-load "cider"
       '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
     (defun my/ac-nrepl-popup-doc()
       "using ac-nrepl's popup documentation in place of nrepl-doc"
       (local-set-key (kbd "C-c C-d") 'ac-nrepl-popup-doc))
     (add-hook 'cider-repl-mode-hook 'my/ac-nrepl-popup-doc)
     (add-hook 'cider-mode-hook 'my/ac-nrepl-popup-doc)

     ;; put this before projectile is loaded, else projectile keybindings don't work
     ;; (require 'midje-mode)
     ;; (add-hook 'clojure-mode-hook 'midje-mode)

     (add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))

     ;; (define-clojure-indent
     ;;   (provided 0)
     ;;   ;; or (provided 'defun)
     ;;   (fact 1)
     ;;   (facts 1))
     (define-clojure-indent
       (defm 'defun))
     ))

(eval-after-load "js"
  '(progn
     ;; two spaces per indent for javascript
     (setq js-indent-level 2)
     ;; turn on highlight parentheses mode for js
     (add-hook 'js-mode-hook 'highlight-parentheses-mode)))

;; remove shift+arrow key shortcuts in org-mode
(setq org-replace-disputed-keys t)
(eval-after-load "org"
  '(progn
     ;; command for quickly inserting a src block in org-mode
     (defun org-insert-src-block-old (src-code-type)
       "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
       (interactive
        (let ((src-code-types
               '("text"
                 "python"
                 "clojure"
                 "C++"
                 "scala"
                 "markdown"
                 "css"
                 "emacs-lisp"
                 "C"
                 "sh"
                 "java"
                 "js"
                 "julia"
                 "dot"
                 "gnuplot"
                 "octave"
                 "oz"
                 "R"
                 "sass"
                 "screen"
                 "sql"
                 "awk"
                 "haskell"
                 "latex"
                 "lisp"
                 "matlab"
                 "ocaml"
                 "perl"
                 "ruby"
                 "scheme"
                 "sqlite")))
          (list (ido-completing-read "Source code type: " src-code-types))))
       (progn
         ;; (newline-and-indent)
         (insert (format "#+BEGIN_SRC %s\n\n" src-code-type))
         ;; (newline-and-indent)
         (insert "#+END_SRC")
         (previous-line 1)
         (org-edit-src-code)))

     ;; command for quickly inserting a src block in org-mode
     (defun org-insert-src-block ()
       "Insert a source code block in org-mode."
       (interactive)
       (progn
         (insert "#+BEGIN_SRC\n\n#+END_SRC")
         (previous-line 1)))

     ;; org-mode shortcut keys
     (add-hook 'org-mode-hook '(lambda ()
                                 ;; turn on flyspell-mode by default
                                 ;; (flyspell-mode 1)
                                 ;; C-TAB for expanding
                                 (local-set-key (kbd "C-<tab>")
                                                'yas/expand-from-trigger-key)
                                 ;; keybinding for editing source code blocks
                                 (local-set-key (kbd "C-c s e")
                                                'org-edit-src-code)
                                 ;; keybinding for inserting code blocks
                                 (local-set-key (kbd "C-c s i")
                                                'org-insert-src-block)
                                 (local-set-key (kbd "C-c s o")
                                                'org-insert-src-block-old)
                                 ))))

(add-hook 'python-mode-hook 'turn-on-fci-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(eval-after-load "python"
  '(progn
     ))

;; ##################### final things ###################

(split4)

(provide 'user)
;;; user.el ends here
