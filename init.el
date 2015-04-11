;; set with to 80 characters
(setq default-frame-alist (append (list
  '(width  . 80))
  default-frame-alist))

;; turn off emacs startup message
(setq inhibit-startup-message t)

;; Tramp
(setq tramp-default-method "ssh")
(setq tramp-verbose 10)
(require 'tramp)
(setcdr (assoc 'tramp-remote-shell (assoc "ssh" tramp-methods)) '("/bin/bash"))
(setcdr (assoc 'tramp-remote-shell (assoc "scp" tramp-methods)) '("/bin/bash"))
(setcdr (assoc 'tramp-remote-shell (assoc "rsync" tramp-methods)) '("/bin/bash"))
;; disable version control
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(clojure-mode-use-backtracking-indent t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(column-number-mode t)
 '(blink-cursor-interval nil))

;; tab width as two, using spaces
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; Mac key bindings

(defun sfp-page-down (&optional arg)
  (interactive "^P")
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun sfp-page-up (&optional arg)
  (interactive "^P")
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "<next>") 'sfp-page-down)
(global-set-key (kbd "<prior>") 'sfp-page-up)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)

;; Switch command and option for emacs key bindings
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)

;; Clojure mode
(load-file "~/.emacs.d/clojure-mode.el")

;; Paredit
(load-file "~/.emacs.d/paredit.el")
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode -1)))

;; Parenthesis highlighting
(load-file "~/.emacs.d/highlight-parenthesis.el")
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("#ff0000" "#0000ff" "#00ff00" "#ff00ff" "#ffff00" "#00ffff"))

;; Macros
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)
(global-set-key (kbd "C-/")        'kmacro-call-macro)

(global-set-key [end]        'move-end-of-line)
(global-set-key [home]       'move-beginning-of-line)

(global-set-key [backspace]  'paredit-backward-delete)
(global-set-key [kp-delete]   'paredit-forward-delete)

(global-set-key "\C-m" 'newline-and-indent)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq frame-title-format '("%f"))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;; set font and current line highlighting
(set-face-attribute 'default nil :family "Menlo" :weight 'normal :height 120)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFD")

;; display pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\(lambda\\)\\>" (0 (prog1 ()
                               (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ?λ))))))
;; turn off scroll-bars
(scroll-bar-mode -1)

;; Smooth scrolling
(load-file "~/.emacs.d/smooth-scrolling.el")
(setq smooth-scroll-margin 3)

;; Auto-complete to Ctrl-Enter
(global-set-key (kbd "C-<return>") 'dabbrev-expand)

;; Previous window
(defun back-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'back-window)

;; Clojure syntax highlighting
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-parens       "#999999"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "#4682b4"   "Clojure brackets")
(defclojureface clojure-keyword      "#2e8b57"   "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#4682b4"   "Clojure special")
(defclojureface clojure-double-quote "#4682b4"   "Clojure double quote")

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("\\btrue\\b\\|\\bfalse\\b\\|\\bnil\\b\\|@\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
            (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))
            (("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t))
            )))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)
(add-hook 'clojure-mode-hook
  (lambda ()
    (set (make-local-variable 'compile-command) "~/bin/lein compile")))

(load-file "~/.emacs.d/nrepl.el")
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
