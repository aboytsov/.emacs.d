;; -- Set window width/height
(setq default-frame-alist (append (list
                                   '(width  . 80)
                                   '(height . 54))
  default-frame-alist))

;; -- Various settings
(custom-set-variables
 '(inhibit-startup-message t)
 '(blink-cursor-interval nil)
 '(clojure-mode-use-backtracking-indent t)
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(frame-title-format '("%f"))
 '(default-tab-width 2)
 '(indent-tabs-mode nil)
 '(x-select-enable-clipboard t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1))))
(scroll-bar-mode -1)

;; -- Trail whitespaces in all files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- Set font and the current line highlighting
(set-face-attribute 'default nil :family "Menlo" :weight 'normal :height 120)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFD")

;; -- Mac key bindings
(defun global-reset-key (key func)
  (global-unset-key key)
  (global-set-key key func))

(defun sfp-page-down (&optional arg)
  (interactive "^P")
;;  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun sfp-page-up (&optional arg)
  (interactive "^P")
;;  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines))
  )

(global-set-key (kbd "<next>") 'sfp-page-down)
(global-set-key (kbd "<prior>") 'sfp-page-up)
;;(global-set-key "\C-m" 'newline-and-indent)

;; Switch command and option for emacs key bindings
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)

;; OS X clipboard
(global-reset-key (kbd "M-c") 'kill-ring-save)
(global-reset-key (kbd "M-v") 'yank)

(global-set-key [end] 'move-end-of-line)
(global-set-key [home] 'move-beginning-of-line)

;; -- Other key bindings
;; auto-complete on Ctrl-Enter
(global-set-key (kbd "C-<return>") 'dabbrev-expand)

;; previous window
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; recording and replaying macros
;; TODO: figure out keys
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)
(global-set-key (kbd "C-/")        'kmacro-call-macro)

;; -- Smooth scrolling
(add-to-list 'load-path "~/.emacs.d/smooth-scrolling/")
(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

;; -- Multiple cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el/")
(require 'multiple-cursors)
(global-unset-key (kbd "C-M-c"))
(global-unset-key (kbd "C-M-."))
(global-unset-key (kbd "C-M-."))
(global-set-key (kbd "C-M-c C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-c C-M-.") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-c C-M-,") 'mc/mark-all-like-this)

;; -- Clojure mode
(global-prettify-symbols-mode +1)
(add-to-list 'load-path "~/.emacs.d/clojure-mode/")
(require 'clojure-mode)

;; syntax highlighting
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
          '(
            (("@\\|%[1-9]?" . 'clojure-special))
            (("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
;;            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))
            (("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t))
            )))

;; (defun tweak-clojure-syntax ()
;;   (mapcar (lambda (x) (font-lock-add-keywords nil x))
;;           '((("#?['`]*(\\|)"       . 'clojure-parens))
;;             (("#?\\^?{\\|}"        . 'clojure-brackets))
;;             (("\\[\\|\\]"          . 'clojure-braces))
;;             ((":\\w+"              . 'clojure-keyword))
;;             (("#?\""               0 'clojure-double-quote prepend))
;; ;;            (("\\btrue\\b\\|\\bfalse\\b\\|\\bnil\\b\\|@\\|%[1-9]?" . 'clojure-special))
;;             (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
;;             (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))
;;             (("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t))
;;             ("(\\(fn\\)[\[[:space:]]" 0 (replacement-region "λ"))
;;             ("\\(#\\)(" 0 (replacement-region "λ"))
;;             ("(\\(partial\\)[[:space:]]" 0 (replacement-region "Ƥ"))
;;             ("(\\(comp\\)[[:space:]]" 0 (replacement-region "ο"))


;;             )))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

;; -- Lisp mode
(font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\(lambda\\)\\>" (0 (prog1 ()
                               (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ?λ))))))
;; -- Paredit
(add-to-list 'load-path "~/.emacs.d/paredit/")
(require 'paredit)
(dolist (hook '(clojure-mode emacs-lisp-mode-hook lisp-mode-hook
                scheme-mode-hook lisp-interaction-mode-hook))
  (add-hook hook (lambda () (paredit-mode +1))))
(global-set-key [backspace] 'paredit-backward-delete)
(global-set-key [kp-delete] 'paredit-forward-delete)

(defun comment-sexp ()
  "Comment out the sexp at point."
  (interactive)
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (mark-sexp)
    (paredit-comment-dwim)))
(global-reset-key (kbd "M-;") 'comment-sexp)

;; -- Parenthesis highlighting
(add-to-list 'load-path "~/.emacs.d/highlight-parentheses.el/")
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("#ff0000" "#0000ff" "#00ff00" "#ff00ff" "#ffff00" "#00ffff"))

;; -- Tramp
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
