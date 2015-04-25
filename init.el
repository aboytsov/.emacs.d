(load-file "~/.emacs.d/utils.el")

(if (not (boundp 'init-ok))
  (x-popup-dialog t '("Please start Emacs with --load init-kinesis.el or --load init-mac.el")))

;;(toggle-debug-on-error)

;; -- Set window width/height
(setq default-frame-alist (append (list
                                   '(width  . 80)
                                   '(height . 54))
  default-frame-alist))

;; -- Various settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-interval nil)
 '(clojure-mode-use-backtracking-indent t)
 '(column-number-mode t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 2)
 '(default-tab-width 2 t)
 '(frame-title-format (quote ("%f")) t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(tool-bar-mode nil)
 '(kill-whole-line t))

(scroll-bar-mode -1)
(delete-selection-mode t)

;; -- Clipboard and undo
(setq x-select-enable-clipboard t)
(global-reset-key (kbd "M-c") 'kill-ring-save)
(global-reset-key (kbd "M-v") 'yank)
(global-reset-key (kbd "M-z") 'undo)

;; -- Trail whitespaces in all files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- Set font and the current line highlighting
(set-face-attribute 'default nil :family "Menlo" :weight 'normal :height 120)
;;(set-face-attribute 'default nil :family "ProofInconsolata" :weight 'normal :height 120)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFD")

;; -- Mac key bindings
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

;; TODO
;; (global-set-key (kbd "<next>") 'sfp-page-down)
;; (global-set-key (kbd "<prior>") 'sfp-page-up)

(global-reset-key (kbd "C-q")
  (lambda () (interactive) (kill-this-buffer) (other-window -1)))

;; -- Other key bindings
;; auto-complete on Ctrl-Enter
(global-reset-key (kbd "C-<return>") 'dabbrev-expand)

;; previous window
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; recording and replaying macros
(global-set-key (kbd "C-,") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-/") 'kmacro-call-macro)

;; rgrep
(global-set-key (kbd "M-g s") 'rgrep)
(global-set-key (kbd "C->") 'next-error)      ;; Control + Shift + >
(global-set-key (kbd "C-<") 'previous-error)  ;; Control + Shift + <


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
(global-set-key (kbd "A-m r") 'mc/edit-lines)
(global-set-key (kbd "A-.") 'mc/mark-next-like-this)
(global-set-key (kbd "A-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "A-m a") 'mc/mark-all-like-this)

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
  (font-lock-add-keywords
   'clojure-mode
   ;; remove progn?
   '(("(\\(fn\\)[\[[:space:]]" (0 (progn (compose-region
                                          (match-beginning 1)
                                          (match-end 1) "λ")
                                         nil))
      )
     ("\\(#\\)(" (0 (progn (compose-region
                                          (match-beginning 1)
                                          (match-end 1) "λ")
                                         nil)))
     ("(\\(partial\\)[[:space:]]" (0 (progn (compose-region
                                          (match-beginning 1)
                                          (match-end 1) "Ƥ")
                                         nil)))
     ("(\\(comp\\)[[:space:]]" (0 (progn (compose-region
                                          (match-beginning 1)
                                          (match-end 1) "ο")
                                         nil)) ))

   )
  )

  ; (mapcar (lambda (x) (font-lock-add-keywords nil x))
;           '(
;             (("@\\|%[1-9]?" . 'clojure-special))
;             (("#?['`]*(\\|)"       . 'clojure-parens))
;             (("#?\\^?{\\|}"        . 'clojure-brackets))
;             (("\\[\\|\\]"          . 'clojure-braces))
; ;;            ((":\\w+"              . 'clojure-keyword))
;             (("#?\""               0 'clojure-double-quote prepend))
;             (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))
;             (("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t))
;             ))

; (font-lock-add-keywords
;     'clojure-mode '(("(\\(fn\\)[\[[:space:]]" (0 (replacement-region "λ")))
;                     ("\\(#\\)(" (0 (replacement-region "λ")))
;                     ("(\\(partial\\)[[:space:]]" (0 (replacement-region "Ƥ")))
;                     ("(\\(comp\\)[[:space:]]" (0 (replacement-region "ο")))))

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
;;(dolist (hook ('clojure-mode-hook
;;               emacs-lisp-mode-hook lisp-mode-hook
;;               scheme-mode-hook lisp-interaction-mode-hook
;;               ))
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;)

(defun comment-sexp (arg)
  "Comment out the sexp at point."
  (save-excursion
    (paredit-forward)
    (call-interactively 'set-mark-command)
    (paredit-backward)
    (paredit-comment-dwim arg)))

(eval-after-load "paredit-mode-hook"
  '(progn
     ;; https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens
     ;; http://www.meetup.com/stl-clojure/messages/70487902/
     ;; http://www.reddit.com/r/emacs/comments/1q99wi/moving_from_paredit_to_smartparens/
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "C-M-<left>") nil)
     (define-key paredit-mode-map (kbd "C-M-<right>") nil)
     (define-key paredit-mode-map (kbd "C-(") nil)
     (define-key paredit-mode-map (kbd "C-)") nil)
     (define-key paredit-mode-map (kbd "C-{") nil)
     (define-key paredit-mode-map (kbd "C-}") nil)
     (define-key paredit-mode-map (kbd "M-)" ) nil)
     (define-key paredit-mode-map (kbd "M-}" ) nil)
     (define-key paredit-mode-map (kbd "M-]" ) nil)
     (define-key paredit-mode-map (kbd "M-\"") nil)))

(add-hook 'paredit-mode-hook
          (lambda () (bind-keys*
                 ;; sensibile key bindings
                 ("M-<right>" . paredit-forward)
                 ("M-<left>"  . paredit-backward)
                 ;; commenting/uncommenting
                 ("M-;"       . (lambda () (interactive) (comment-sexp 1)))
                 ("C-M-;"     . (lambda () (interactive) (comment-sexp -1)))
                 ;; barf/slurp
                 ("A-<right>"   . paredit-forward-slurp-sexp)
                 ("A-S-<left>"  . paredit-forward-barf-sexp)
                 ("A-<left>"    . paredit-backward-slurp-sexp)
                 ("A-S-<right>" . paredit-backward-barf-sexp)
                 ;; closing
                 ("S-<return>"  . paredit-close-parenthesis-and-newline)
                 ("A-)"         . paredit-close-round)
                 ("A-}"         . paredit-close-curly)
                 ("A-]"         . paredit-close-bracket)
                 ("A-\""        . paredit-meta-doublequote)
                 ;; killing
                 ("C-<backspace>" . paredit-backward-kill-word)
                 ("C-<delete>"    . paredit-forward-kill-word)
                 ;; if a selection is active, use the default behavior
                 ;; (see delete-selection-mode above) otherwise call
                 ;; paredit
                 ("<backspace>"   . (lambda ()
                                      (interactive)
                                      (if (use-region-p)
                                        (delete-backward-char 1)
                                        (paredit-backward-delete))))
                 ("<deletechar>"  . (lambda ()
                                      (interactive)
                                      (if (use-region-p)
                                        (delete-forward-char 1)
                                        (paredit-forward-delete))))
                 ;; TODO:
                 ;; killing sexpr
                 ;; splicing and joining
                 ;; killing up or down
                 ;; go through the whole list
                 )))

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

;; --- CIDER
;; TODO: add-to-list & require
(add-to-list 'load-path "~/.emacs.d/dash.el/")
(require 'dash)
(add-to-list 'load-path "~/.emacs.d/queue/")
(require 'queue)
(add-to-list 'load-path "~/.emacs.d/CIDER")
(require 'cider)
(require 'cider-macroexpansion)

(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-prefer-local-resources t)
(setq cider-stacktrace-fill-column 80)
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-display-in-current-window t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-interactive-eval-result-prefix "")
(setq cider-repl-tab-command 'indent-for-tab-command)
(cider-repl-toggle-pretty-printing)

(add-to-list 'load-path "~/.emacs.d/use-package/")
(require 'bind-key)

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

;; TODO: was for syntax highlighting, but problem with namespaces now
;; should turn syntax highlighting on somehow else
;;(add-hook 'cider-repl-mode-hook 'clojure-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (bind-keys*
              ;; REPL
              ("S-<return>" . newline-and-indent)
              ("M-<up>"     . cider-repl-previous-input)
              ("M-<down>"   . cider-repl-next-input)
              ("M-S-<up>"   . cider-repl-previous-prompt)
              ("M-S-<down>" . cider-repl-next-prompt)))
              ;; sensibile key bindings

          )

(defun cider-local ()
  (interactive)
  (cider "127.0.0.1" 12121))

(global-reset-key (kbd "C-c C-l") 'cider-local)
(global-reset-key (kbd "M-g f") 'find-tag)
(global-reset-key (kbd "M-g r") 'cider-switch-to-repl-buffer)

;; -- Autocomplete (in mini-buffer)
(require 'icomplete)
(icomplete-mode 99)

(require 'ido)
(ido-mode t)

(add-to-list 'load-path "~/.emacs.d/flx")
(require 'flx-ido)

(ido-mode 1)
;; TOOD: possible to use on Meta-X?
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; -- Symbol completion
(add-to-list 'load-path "~/.emacs.d/company-mode")
(require 'company)
(global-company-mode)
(global-set-key (kbd "M-<return>") 'company-complete)

;; -- Git
(setq magit-last-seen-setup-instructions "1.4.0")

(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)

;; -- Finding any file in the current git repository
(add-to-list 'load-path "~/.emacs.d/find-file-in-repository")
(require 'find-file-in-repository)
(global-reset-key (kbd "C-x C-g") 'find-file-in-repository)
