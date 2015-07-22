;; TODO: ace-window mode
;; TOOD: all other packages installed
;; http://www.emacswiki.org/emacs/WinnerMode
;; http://www.emacswiki.org/emacs/WhiteSpace
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Highlight-Interactively.html https://www.masteringemacs.org/article/highlighting-by-word-line-regexp
;; TODO: full screen
;; smart-mode-line (was recently adapted by Prelude)

(require 'auto-package-update)
(auto-package-update-maybe)

(require 'use-package)   ;; TODO: tidy init.el, start using

;; TODO: cleanup (this is not auto-compile package, this is manual!)
(require 'dash)
(require 'f)

(defun was-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))

(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (--each (f-directories package-user-dir)
    (unless (was-compiled-p it)
      (byte-recompile-directory it 0))))

(ensure-packages-compiled)

;; ;; this will install undo-tree if it's not there
;; ;; and it will set it up how I want
;; (use-package undo-tree
;;   :init (global-undo-tree-mode 1)
;;   :bind (("C-c j" . undo-tree-undo)
;;          ("C-c k" . undo-tree-redo)
;;          ("C-c l" . undo-tree-switch-branch)
;;          ("C-c ;" . undo-tree-visualize))
;;   :ensure t)

;; -- Clear all hooks and font-locks for config reloading without
;; restarting Emacs
(dolist (hook '(clojure-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                before-save-hook
                smartparens-mode-hook
                cider-mode-hook
                cider-repl-mode-hook
                lisp-interaction-mode-hook))
  (setq hook nil))
(setq font-lock-keywords-alist nil)

(load-file "~/.emacs.d/utils.el")
(load-file (concat "~/.emacs.d/profile-" (getenv "EMACS_PROFILE") ".el"))

;; -- Backups & autosave
(defconst backup-directory "~/.backups-emacs/")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))

(setq backup-directory-alist `(("." . ,backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name backup-directory) t)))
(setq auto-save-list-file-prefix (concat backup-directory "autosave-filenames/"))

(setq make-backup-files    t
      vc-make-backup-files t    ;; even if under version control
      backup-by-copying    t    ;; don't clobber symlinks
      version-control      t    ;; add version numbers for backup files
      delete-old-versions  t    ;; delete excess backup files silently
      ;; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-old-versions    100


      auto-save-default    t    ;; auto-save every buffer that visits a file
      auto-save-timeout    20   ;; number of seconds idle time before auto-save
      auto-save-interval   200) ;; number of keystrokes between auto-saves

;; -- Windows
(let ((width 165)
      (frame (selected-frame)))
  (set-frame-width    frame width)
  (set-frame-height   frame 56)
  (set-frame-position frame 260 0)

  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below))

;; kill buffer in the current window
(global-reset-key (kbd "C-w")
  (lambda () (interactive) (kill-this-buffer)))
;; kill buffer in the other window (for error messages, greps etc)
(global-reset-key (kbd "C-q")
  (lambda () (interactive) (kill-buffer (window-buffer (previous-window)))))

;; so that quit (escape) doesn't unsplit all the windows
(defadvice one-window-p (around always (arg))
  (setq ad-return-value t))

(defun keyboard-escape-quit-leave-windows-alone ()
  (interactive)
  (ad-enable-advice 'one-window-p 'around 'always)
  (ad-activate 'one-window-p)
  (unwind-protect
      (keyboard-escape-quit)
    (ad-disable-advice 'one-window-p 'around 'always)
    (ad-deactivate 'one-window-p)))

;; switch esc and ctrl+g and use only one escape instead of 3
;;(global-set-key (kbd "<escape>") 'keyboard-quit)
;;(global-set-key (kbd "C-g") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>")
                (lambda () (interactive)
                  (keyboard-escape-quit-leave-windows-alone)
                  (keyboard-quit)))

;; -- Popwin

(add-to-list 'load-path "~/.emacs.d/modules/popwin-el/")

(require 'popwin)
(popwin-mode 1)

(setq popwin:special-display-config
      '(("*Miniedit Help*" :noselect t)
        help-mode
        (completion-list-mode :noselect t)
        (compilation-mode :noselect t)
        (grep-mode :noselect t)
        (occur-mode :noselect t)
       (" *undo-tree*" :width 60 :position right)
       (" *undo-tree*" :width 60 :position right)
;;        ("*undo-tree Diff*" :height 0.3 :position bottom)
        ;; TODO: increase height for descbinds
        ;; seems like descbinds breaks my bindings! (cmd+c)
        ("^\*helm.*\*$" :regexp t)
        ("*cider-error*" :height 0.5)
        ("*Messages*" :height 0.5)
        ))

(global-set-key (kbd "M-w") popwin:keymap)

;; -- Various settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-interval nil)
 '(column-number-mode t)
 '(comint-scroll-to-bottom-on-input (quote all))
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (32 40 46)))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(default-tab-width 2 t)
 '(eshell-scroll-to-bottom-on-input (quote this))
 '(frame-title-format (quote ("%f")) t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(org-M-RET-may-split-line t)
 '(org-table-auto-blank-field t)
 '(rainbow-ansi-colors (quote auto))
 '(rainbow-ansi-colors-major-mode-list
   (quote
    (sh-mode c-mode c++-mode clojure-mode emacs-lisp-mode html-mode)))
 '(recentf-max-menu-items 200)
 '(recentf-max-saved-items 200)
 '(safe-local-variable-values
   (quote
    ((outline-minor-mode)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark))))
 '(tool-bar-mode nil))

(scroll-bar-mode -1)
(delete-selection-mode t)

;; just 'y'/'n' instead of 'yes'/'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; revert files when they change
(global-auto-revert-mode t)

;; --- Org mode
;; Emacs comes with org-mode, but ours is hacked so need to load locally
;(add-to-list 'load-path "~/.emacs.d/org-mode/")
;(require 'org-table)
;;(require 'org-mode)
(load-file "~/.emacs.d/org-mode/lisp/org-table.el")
;; -- Orgtbl-mode for table editing (useful for Midje tests)
;;(add-hook 'clojure-mode-hook 'orgtbl-mode)

;; -- Clipboard and undo
(setq undo-limit 100000)

(global-unset-key (kbd "C-_"))
(setq undo-tree-map (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "C--") 'undo-tree-undo)
                      (define-key map (kbd "C-=") 'undo-tree-redo)
                      (define-key map (kbd "A--") 'undo-tree-visualize)
                      (define-key map (kbd "C-x i") 'undo-tree-visualize)
                      (define-key map "\C-x o" 'undo-tree-visualize)
                      map))

(add-to-list 'load-path "~/.emacs.d/modules/undo-tree/")
(require 'undo-tree)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `(("." . ,(expand-file-name backup-directory))))
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

(global-undo-tree-mode)

(add-to-list 'load-path "~/.emacs.d/modules/point-undo/")
(require 'point-undo)
(global-set-key (kbd "M--") 'point-undo)
(global-reset-key (kbd "M-=") 'point-redo)

;; -- Clipboard

(setq x-select-enable-clipboard t)
(global-reset-key (kbd "M-c") 'kill-ring-save)
(global-reset-key (kbd "A-c") 'kill-ring-save)
(global-reset-key (kbd "M-v") 'yank)
(global-reset-key (kbd "A-v") 'yank)

;; -- Trail whitespaces in all files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- Set font and the current line highlighting
(set-face-attribute 'default nil :family "MenloClojure"
                    :weight 'normal :height 120)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFD")

;; -- Mac-specific including key bindings
(menu-bar-mode)

;; TODO
(defun sfp-page-down (&optional arg)
  (interactive "^P")
;;  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

;; TODO
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

;; -- Dash is used to several packages below
(add-to-list 'load-path "~/.emacs.d/modules/dash.el/")
(require 'dash)

;; -- Helm
;;(add-to-list 'load-path "~/.emacs.d/helm/")
(add-to-list 'load-path "~/.emacs.prelude/elpa/helm-20150719.721/")
(add-to-list 'load-path "~/.emacs.prelude/elpa/helm-core-20150719.845/")
(require 'helm-config)
;;(require 'helm)
(helm-mode)

(set-face-attribute 'helm-source-header nil :family "Menlo"
                    :weight 'normal :height 100)

(helm-adaptive-mode t)   ;; NOT-CONFIRMED
(setq helm-always-two-windows               nil
  helm-split-window-default-side        'other
  helm-quick-update                     t
  helm-buffer-skip-remote-checking      t ;; NOT-CONFIRMED
  helm-candidate-number-limit         500
  helm-mode-fuzzy-match                 t
  helm-M-x-fuzzy-match                  t
  helm-apropos-fuzzy-match              t
  helm-buffers-fuzzy-matching           t
  helm-locate-fuzzy-match               t
  helm-recentf-fuzzy-match              t
  helm-tramp-verbose                    6 ;; NOT-CONFIRMED
  helm-buffers-favorite-modes           '(clojure-mode org-mode) ;; NOT-CONFIRMED
  helm-descbinds-window-style           'split-window
  helm-ff-file-name-history-use-recentf t
  helm-move-to-line-cycle-in-source nil
  helm-ff-history-max-length            500 ;; NOT-CONFIRMED
  helm-ff-skip-boring-files             t   ;; NOT-SURE
;;  helm-ff-transformer-show-only-basename nil ;; NOT-SURE NOT-CONFIRMED
  ;;       ;;helm-reuse-last-window-split-state    t ;; NOT-SURE
  )

;; (add-hook 'helm-minibuffer-set-up-hook
;;           (lambda ()
;;             (message "ME")
;;            (helm-attrset 'follow 1 helm-source-buffers-list)
;;             ))


(defun helm-debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

;; -- grep coloring
(setq helm-grep-default-command
      "ack-grep -Hn --smart-case --no-group %e %p %f")
(setq helm-grep-default-recurse-command
      "ack-grep -H --smart-case --no-group %e %p %f")
(setq helm-ls-git-grep-command
      "git grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p %f")
(setq helm-default-zgrep-command
      "zgrep --color=always -a -n%cH -e %p %f")

(global-reset-key (kbd "C-y") 'helm-show-kill-ring)
(global-reset-key (kbd "C-x C-f") 'helm-find-files)
(global-reset-key (kbd "C-x C-r") 'helm-recentf)
(global-reset-key (kbd "M-l") 'helm-locate)
(global-reset-key (kbd "M-X") 'helm-complex-command-history)
(global-reset-key (kbd "C-\\") 'helm-resume)
(global-reset-key (kbd "C-|") (lambda () (interactive) (helm-resume 1)))

;; helm-mode keys
(define-key helm-map (kbd "A-<up>") 'helm-beginning-of-buffer)
(define-key helm-map (kbd "A-<down>") 'helm-end-of-buffer)

(add-to-list 'load-path "~/.emacs.d/modules/helm-descbinds/")
(require 'helm-descbinds)
(helm-descbinds-install)            ; C-h b, C-x C-h

;; -- Buffers
(global-unset-key (kbd "C-x b"))
(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "C-M-[") 'switch-to-prev-buffer)
(global-set-key (kbd "C-M-]") 'switch-to-next-buffer)
(global-set-key (kbd "C-M-p") (lambda () (interactive) (switch-to-buffer nil)))

;; -- Projectile
(add-to-list 'load-path "~/.emacs.d/modules/projectile/")
(require 'projectile)
(require 'helm-projectile)

(setq projectile-enable-caching t)
(projectile-global-mode)
(helm-projectile-on)

(global-set-key (kbd "C-<tab>") 'helm-mini)
(global-set-key (kbd "C-b") 'helm-projectile-switch-to-buffer)
(global-reset-key (kbd "M-o") 'helm-projectile-grep)
(global-reset-key (kbd "C-p") (make-sparse-keymap))
(global-set-key (kbd "C-p p") 'helm-projectile)
(global-set-key (kbd "C-p s") 'helm-projectile-switch-project)
(global-set-key (kbd "C-p f") 'helm-projectile-find-file-in-known-projects)
(global-set-key (kbd "C-p o") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "C-p r") 'helm-projectile-recentf)
(global-set-key (kbd "C-p d") 'helm-projectile-find-dir)

;; -- Search
;;(add-to-list 'load-path "~/.emacs.d/color-occur/")
;;(require 'color-occur)

;; make clipboard work in isearch
(define-key isearch-mode-map (kbd "A-v") 'isearch-yank-kill)

(global-set-key (kbd "C-a") 'helm-occur)
(global-set-key (kbd "C-S-a") 'helm-multi-occur)

;; don't need isearch extended actions
(global-unset-key (kbd "M-s"))
;; but occur mode is handy
;;(global-reset-key (kbd "C-o") 'occur)

;; multi-occur will search all buffers in the current mode

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(global-reset-key (kbd "C-c C-o") 'multi-occur-in-this-mode)

;; -- Helm interface for the Silver Searcher
;; TODO: some of earlier search configuration is probably not needed now?
;;(add-to-list 'load-path "~/.emacs.d/modules/emacs-helm-ag/")
;;(require 'helm-ag)

;; -- Other key bindings
;; auto-complete on Ctrl-Enter
(global-reset-key (kbd "C-<return>") 'dabbrev-expand)

;; recording and replaying macros
(global-set-key (kbd "C-,") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-/") 'kmacro-call-macro)

;; rgrep
(global-set-key (kbd "M-g s") 'rgrep)
(global-set-key (kbd "C->") 'next-error)      ;; Control + Shift + >
(global-set-key (kbd "C-<") 'previous-error)  ;; Control + Shift + <

;; quick mode toggling and text properties
(defun list-text-properties-at (&optional pos)
  (interactive)
  (setq pos (or pos (point)))
  (let ((properties (text-properties-at pos))
      (buf (get-buffer-create "*Text properties*")))
    (set-buffer buf)
    (erase-buffer)
    (insert (format "%s" properties))
    (pop-to-buffer buf)))

(defun list-text-overlays-at (&optional pos)
  "Describe overlays at POS or point."
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
        (obuf (current-buffer))
        (buf (get-buffer-create "*Overlays*"))
        (props '(priority window category face mouse-face display
                 help-echo modification-hooks insert-in-front-hooks
                 insert-behind-hooks invisible intangible
                 isearch-open-invisible isearch-open-invisible-temporary
                 before-string after-string evaporate local-map keymap
                 field))
        start end text)
    (if (not overlays)
        (message "None.")
      (set-buffer buf)
      (erase-buffer)
      (dolist (o overlays)
        (setq start (overlay-start o)
              end (overlay-end o)
              text (with-current-buffer obuf
                     (buffer-substring start end)))
        (when (> (- end start) 13)
          (setq text (concat (substring text 1 10) "...")))
        (insert (format "From %d to %d: \"%s\":\n" start end text))
        (dolist (p props)
          (when (overlay-get o p)
            (insert (format " %15S: %S\n" p (overlay-get o p))))))
      (pop-to-buffer buf))))

(global-reset-key (kbd "M-m") (make-sparse-keymap))
(global-set-key (kbd "M-m c") 'clojure-mode)
(global-set-key (kbd "M-m o") 'org-mode)
(global-set-key (kbd "M-m t") 'text-mode)
(global-set-key (kbd "M-m l") 'emacs-lisp-mode)
(global-set-key (kbd "M-m p") 'list-text-properties-at)
(global-set-key (kbd "M-m o") 'list-text-overlays-at)

;; -- Smooth scrolling
(add-to-list 'load-path "~/.emacs.d/modules/smooth-scrolling/")
(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

;; -- Multiple cursors
;; TODO: DRY
(add-to-list 'load-path "~/.emacs.d/modules/multiple-cursors.el/")
(require 'multiple-cursors)
(global-unset-key (kbd "C-M-c"))
(global-unset-key (kbd "C-M-."))
(global-unset-key (kbd "C-M-."))
(global-set-key (kbd "A-m r") 'mc/edit-lines)
(global-set-key (kbd "A-.") 'mc/mark-next-like-this)
(global-set-key (kbd "A-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "A-m a") 'mc/mark-all-like-this)

;; -- Autofill (very useful for writing comments)
(setq default-fill-column 80)
(add-hook 'clojure-mode-hook 'auto-fill-mode)

;; -- Clojure mode
(add-to-list 'load-path "~/.emacs.d/clojure-mode/")
(require 'clojure-mode)

(setq open-paren-in-column-0-is-defun-start nil)
(setq clojure--prettify-symbols-alist nil)

(setq clojure-docstring-fill-column 80)
(setq clojure-docstring-fill-prefix-width 3)

;; indentation
(setq clojure-use-backtracking-indent nil)

(global-set-key (kbd "<backtab>") 'sp-indent-defun)

;; TODO: consolidate
(mapcar (lambda (x) (put-clojure-indent x 1))
        '(if-let if-not-let when-let when-not-let
          if-nil if-not-nil when-nil when-not-nil
          if-nil-let if-not-nil-let when-nil-let when-not-nil-let
          if-empty if-not-empty when-empty when-not-empty
          if-empty-let if-not-empty-let when-empty-let when-not-empty-let
          if-coll if-not-coll when-coll when-not-coll
          if-coll-let if-not-coll-let when-coll-let when-not-coll-let
          if-list if-not-list when-list when-not-list
          if-list-let if-not-list-let when-list-let when-not-list-let
          if-vector if-not-vector when-vector when-not-vector
          if-vector-let if-not-vector-let when-vector-let when-not-vector-let
          if-string if-not-string when-string when-not-string
          if-string-let if-not-string-let when-string-let when-not-string-let
          match match-fn))
(mapcar (lambda (x) (put-clojure-indent x 'defun))
        '(defn+ fn+ facts fact tabular decorate catch-all catch+ update-error))

;; syntax highlighting
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name
     '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-parens       "#999999"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "#4682b4"   "Clojure brackets")
(defclojureface clojure-keyword      "#2e8b57"   "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#4682b4"   "Clojure special")
(defclojureface clojure-double-quote "#4682b4"   "Clojure double quote")

(defun replacement (txt)
  `(0 (progn
        ;; (put-text-property (match-beginning 1)
        ;;                         (match-end 1)
        ;;                         'display
        ;;                         ,txt)
        ;;      (put-text-property (match-beginning 1)
        ;;                         (match-end 1)
        ;;                         'intangible
        ;;                         t)
        ;;      (add-face-text-property (match-beginning 1)
        ;;                              (+ 1 (match-beginning 1))
        ;;                              '(:foreground "red"))
        ;;      (add-face-text-property (match-beginning 1)
        ;;                              (+ 1 (match-beginning 1))
        ;;                              'bold)
        ;;      (add-face-text-property (match-beginning 1)
        ;;                              (match-end 1)
        ;;                              '(:raise 1))
        ;; TODO: not 1!
;;        (let ((overlay (make-overlay (match-beginning 1)
  ;;                                   (match-end 1))))
    ;;      (overlay-put overlay 'display ,txt)
;;          (overlay-put overlay 'face '(:foreground "red"))
        ;;    )
;;        (compose-region (match-beginning 1)
  ;;                      (match-end 1)
        ;;                    ,txt)
        (when (not (member (get-text-property (match-beginning 1) 'face)
                           '(font-lock-comment-face
                             font-lock-string-face
                             font-lock-doc-face)
                       ))
            (put-text-property (match-beginning 1)
                               (match-end 1)
                               'display
                               ,txt
                               ))
;;        (put-text-property (match-beginning 1)
;;                           (match-end 1)
;;                           'intangible
;;                           12
;;                           )
        ;; TODO: help-echo
        nil)))

;; http://www.emacswiki.org/emacs/InPlaceAnnotations
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Fields.html#Fields
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html#Special-Properties
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Replacing-Specs.html#Replacing-Specs
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Property.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Properties.html
;; http://www.emacswiki.org/emacs/TextProperties#text_property
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Properties.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html#Multiline-Font-Lock

;; (defun let-font-lock-extend-region1 ()
;;   "Extend the search region to include an entire block of text."
;;   ;; Avoid compiler warnings about these global variables from font-lock.el.
;;   ;; See the documentation for variable `font-lock-extend-region-functions'.
;;   (message "%s" "yay")
;;   (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
;;   (message "point: %d" (point))
;;   (let ((sexp-beg (nth 1 (syntax-ppss))))
;;     (message "sexp-beg: %d" sexp-beg)
;;     (if (= ?[ (char-after sexp-beg))
;;            ;; NEED TO SET END?
;;            (progn
;;              (setq font-lock-beg sexp-beg)
;;              (setq font-lock-end (+ 20 sexp-beg)))
;;            )
;;         )
;;     )
;;   )

(defun let-font-lock-extend-region ()
;;  (message "extend point %d" (point))
  (if (<= (point) 300)
      (progn (setq font-lock-beg (point))
             (setq font-lock-end (+ (point) 10)))
    nil)
  ;; return t!
  )

;; (defun let-font-lock-match-blocks1 (last)
;;   (message "%s" "blocks now")
;;   (set-match-data (list (+ point 2)
;;                         (+ point 4)
;;                         )
;;                   )
;;   )

;; TODO: extra font-locking in clojure mode


(defun down-or-out-sexp ()
  "Moves the point deeper into the s-expression tree, if there are no
  levels deeper moves it forward and out of the current s-expression."
  (let ((pos (point)))
    (sp-down-sexp)
    (if (<= (point) pos)
        (sp-up-sexp))))

;; if-not-string-let etc.
;; TODO: for doseq etc
(setq let-symbols '("let" "if-let"))

(defun clojure-match-pairs-step (last)
  ;; TODO: doc
  (setq m t)
  (if m (message "-- point %d" (point)))
  (let ((pos (point))
        (syntax (syntax-ppss))
        (result nil))
    (if m (message "pos: %d, syntax: %s" pos syntax))
    ;; skip comments
    (if (nth 4 syntax)
        (sp-up-sexp)
      ;; let is at least 2 levels deep
      (if (< (nth 0 syntax) 2)
          (progn
            (if m (message "not enough depth"))
            (down-or-out-sexp))
        ;; ok, we're inside an s-expression at least 2 levels deep
        ;; let's look at the 2nd inner level and make sure that
        ;; the symbol is one that we support (let, if-let etc.)
        (let ((sexps-stack (reverse (nth 9 syntax))))
          (let ((sexp-beg (second sexps-stack)))
            (if (not (member (save-excursion
                               (goto-char sexp-beg)
                               (forward-char)
                               (forward-sexp)
                               (thing-at-point 'symbol))
                             let-symbols))
                ;; nope, it wasn't, now we can either look deeper or go
                ;; to the next one
                (progn
                  (if m (message "not a member of let family"))
                  (down-or-out-sexp))
              ;; ok, we're in let - let's see how many arguments we'd need
              ;; to skip in the argument list to get to where we were
              ;; and determine if it's an odd or an even arg
              (if m (message "in let, point: %d" (point)))
              (goto-char (first sexps-stack))
              (forward-char)
              (let ((end-of-let (save-excursion
                                  (sp-end-of-sexp)
                                  (point))))
                (if m (message "let form: (%d %d)" (1- (point)) end-of-let))
                (let ((last-sexp (sp-forward-sexp)))
                  (let ((even t))
                    (if m (message "after the first one, point: %d"
                                   (point)))
                    (while (< (point) pos)
                      (if m (message "looping... %d" (point)))
                      (setq even (not even))
                      (setq last-sexp (sp-forward-sexp)))
                    (when m
                      (message "last-sexp: %s" last-sexp)
                      (message "even: %s" even))
                    ;; if it's an odd one, try to mark the one after it
                    (when (not even)
                      (setq last-sexp (sp-forward-sexp))
                      (if m (message "last-sexp reset to: %s" last-sexp)))
                    ;; if we ran out of let by this point, there's
                    ;; nothing to report
                    (when (>= (point) end-of-let)
                      ;; it doesn't exist
                      (if m (message "reached the end of let"))
                      (setq last-sexp nil)))
                  (if last-sexp
                      (setq result (list (sp-get last-sexp :beg)
                                         (sp-get last-sexp :end))))
                  (forward-sexp))))))))
    (if m (message "-> %s \"%s\""
                   result (if result
                              (apply 'buffer-substring result)
                            "")))
    ;; if the point didn't advance we're done searching, so move it manually
    (if (= (point) pos)
        (goto-char (1+ last)))
    (if result
        (progn (set-match-data result) t)
      nil)))

;; TODO: rename
(defun perf (last)
;;  (message "-- let-font-lock-match-blocks %d last %d" (point) last)
  (let ((result nil))
    (while (and (not result) (<= (point) last))
      (setq result (clojure-match-pairs-step last)))
    result))

(defun pperf (last)
  (while (< (point) last)
;;    (message "pp point = %d" (point))
    (perf (buffer-size))))

(defun let-font-lock-match-blocks (last)
;;  (message "-- let-font-lock-match-blocks %d last %d" (point) last)
  nil)

(setq clojure-font-locks
      ;;       '(("data" 0 font-lock-keyword-face))
      ;;       '(let-font-lock-match-blocks . font-lock-keyword-face)
      (nconc
       ;;       '((let-font-lock-match-blocks 0 font-lock-keyword-face))
       ;; what is <?
       ;; TODO: different font face for doc?
       `(("\\<\\(DOC\\|FIX\\|FIXME\\|TODO\\|BUG\\|CODEQUALITY\\):"
          1 font-lock-warning-face t))
       (mapcar (lambda (pair) `(,(first pair) . ,(replacement (second pair))))
               ;; TODO: instead of '(' need to detect if those symbols are
               ;; keywords
               '(("(\\(fn\\)[\[[:space:]]"          "ƒ")
                 ;; TODO: bigger plus
                 ("(\\(fn\\+\\)[\[[:space:]]"       "ƒ⁺")
                 ("\\b\\(defn\\)\\b"
                  (concat
                   (propertize "∎" 'face 'bold)
                   (propertize "ƒ" 'help-echo "help-text")
                   )
                  )
                 ("\\b\\(defn\\+\\)\\b"
                  (concat
                   (propertize "∎" 'face 'bold)
                   (propertize "ƒ⁺" 'help-echo "help-text")
                   )
                  )
                 ("\\b\\(defmacro\\)\\b"
                  (concat
                   (propertize "∎" 'face 'bold)
                   (propertize "Ƒ" 'help-echo "help-text")
                   )
                  )
                 ("\\b\\(def\\)\\b"
                  (concat
                   (propertize "∎" 'face 'bold)
                   )
                  )
                 ("\\b\\(complement\\|!\\)\\b"
                  (concat
                   (propertize "∁")
                   )
                  )
                 ;; ("\\(comp\\|[|]\\)\\(\\b\\| \\)"
                 ;;  (concat
                 ;;   (propertize "∘")
                 ;;   )
                 ;;  )
                 ("\\b\\(not\\)\\b"
                  "¬"
                  )
                 ("\\b\\(min\\)\\b"
                  "꜖"
                  )
                 ("\\b\\(max\\)\\b"
                  "꜒"
                  )
                 ;; TODO: vector versions
                 ;; TODO: ++ and -- as one symbol
                 ("\\b\\(nil\\)\\b"
                  "∅"
                  )
                 ("\\b\\(if-nil\\)\\b"
                  "if-∅"
                  )
                 ("\\b\\(nil\\?\\)\\b"
                  "∄"
                  )
                 ("\\b\\(some\\?\\)\\b"
                  "∃"
                  )
                 ("\\b\\(con>\\)\\b"
                  "☰"
                  )
                 ("\\b\\(con<\\)\\b"
                  "☱"
                  )
                 ("\\b\\(concat\\)\\b"
                  "☲"
                  )
                 ;; TODO: parens with indexes for highlights?
                 ;; TODO: reverse and reverse-args
                 ;; TODO: loop/recur
                 ("\\b\\(\\*>\\)\\b"
                  "λ…"
                  )
                 ("\\b\\(partial\\)\\b"
                  "λ…"
                  )
                 ("\\b\\(\\*<\\)\\b"
                  "…λ"
                  )
                 ("\\b\\(let\\)\\b"
                  "∎"
                  )
                 ("\\b\\(ns\\)\\b"
                  "§"
                  )
                 ("\\b\\(map\\)\\b"
                  "↦"
                  )
                 ("\\b\\(last\\)\\b"
                  "↩"
                  )
                 ("\\b\\(first\\)\\b"
                  "↪"
                  )
                 ("\\b\\(for\\)\\b"
                  "∀"
                  )
                 ("\\b\\(not=\\)\\b"
                  "≠"
                  )
                 ("\\b\\(<=\\)\\b"
                  "≤"
                  )
                 ("\\b\\(>=\\)\\b"
                  "≥"
                  )
                 ;;                ("\\(def-decorator\\)[\[[:space:]]"
                 ;;                 (concat
                 ;; ;;                 (propertize "⊐" 'face 'bold 'intangible 'def-decorator)
                 ;;                  (propertize "q" 'face '(:foreground "green")
                 ;;                              'help-echo "help-text"
                 ;;                              'intangible 'def-decorator)
                 ;;                  ))
                 ;; ("(\\(defn\\+\\)[\[[:space:]]"     "⌝ƒ⁺")
               ;;;; ("(\\(defmacro\\)[\[[:space:]]"    "⌉Ƒ")
                 ;;("(\\(defn\\+\\)[\[[:space:]]"     "⊐ƒ⁺")
                 ;;("(\\(defmacro\\)[\[[:space:]]"    "⊐Ƒ")
                 ;;("(\\(defmacro\\+\\)[\[[:space:]]" "⊐Ƒ⁺")
                 ;;("(\\(def\\)[\[[:space:]]"         "⊐")
                 ;;("(\\(def\\+\\)[\[[:space:]]"      "⊐⁺")
                 ;;("\\(#\\)("                        "λ")
                 ;;("(\\(ns\\)("                      "§")
                 ;;("(\\(comp\\)("                    "∘")
                 ;;("(\\(\\|\\)("                     "∘")
                 ))))
;; TODO: reloding without restarting
;; copy/paste/text-modes
;; overlays vs text properties


;; TODO: TODO, FIXME and BUG
;; also:
;; (defvar font-lock-format-specifier-face
;;   'font-lock-format-specifier-face
;;   "Face name to use for format specifiers.")

;; (defface font-lock-format-specifier-face
;;   '((t (:foreground "OrangeRed1")))
;;   "Font Lock mode face used to highlight format specifiers."
;;   :group 'font-lock-faces)

;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (font-lock-add-keywords nil
;; 				    '(("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
;; 				       1 font-lock-format-specifier-face t)
;; 				      ("\\(%%\\)"
;; 				       1 font-lock-format-specifier-face t)) )))

;;(font-lock-add-keywords 'clojure-mode clojure-font-locks)

;; TODO: needs to be highlighted: throw+, try+

;; -- Identifier highlighting
(add-to-list 'load-path "~/.emacs.d/color-identifiers-mode/")
(require 'color-identifiers-mode)
(setq color-identifiers:timer
      (run-with-idle-timer 0.1 t 'color-identifiers:refresh))
(add-hook 'clojure-mode-hook 'color-identifiers-mode)

(defun reload-syntax-highlighting ()
  (interactive)
  (eval-buffer)
  (other-window -1)
  (set-text-properties 1 (buffer-size) nil)
  (remove-overlays)
  (clojure-mode)
  (other-window -1))

(global-set-key (kbd "C-c s") 'reload-syntax-highlighting)

(setq font-lock-multiline t)

(add-hook 'clojure-mode-hook
          (lambda ()
            ;; cleanup
            (font-lock-remove-keywords 'clojure-mode clojure-font-locks)
;;            (remove-hook 'font-lock-extend-region-functions
  ;;                       'let-font-lock-extend-region)
            ;; setup
            (setq font-lock-extra-managed-props '(composition display))
            (set (make-local-variable 'font-lock-multiline) t)
;;            (add-hook 'font-lock-extend-region-functions
  ;;                    'let-font-lock-extend-region)
            (font-lock-add-keywords 'clojure-mode clojure-font-locks)
            (orgtbl-mode)
            ))

;; -- Window navigation
(global-set-key (kbd "A-M-<left>")  'windmove-left)
(global-set-key (kbd "A-M-<right>") 'windmove-right)
(global-set-key (kbd "A-M-<down>")  'windmove-down)
(global-set-key (kbd "A-M-<up>")    'windmove-up)
(global-unset-key (kbd "C-x o"))

(add-to-list 'load-path "~/.emacs.d/modules/buffer-move/")
(require 'buffer-move)
(global-set-key (kbd "A-M-S-<left>")  'buf-move-left)
(global-set-key (kbd "A-M-S-<right>") 'buf-move-right)
(global-set-key (kbd "A-M-S-<down>")  'buf-move-down)
(global-set-key (kbd "A-M-S-<up>")    'buf-move-up)

;; -- Smartparens

;; Key bindings

;; * ;shift              Marking in the buffer
;; * cmd                Beginning/end of line (left/right) or document (up/down)
;; * cmd+shift          Mark until beginning/end of line or document
;; * ctrl+alt           Switch desktop space (OS X, TotalSpaces2)
;; * ctrl+alt+shift     Move window to another space (OS X, TotalSpaces2)
;; * alt                Left/right: next/prev word; down/up - split/join sexpr
;; * alt+shift          Left-right: mark to next/prev word; down/up - split
;;                      sexpr killing backward/forward
;; * ctrl               Backward/forward/down/up sexpr
;; * ctrl+shift         Mark until backward/forward/down/up sexpr
;; * cmd+ctrl           Org-table mode: move row or column left/right/down/up
;; * cmd+ctrl+shift     Org-table mode: insert (down) or delete (up) row/column
;; * cmd+alt            Select window to the left/right/down/up
;; * cmd+alt+shift      Move (swap with) selected window left/right/down/up
;;   cmd+alt+ctrl       <empty>
;;   cmd+alt+ctrl+shift <empty>
;;
;; * ctrl+backspace     Backward-kill sexp
;; * ctrl+delete        Forward-kill sexp

;; * alt+], alt+shift+]  slurp/barf forwards
;; * alt+[, alt+shift+[  slurp/barf backwards
;;
;; * alt+ctrl+], alt+ctrl+[ Switch to next/previous buffer

(setq sp-base-key-bindings 'sp)
(setq sp-cancel-autoskip-on-backward-movement nil)
(setq sp-hybrid-kill-entire-symbol t)
(setq sp-hybrid-kill-excessive-whitespace t)
(setq sp-navigate-close-if-unbalanced t)
(setq sp-successive-kill-preserve-whitespace 2)
(setq sp-override-key-bindings
      '(("<return>" . indent-new-comment-line)
        ;; as per the map above
        ("M-<down>" . sp-split-sexp)
        ("M-<up>" . sp-join-sexp)
        ("C-<left>" . sp-backward-sexp)
        ("C-<right>" . sp-forward-sexp)
        ("C-<down>" . sp-down-sexp)
        ("C-<up>" . sp-up-sexp)
        ;; -- killing
        ("C-k" . sp-kill-hybrid-sexp)  ;; this kills a line respecting forms
        ("C-<delete>"   . sp-kill-sexp)
        ("C-<backspace>"   . sp-backward-kill-sexp)
        ("M-S-<up>"   . sp-splice-sexp-killing-backward)
        ("M-S-<down>"   . sp-splice-sexp-killing-forward)
        ;; -- barfing/slurping
        ("M-]" . sp-forward-slurp-sexp)
        ("M-}" . sp-forward-barf-sexp)
        ("M-[" . sp-backward-slurp-sexp)
        ("M-{" . sp-backward-barf-sexp)
        ;; -- we use this for buffer switching
        ("C-M-]" . nil)
        ("C-M-p" . nil)
        ("C-S-a" . nil)))

;; TODO: DRY
(add-to-list 'load-path "~/.emacs.d/modules/smartparens")
(require 'smartparens)
(require 'smartparens-config)

(add-hook 'smartparens-mode-hook 'smartparens-strict-mode)

(dolist (hook '(clojure-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook 'smartparens-mode))

;; (defun comment-sexp (arg)
;;   "Comment out the sexp at point."
;;   (save-excursion
;;     (paredit-forward)
;;     (call-interactively 'set-mark-command)
;;     (paredit-backward)
;;     (paredit-comment-dwim arg)))

;; (eval-after-load "paredit-mode-hook"
;;   '(progn
;;      ;; https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens
;;      ;; http://www.meetup.com/stl-clojure/messages/70487902/
;;      ;; http://www.reddit.com/r/emacs/comments/1q99wi/moving_from_paredit_to_smartparens/
;;      (define-key paredit-mode-map (kbd "C-<right>") nil)
;;      (define-key paredit-mode-map (kbd "C-<left>") nil)
;;      (define-key paredit-mode-map (kbd "C-M-<left>") nil)
;;      (define-key paredit-mode-map (kbd "C-M-<right>") nil)
;;      (define-key paredit-mode-map (kbd "C-(") nil)
;;      (define-key paredit-mode-map (kbd "C-)") nil)
;;      (define-key paredit-mode-map (kbd "C-{") nil)
;;      (define-key paredit-mode-map (kbd "C-}") nil)
;;      (define-key paredit-mode-map (kbd "M-)" ) nil)
;;      (define-key paredit-mode-map (kbd "M-}" ) nil)
;;      (define-key paredit-mode-map (kbd "M-]" ) nil)
;;      (define-key paredit-mode-map (kbd "M-\"") nil)))

;; (add-hook 'paredit-mode-hook
;;           (lambda () (local-set-key (kbd "<backspace>")
;;                                (lambda ()
;;                                  (interactive)
;;                                  (if (use-region-p)
;;                                      (delete-backward-char 1)
;;                                    (paredit-backward-delete))))
;;             (local-set-key (kbd "<deletechar>")
;;                            (lambda ()
;;                              (interactive)
;;                              (if (use-region-p)
;;                                  (delete-forward-char 1)
;;                                (paredit-forward-delete))))
;;                  (bind-keys*
;;                  ;; sensibile key bindings
;;                  ("M-<right>" . paredit-forward)
;;                  ("M-<left>"  . paredit-backward)
;;                  ;; commenting/uncommenting
;;                  ("M-;"       . (lambda () (interactive) (comment-sexp 1)))
;;                  ("C-M-;"     . (lambda () (interactive) (comment-sexp -1)))
;;                  ;; barf/slurp
;;                  ("A-<right>"   . paredit-forward-slurp-sexp)
;;                  ("A-S-<left>"  . paredit-forward-barf-sexp)
;;                  ("A-<left>"    . paredit-backward-slurp-sexp)
;;                  ("A-S-<right>" . paredit-backward-barf-sexp)
;;                  ;; closing
;;                  ("S-<return>"  . paredit-close-parenthesis-and-newline)
;;                  ("A-)"         . paredit-close-round)
;;                  ("A-}"         . paredit-close-curly)
;;                  ("A-]"         . paredit-close-bracket)
;;                  ("A-\""        . paredit-meta-doublequote)
;;                  ;; killing
;;                  ("C-<backspace>" . paredit-backward-kill-word)
;;                  ("C-<delete>"    . paredit-forward-kill-word)
;;                  ;; if a selection is active, use the default behavior
;;                  ;; (see delete-selection-mode above) otherwise call
;;                  ;; paredit

;;                  ;; TODO:
;;                  ;; killing sexpr
;;                  ;; splicing and joining
;;                  ;; killing up or down
;;                  ;; go through the whole list
;;                  )))

;; -- Parenthesis highlighting
;; TODO: better colors, maybe slightly darker background or symbols
;; like )1 )2 )3 etc - this is probably even better
;; symbols?
;; TODO: red/different background for unbalanced ones?
(add-to-list 'load-path "~/.emacs.d/modules/highlight-parentheses.el/")
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("#ff0000" "#0000ff" "#00ff00" "#ff00ff" "#ffff00" "#00ffff"))

;; -- Tramp
;; TODO: is scp faster? which one is the fastest one?
(setq tramp-default-method "ssh")
(setq tramp-verbose 10)
(require 'tramp)
(setcdr (assoc 'tramp-remote-shell (assoc "ssh" tramp-methods))
        '("/bin/bash"))
(setcdr (assoc 'tramp-remote-shell (assoc "scp" tramp-methods))
        '("/bin/bash"))
(setcdr (assoc 'tramp-remote-shell (assoc "rsync" tramp-methods))
        '("/bin/bash"))

;; disable version control (???)
;; (setq vc-ignore-dir-regexp
;;                 (format "\\(%s\\)\\|\\(%s\\)"
;;                         vc-ignore-dir-regexp
;;                         tramp-file-name-regexp))

;; --- Emacs Lisp
(global-prettify-symbols-mode +1)

;; printing and evaluation
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(defun eval-region-print ()
  (interactive)
  (if (use-region-p)
    (eval-region (region-beginning) (region-end) t)
    (message "No region")))

;; key bindings are similar to CIDER
(global-reset-key (kbd "C-c C-e") 'eval-last-sexp)
(global-reset-key (kbd "C-c C-k") 'eval-buffer)
(global-reset-key (kbd "C-c C-r") 'eval-region-print)

;; --- CIDER

;; TODO: add-to-list & require
(add-to-list 'load-path "~/.emacs.d/queue/")
(require 'queue)
(add-to-list 'load-path "~/.emacs.prelude/elpa/cider-20150719.2231/")
(require 'cider)
(require 'cider-macroexpansion)

(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-prefer-local-resources t)
(setq cider-stacktrace-fill-column 80)
(setq nrepl-buffer-name-show-port t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-interactive-eval-result-prefix "")
(setq cider-auto-select-error-buffer nil)

(setq cider-repl-display-in-current-window t)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-tab-command 'indent-for-tab-command)
(cider-repl-toggle-pretty-printing)

(setq cider-test-infer-test-ns
      (lambda (ns) (replace-regexp-in-string "stuph\\." "stuph.test." ns)))
(add-hook 'clojure-mode-hook 'midje-test-mode)

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

;; TODO: was for syntax highlighting, but problem with namespaces now
;; should turn syntax highlighting on somehow else
;;(add-hook 'cider-repl-mode-hook 'clojure-mode)

;;(add-hook 'cider-repl-mode-hook 'paredit-mode)

(defun cider-local ()
  (interactive)
  (cider-connect "127.0.0.1" 12121))

(global-reset-key (kbd "C-c r") 'cider-local)
(global-reset-key (kbd "M-g f") 'find-tag)
(global-reset-key (kbd "M-g r") 'cider-switch-to-repl-buffer)

;;(define-key cider-repl-mode-map (kbd "<return>")   'cider-repl-closing-return)
;;(define-key cider-repl-mode-map (kbd "S-<return>") 'newline-and-indent)
(define-key cider-repl-mode-map (kbd "M-<up>")     'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "M-<down>")   'cider-repl-next-input)
(define-key cider-repl-mode-map (kbd "M-S-<up>")   'cider-repl-previous-prompt)
(define-key cider-repl-mode-map (kbd "M-S-<down>") 'cider-repl-next-prompt)

;; (define-derived-mode cider-repl-mode-fixed cider-repl-mode "REPL"
;;   (derived-mode-set-keymap cider-repl-mode-map))

;; believe it or not, it seems like they just forgot to do it?
;; what am I not understanding?
;; (add-hook 'cider-repl-mode-hook (lambda ()
;; ;;                                  (smartparens-mode)
;;                                   (use-local-map 'cider-repl-mode-map)
;;                                   ))

;; (add-hook 'cider-repl-mode-hook
;;           (lambda ()
;;             (setq sp-backward-bound-fn (lambda ()
;;                                          cider-repl-input-start-mark))))

;; -- Midje
(provide 'clojure-test-mode)      ;; fool midje-test-mode.el
(add-to-list 'load-path "~/.emacs.d/modules/midje-test-mode/")
(require 'midje-test-mode)

;; -- Autocomplete (in mini-buffer)
(require 'icomplete)
(icomplete-mode 99)

(require 'ido)
(ido-mode t)
(setq ido-default-buffer-method 'selected-window)

(add-to-list 'load-path "~/.emacs.d/modules/flx")
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
;; TODO: finish
;;(setq magit-last-seen-setup-instructions "1.4.0")

(add-to-list 'load-path "~/.emacs.d/modules/magit/lisp")
(require 'magit)

(global-set-key (kbd "C-p m") 'magit-status)

;; -- Finding any file in the current git repository
(add-to-list 'load-path "~/.emacs.d/modules/find-file-in-repository")
(require 'find-file-in-repository)
(global-reset-key (kbd "C-x C-g") 'find-file-in-repository)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -- Sessions
;; TODO: DRY
(unless (boundp 'loaded-once)
  (add-to-list 'load-path "~/.emacs.d/modules/f.el/")
  (add-to-list 'load-path "~/.emacs.d/modules/s.el/")
  (add-to-list 'load-path "~/.emacs.d/modules/anaphora/")
  (add-to-list 'load-path "~/.emacs.d/modules/workgroups2/src/")
  (require 'workgroups2)

  (workgroups-mode 1))        ; put this one at the bottom of .emacs

(setq loaded-once t)
