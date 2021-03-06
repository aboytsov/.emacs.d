(setq mac-command-modifier 'alt)

(global-set-key (kbd "A-<right>") 'end-of-line)
(global-set-key (kbd "A-<left>") 'beginning-of-line)
(global-set-key (kbd "A-<down>") 'end-of-buffer)
(global-set-key (kbd "A-<up>") 'beginning-of-buffer)

(global-set-key (kbd "A-s") 'save-buffer)

(global-set-key (kbd "A-c") 'kill-ring-save)
(global-set-key (kbd "A-x") 'kill-region)
(global-set-key (kbd "A-a") 'mark-whole-buffer)
