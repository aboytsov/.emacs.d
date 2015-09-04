;; TODO: there should be absolutely no need for these functions

(defun global-reset-key (key func)
  (global-unset-key key)
  (global-set-key key func))

(defun local-reset-key (key func)
  (local-unset-key key)
  (local-set-key key func))
