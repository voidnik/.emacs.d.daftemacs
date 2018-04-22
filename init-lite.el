(setq visible-bell t
      make-backup-files nil
      column-number-mode t
      split-height-threshold nil ; not to split this way.
      gdb-many-windows t)
(setq-default truncate-lines t)
(put 'erase-buffer 'disabled nil)
(show-paren-mode t)
(delete-selection-mode t)
(require 'cl) ; Common Lisp

(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))

;;==============================================================================
;; Key Mapping Customiaztion
;;==============================================================================

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x p") 'other-window-reverse)
