;;==============================================================================
;; magit
;;==============================================================================

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(setq magit-gitflow-popup-key "~")

(global-set-key (kbd "C-c m") 'magit-status)

;;==============================================================================
;; pdf-tools
;;==============================================================================

(pdf-tools-install)

;;==============================================================================
;; ediff
;;
;; https://oremacs.com/2015/01/17/setting-up-ediff/
;;==============================================================================

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;==============================================================================
;; diff-hl
;;==============================================================================

(use-package diff-hl
    :ensure t
    :config
    (global-diff-hl-mode))


(provide 'init-tools)
