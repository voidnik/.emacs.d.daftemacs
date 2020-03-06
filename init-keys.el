;;==============================================================================
;; Global Keys
;;==============================================================================

(global-set-key (kbd "S-SPC") 'toggle-input-method)

(global-set-key (kbd "M-m") 'imenu-list)
(global-set-key (kbd "M-o") 'projectile-find-other-file)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c r") 'counsel-register)
(global-set-key (kbd "C-c t") 'counsel-recentf)
(global-set-key (kbd "C-c f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-c d") 'counsel-projectile-find-dir)
(global-set-key (kbd "C-c b") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "C-c g") 'counsel-projectile-rg)
(global-set-key (kbd "C-c p") 'counsel-projectile-switch-project)

(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c w") 'xwidget-webkit-browse-url)

(global-set-key (kbd "C-c .") 'whitespace-mode)

(global-set-key (kbd "C-c 1") 'eshell)
(global-set-key (kbd "C-c 2") '(lambda () (interactive) (ansi-term explicit-shell-file-name)))
(global-set-key (kbd "C-c 0") 'neato-graph-bar)

(global-set-key (kbd "C-c z") 'resize-window)

(global-set-key "\C-ct" 'google-translate-smooth-translate)
(global-set-key "\C-cT" 'google-translate-query-translate)

(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; GUD
;(global-set-key [f5] '(lambda ()
;                        (interactive)
;                        (call-interactively 'gud-tbreak)
;                        (call-interactively 'gud-cont)))
;(global-set-key [f6] 'gud-next)
;(global-set-key [f7] 'gud-step)
;(global-set-key [f8] 'gud-finish)
;(global-set-key [f9] 'gud-break)

;(global-set-key (kbd "C-c f") 'find-file-in-tags) ;; OBSOLETE
;(global-set-key (kbd "M-o") 'ff-find-other-file) ;; OBSOLETE


(provide 'init-keys)
