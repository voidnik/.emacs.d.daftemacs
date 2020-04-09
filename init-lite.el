(setq user-init-file "~/.emacs.d/init-lite.el")

(load-file "~/.emacs.d/init-base.el")
(require 'init-base)

(global-set-key (kbd "C-x C-b") 'ibuffer)
