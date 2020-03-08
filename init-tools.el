;;==============================================================================
;; pdf-tools
;;==============================================================================

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;==============================================================================
;; graphviz
;;==============================================================================

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot)

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
  :custom-face
  (diff-hl-insert ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (diff-hl-delete ((t (:foreground "#ff5555" :background "#ff5555"))))
  (diff-hl-change ((t (:foreground "#8be9fd" :background "#8be9fd"))))
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

;;==============================================================================
;; ansi-term
;;
;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
;;==============================================================================

(cond
 ((string-equal system-type "darwin")
  (setq explicit-shell-file-name "/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq explicit-shell-file-name "/usr/bin/zsh")))

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;;==============================================================================
;; markdown
;;==============================================================================

(setq markdown-command "pandoc")

;; markdown-preview-mode
;; https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets (list "~/.emacs.d/css/github-markdown.css"))
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

;;==============================================================================
;; org-re-reveal
;;
;; https://gitlab.com/oer/org-re-reveal
;; https://github.com/hakimel/reveal.js
;;==============================================================================

(use-package org-re-reveal
  :ensure t
  :config
  (setq org-re-reveal-root (concat (getenv "HOME") "/.emacs.d/reveal.js")))

;;==============================================================================
;; Google Translate (version): 0.11.18
;;
;; https://github.com/atykhonov/google-translate
;;
;; NOTE: Since the version 0.11.8 has a bug about error of "(args-out-of-range
;; [] 1)", I use the source code which has been customized to fix the bug.
;; The original source code is from 'dc118de511c433750d4c98b9dd67350118c04fd6'
;; on Jun 20, 2019
;;
;; https://github.com/atykhonov/google-translate/issues/98
;; https://qiita.com/akicho8/items/cae976cb3286f51e4632
;;==============================================================================

(add-to-list 'load-path "~/.emacs.d/google-translate-custom-dc118de-20190620/")
(require 'google-translate)
(require 'google-translate-smooth-ui)
(require 'google-translate-default-ui)
(setq google-translate-translation-directions-alist
      '(("auto" . "ko") ("auto" . "en")))
(setq google-translate-output-destination nil)
(setq google-translate-pop-up-buffer-set-focus t)


(provide 'init-tools)
