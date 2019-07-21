;;==============================================================================
;; Theme
;;==============================================================================

(defun init-doom-theme ()
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  ;(load-theme 'doom-one t)
  ;(load-theme 'doom-dracula t)
  ;(load-theme 'doom-city-lights t)
  ;(load-theme 'doom-molokai t)
  ;(load-theme 'doom-nord t)
  ;(load-theme 'doom-nova t)
  ;(load-theme 'doom-peacock t)
  ;(load-theme 'doom-solarized-light t)
  ;(load-theme 'doom-spacegrey t)
  ;(load-theme 'doom-tomorrow-night t)
  ;(load-theme 'doom-tomorrow-day t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun init-zerodark-theme ()
  (load-theme 'zerodark t)
  ;; Optionally setup the modeline
  (zerodark-setup-modeline-format))

(defun init-color-theme ()
  (add-to-list 'load-path "~/.emacs.d/emacs-goodies-el/")
  (require 'color-theme)
  (load-file "~/.emacs.d/color-theme/color-theme-sunburst.el")
  ;(load-file "~/.emacs.d/color-theme/color-theme-tangotango.el")
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-sunburst))))

(defun init-themes ()
  ;(load-theme 'base16-default-dark t)
  ;(init-doom-theme)
  ;(init-zerodark-theme)
  ;(init-color-theme)
  (load-theme 'dracula t) ; https://draculatheme.com/emacs/
  )

;;==============================================================================
;; Mode Line
;;==============================================================================

(defun init-doom-mode-line ()
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode))

  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 3)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are expereicing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)

  ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon nil)

  ;; Whether display minor modes or not. Non-nil to display in mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name t)

  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display github notifications or not. Requires `ghub` package.
  (setq doom-modeline-github nil)

  ;; The interval of checking github.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display environment version or not
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python")
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-mu4e t)

  ;; Whether display irc notifications or not. Requires `circe' package.
  (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity))

(defun init-mode-line ()
  ;(init-doom-mode-line)

  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

;;==============================================================================
;; Font
;;==============================================================================

;IBM 3270 (https://github.com/rbanffy/3270font)
;Hack (https://github.com/source-foundry/Hack)
;NanumGothicCoding (https://github.com/naver/nanumfont/blob/master/README.md)
(defun init-font ()
  ;(print (font-family-list))
  (cond
   ((string-equal system-type "darwin") ; Font path: ~/Library/Fonts
    (progn
      ;(set-frame-font "-PfEd-IBM 3270-normal-italic-normal-*-*-130-*-*-*-0-iso10646-1")
      (set-face-attribute 'default nil :height 115 :family "Hack")
      ;(set-face-attribute 'default nil :height 115 :family "monospace")
      ;(set-face-attribute 'default nil :height 115 :family "Menlo")
      ))
   ((string-equal system-type "gnu/linux") ; Font path: ~/.local/share/fonts
    (progn
      ;(set-frame-font "-PfEd-IBM 3270-normal-italic-normal-*-*-115-*-*-*-0-iso10646-1")
      (set-face-attribute 'default nil :height 95 :family "Hack")
      ;(set-face-attribute 'default nil :height 100 :family "Inconsolata")
      ;(set-face-attribute 'default nil :height 95 :family "FreeMono")
      ;(set-face-attribute 'default nil :height 100 :family "monospace")
      ;(set-face-attribute 'default nil :height 115 :family "Ubuntu Mono")
      ))))
;To resolve the problem that cells of a table on Org mode containing Hangul are broken
;https://crazia.tistory.com/entry/Emacs-24x-%EB%B2%84%EC%A0%BC-%ED%95%9C%EA%B8%80-%ED%8F%B0%ED%8A%B8-%EC%84%A4%EC%A0%95-orgmode-%EC%9D%98-%ED%95%9C%EA%B8%80-%ED%85%8C%EC%9D%B4%EB%B8%94-%EA%B9%A8%EC%A7%80%EC%A7%80-%EC%95%8A%EA%B2%8C-%EB%B3%B4%EC%9D%B4%EA%B8%B0
(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
(setq face-font-rescale-alist
      '((".*hiragino.*" . 1.2)
        ("NanumGothicCoding" . 1.2307692307692308)))

(init-themes)
(init-mode-line)

(defun startup-on-gui ()
  (init-font)

  (tool-bar-mode -1) ; hide tool bar
  ;(menu-bar-mode -1) ; hide menu bar

  (set-frame-position (selected-frame) 0 0)
  ;(set-frame-width (selected-frame) 150)
  ;(set-frame-height (selected-frame) 100)

  (setq window-min-width (/ (display-pixel-height) 22))

  (require 'all-the-icons)

  (defun toggle-fullscreen (&optional f)
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

(defun startup-on-cui ()
  (setq window-min-width (/ (display-pixel-width) 5)))

(if (display-graphic-p)
    (startup-on-gui)
  (startup-on-cui))


(provide 'init-appearance)
