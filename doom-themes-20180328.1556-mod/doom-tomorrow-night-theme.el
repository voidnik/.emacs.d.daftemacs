;;; doom-tomorrow-night-theme.el
(require 'doom-themes)

(defgroup doom-tomorrow-night-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-tomorrow-night-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-tomorrow-night-theme
  :type '(or integer boolean))

(def-doom-theme doom-tomorrow-night
  "A theme based off of Chris Kempson's Tomorrow Dark."

  ;; name        gui       256       16
  ((bg         '("#1d1f21" nil       nil          ))
   (bg-alt     '("#232527" nil       nil          ))
   (base0      '("#0d0d0d" "black"   "black"      ))
   (base1      '("#1b1b1b" "#1b1b1b"              ))
   (base2      '("#212122" "#1e1e1e"              ))
   (base3      '("#292b2b" "#292929" "brightblack"))
   (base4      '("#3f4040" "#3f3f3f" "brightblack"))
   (base5      '("#5c5e5e" "#525252" "brightblack"))
   (base6      '("#757878" "#6b6b6b" "brightblack"))
   (base7      '("#969896" "#979797" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"      ))
   (fg         '("#c5c8c6" "#c5c5c5" "white"))
   (fg-alt     (doom-darken fg 0.6))

   (grey       '("#5a5b5a" "#5a5a5a" "brightblack"))
   (red        '("#cc6666" "#cc6666" "red"))
   (orange     '("#de935f" "#dd9955" "brightred"))
   (yellow     '("#f0c674" "#f0c674" "yellow"))
   (green      '("#b5bd68" "#b5bd68" "green"))
   (blue       '("#81a2be" "#88aabb" "brightblue"))
   (dark-blue  '("#41728e" "#41728e" "blue"))
   (teal       blue) ; FIXME replace with real teal
   (magenta    '("#c9b4cf" "#c9b4cf" "magenta"))
   (violet     '("#b294bb" "#b294bb" "brightmagenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))

   (diff-default                 '("#abb2bf" "#afafaf" "brightblack"))
   (diff-background              '("#282c34" "#333333" "brightblack"))
   (diff-background-red          '("#4c3840" "#5f5f5f" "red"))
   (diff-background-green        '("#3d4a41" "#5f5f5f" "green"))
   (diff-background-blue         '("#38394c" "#444444" "blue"))
   (diff-bright-background-red   '("#744a5b" "#744a5b" "brightred"))
   (diff-bright-background-green '("#3f6d54" "#3f6d54" "brightgreen"))
   (diff-bright-background-blue  '("#4e5079" "#4e5079" "brightblue"))
   (diff-highlight               '("#2a2b2a" "#2a2a2a" "brightblack"))

   ;; face categories
   (highlight      dark-blue)
   (vertical-bar   base0)
   (selection      (doom-lighten bg 0.1))
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.1))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-tomorrow-night-padded-modeline
      (if (integerp doom-tomorrow-night-padded-modeline)
          doom-tomorrow-night-padded-modeline
        4))))

  ;; --- faces ------------------------------
  ((doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;; diff
   (diff-removed :background diff-background-red :foreground red)
   (diff-added :background diff-background-green :foreground green)
   (diff-hunk-header :background diff-background-blue :bold bold :foreground blue)
   (diff-file-header :bold bold)
   (diff-header :background diff-background :foreground blue)
   (diff-context :foreground diff-default)
   (diff-refine-added :foreground green :background diff-bright-background-green)
   (diff-refine-removed :background diff-bright-background-red :foreground red)

   ;; ediff
   (ediff-fine-diff-B :inherit 'diff-refine-added)
   (ediff-current-diff-B :inherit 'diff-added)
   (ediff-fine-diff-A :inherit 'diff-refine-removed)
   (ediff-current-diff-A :inherit 'diff-removed)
   (ediff-fine-diff-C :foreground blue :background diff-bright-background-blue)
   (ediff-current-diff-C :background diff-background-blue :foreground blue)
   (ediff-even-diff-A :background diff-highlight)
   (ediff-even-diff-B :background diff-highlight)
   (ediff-even-diff-C :background diff-highlight)
   (ediff-odd-diff-A :background diff-highlight)
   (ediff-odd-diff-B :background diff-highlight)
   (ediff-odd-diff-C :background diff-highlight))

  ;; --- variables --------------------------
  ;; ()
  )

(provide 'doom-tomorrow-night-theme)
;;; doom-tomorrow-night-theme.el ends here
