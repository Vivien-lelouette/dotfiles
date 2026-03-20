;;; dracula-light-theme.el --- Dracula Light Theme  -*- lexical-binding: t; -*-

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Étienne Deparis <etienne@depar.is>
;; Author: film42
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/dracula/emacs

;;; Commentary:

;; A light variant of the Dracula color theme.
;; Shares all face definitions with the dark variant but uses
;; a light background palette with darkened accent colors.

;;; Code:
(require 'dracula-common)

(deftheme dracula-light)

;; Light palette -- same structure, inverted luminance, adjusted saturation.
;; Accent hues are darkened versions of the originals for readability on
;; light backgrounds.
(dracula-common--apply-theme
 'dracula-light
 '(;; Core backgrounds/foregrounds (inverted)
   (dracula-bg           "#f0f0e8" "#e4e4e4" "white")
   (dracula-bg-alternate "#f8f8f2" "#ffffff" "white")
   (dracula-disabled-bg  "#eaeae4" "#d0d0d0" "white")
   (dracula-fg           "#282a36" "#000000" "black")
   (dracula-current      "#e8e8e2" "#e4e4e4" "brightwhite")
   (dracula-comment      "#6272a4" "#5f5faf" "blue")
   ;; Accent colors (darkened for light bg readability)
   (dracula-cyan         "#0189cc" "#005faf" "cyan")
   (dracula-green        "#1d9a49" "#008700" "green")
   (dracula-orange       "#d47a21" "#d75f00" "red")
   (dracula-pink         "#d5398f" "#d7005f" "magenta")
   (dracula-purple       "#7c54c9" "#5f5faf" "magenta")
   (dracula-red          "#e03030" "#d70000" "red")
   (dracula-yellow       "#a8860a" "#af8700" "yellow")
   ;; UI chrome
   (dracula-selection    "#d0d0f0" "#d0d0d0" "brightwhite")
   (dracula-selection-alternate "#7c54c9" "#5f5faf" "magenta")
   (dracula-gtk-scrollbar "#a1a1a2" "unspecified" "unspecified")
   ;; Shades
   (bg2                  "#e0e0da" "#d0d0d0" "brightwhite")
   (bg3                  "#c8c8c2" "#b2b2b2" "white")
   (fg2                  "#3a3c48" "#1c1c1c" "black")
   (fg3                  "#4c4e5a" "#303030" "brightblack")
   (fg4                  "#5e606c" "#444444" "brightblack")
   (dark-red             "#fce4ec" "#ffd7d7" "brightred")     ; subtle pastel for diffs
   (dark-green           "#e4f5e8" "#d7ffd7" "brightgreen")   ; subtle pastel for diffs
   (mid-red              "#f0c0c8" "#ffafaf" "brightred")     ; refined diffs
   (mid-green            "#b8e0c0" "#afffaf" "brightgreen")   ; refined diffs
   (dark-blue            "#0189cc" "#0087ff" "brightblue")
   (hl-accent            "#e0d8f0" "#d7d7ff" "brightwhite") ; subtle violet tint
   (grab-accent           "#f5f0c8" "#ffffd7" "brightyellow")))

(provide-theme 'dracula-light)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dracula-light-theme.el ends here
