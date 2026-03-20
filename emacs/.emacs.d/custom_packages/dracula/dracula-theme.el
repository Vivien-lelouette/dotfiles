;;; dracula-theme.el --- Dracula Dark Theme  -*- lexical-binding: t; -*-

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Étienne Deparis <etienne@depar.is>
;; Author: film42
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/dracula/emacs

;;; Commentary:

;; A dark color theme available for a number of editors.
;; This theme tries as much as possible to follow the consensual
;; specification (see URL `https://spec.draculatheme.com/').

;;; Code:
(require 'dracula-common)

(deftheme dracula)

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(dracula-common--apply-theme
 'dracula
 '(;; Upstream theme color
   (dracula-bg           "#232530" "color-234" "black")       ; official background
   (dracula-bg-alternate "#282a36" "color-235" "black")       ; alternate background
   (dracula-disabled-bg  "#1f212c" "color-233" "black")       ; disabled background
   (dracula-fg           "#f8f8f2" "#ffffff" "brightwhite")   ; official foreground
   (dracula-current      "#282a36" "#282a36" "brightblack")   ; official current-line/selection
   (dracula-comment      "#6272a4" "#5f5faf" "blue")          ; official comment
   (dracula-cyan         "#8be9fd" "#87d7ff" "brightcyan")    ; official cyan
   (dracula-green        "#50fa7b" "#5fff87" "green")         ; official green
   (dracula-orange       "#ffb86c" "#ffaf5f" "brightred")     ; official orange
   (dracula-pink         "#ff79c6" "#ff87d7" "magenta")       ; official pink
   (dracula-purple       "#bd93f9" "#af87ff" "brightmagenta") ; official purple
   (dracula-red          "#ff5555" "#ff8787" "red")           ; official red
   (dracula-yellow       "#f1fa8c" "#ffff87" "yellow")        ; official yellow
   (dracula-selection    "#44475a" "#303030" "selection")
   (dracula-selection-alternate "#BD93F9" "#BD93F9" "selection")
   (dracula-gtk-scrollbar "#a1a1a2" "unspecified" "unspecified")
   ;; Other colors
   (bg2                  "#373844" "#121212" "brightblack")
   (bg3                  "#565761" "#444444" "brightblack")
   (fg2                  "#e2e2dc" "#e4e4e4" "brightwhite")
   (fg3                  "#ccccc7" "#c6c6c6" "white")
   (fg4                  "#b6b6b2" "#b2b2b2" "white")
   (dark-red             "#3a1520" "#870000" "red")
   (dark-green           "#1a3524" "#00af00" "green")
   (mid-red              "#5a2535" "#af0000" "red")           ; refined diffs
   (mid-green            "#2a5e3a" "#00af00" "green")         ; refined diffs
   (dark-blue            "#0189cc" "#0087ff" "brightblue")
   (hl-accent            "#302b45" "#302b45" "brightblack")
   (grab-accent           "#3a3520" "#3a3520" "brightyellow")))

(provide-theme 'dracula)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dracula-theme.el ends here
