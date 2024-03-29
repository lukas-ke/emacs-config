;;; -*- coding: utf-8; lexical-binding: t -*-

(deftheme luk-bright
  "A bright Emacs theme.

Many colors are taken from the Leuven theme:
URL `https://github.com/fniessen/emacs-leuven-theme'")

(let*
    ;; Local definitions used for the customization...
    ((class '((class color) (min-colors 89))) ;; Apply face when at least 89 colors available, see `defface'
     (fringe-background-color "#DCDCDC")
     (buffer-background-color "#FFFFFF")
     (heading-color "#335EA8")
     (code-inline '(:foreground "#006400" :background "#FDFFF7"))
     (column '(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :foreground "#E6AD4F" :background "#FFF2DE"))
     (completion-inline '(:weight normal :foreground "#000000" :inherit hl-line))
     (diff-added '(:background "#DDFFDD"))
     (diff-changed '(:foreground "#0000FF" :background "#DDDDFF"))
     (diff-header '(:weight bold :foreground "#800000" :background "#FFFFAF"))
     (diff-hunk-header '(:foreground "#990099" :background "#FFEEFF"))
     (diff-none '(:foreground "#888888"))
     (diff-refine-added '(:background "#97F295"))
     (diff-refine-removed '(:background "#FFB6BA"))
     (diff-removed '(:background "#FEE8E9"))
     (directory `(:weight normal :foreground "blue" :background ,buffer-background-color))
     (highlight-yellow '(:background "#F6FECD"))
     (link '(:weight normal :underline t :foreground "#006DAF"))
     (link-no-underline '(:weight normal :foreground "#006DAF"))
     (marked-line '(:foreground "#AA0000" :background "#FFAAAA"))
     (match '(:weight bold :background "#FFFF00")) ; occur patterns + match in helm for files + match in Org files.
     (ol1 `(:weight normal :foreground ,heading-color :background ,buffer-background-color))
     (ol2 `(:height 1.0 :weight normal :foreground "#3C3C3C" :background ,buffer-background-color))
     (ol3 `(:height 1.0 :weight normal :foreground "#3C3C3C" :background ,buffer-background-color))
     (ol4 '(:height 1.0 :weight normal :slant normal :foreground "#3C3C3C"))
     (ol5 '(:height 1.0 :weight normal :slant normal :foreground "#3C3C3C"))
     (ol6 '(:height 1.0 :weight normal :slant normal :foreground "#3C3C3C"))
     (ol7 '(:height 1.0 :weight normal :slant normal :foreground "#3C3C3C"))
     (ol8 '(:height 1.0 :weight normal :slant normal :foreground "#3C3C3C"))
     (paren-matched '(:background "#9EDCA3"))
     (paren-unmatched '(:weight bold :foreground "black" :background "#FFF200"))
     (region '(:background "#8ED3FF"))
     (shadow '(:foreground "#7F7F7F"))
     (string '(:foreground "#AC1515"))
     (tab '(:foreground "#E8E8E8" :background "white"))
     (trailing '(:foreground "#E8E8E8" :background "#FFFFAB"))
     (xml-attribute '(:foreground "#F36335"))
     (xml-tag '(:foreground "#AE1B9A")))

  ;; ... the actual customization
  (custom-theme-set-faces
   'luk-bright
   `(default ((,class (:foreground "#000000" :background ,buffer-background-color))))
   `(bold ((,class (:weight bold :foreground "black"))))
   `(bold-italic ((,class (:weight bold :slant italic :foreground "black"))))
   `(italic ((,class (:slant italic :foreground "#1A1A1A"))))
   `(underline ((,class (:underline t))))
   `(cursor ((,class (:background "#000000"))))

   ;; Background for the fringe
   `(fringe ((,class (:foreground "#4C9ED9" :background ,fringe-background-color))))

   `(highlight ((,class '(:background "#B6D6FD"))))
   `(region ((,class ,region)))
   `(secondary-selection ((,class ,match))) ; used by org-mode for highlighting matched entries and keywords.
   `(nobreak-space ((,class (:background "#CCE8F6"))))

   ;; icomplete
   `(icomplete-selected-match ((,class (:foreground "black" :background "#CCE8FF")))) ;; (For `fido-vertical-mode')

   ;; isearch
   `(isearch ((,class (:underline "black" :foreground "white" :background "#5974AB"))))
   `(isearch-fail ((,class (:weight bold :foreground "black" :background "#FFCCCC"))))
   `(lazy-highlight ((,class (:foreground "black" :background "#FFFF00")))) ; isearch others (see `match').
   `(trailing-whitespace ((,class ,trailing)))
   `(query-replace ((,class (:inherit isearch))))

   ;; whitespace (built-in, for visible whitespace, long line indications)
   `(whitespace-hspace ((,class (:foreground "#D2D2D2"))))
   `(whitespace-indentation ((,class ,tab)))
   `(whitespace-line ((,class (:foreground "#CC0000" :background "#FFFF88"))))
   `(whitespace-tab ((,class ,tab)))
   `(whitespace-trailing ((,class ,trailing)))

   ;; Mode line faces.
   `(mode-line ((,class (:box (:line-width 1 :color "#1A2F54") :foreground "#FFFFFF" :background "#335EA8"))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color "#4E4E4C") :foreground "#F0F0EF" :background "#9B9C97"))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "white"))))
   `(mode-line-emphasis ((,class (:weight bold :foreground "white"))))
   `(mode-line-highlight ((,class (:foreground "yellow"))))

   ;; Escape and prompt faces.
   `(minibuffer-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(minibuffer-noticeable-prompt ((,class (:weight bold :foreground "black" :background "gold"))))
   `(escape-glyph ((,class (:foreground "#008ED1"))))
   `(error ((,class (:weight bold :foreground "#C74051"))))
   `(warning ((,class (:foreground "#BFA307"))))
   `(success ((,class (:foreground "#1A893B"))))

   ;; Font lock faces.
   `(font-lock-builtin-face ((,class (:foreground "#006FE0"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#707070"))))
   `(font-lock-comment-face ((,class (:foreground "#505050"))))
   `(font-lock-constant-face ((,class (:foreground "#D0372D"))))
   `(font-lock-doc-face ((,class (:foreground "#036A07"))))
   `(font-lock-function-name-face ((,class (:weight normal :foreground "#006699"))))
   `(font-lock-keyword-face ((,class (:bold nil :foreground "#0000FF"))))
   `(font-lock-preprocessor-face ((,class (:foreground "#808080"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil))))
   `(font-lock-string-face ((,class ,string)))
   `(font-lock-type-face ((,class (:weight normal :foreground "#6434A3"))))
   `(font-lock-variable-name-face ((,class (:weight normal :foreground "#BA36A5"))))
   `(font-lock-warning-face ((,class (:weight bold :foreground "red"))))

   ;; Button and link faces.
   `(link ((,class ,link)))
   `(link-visited ((,class (:underline t :foreground "#E5786D"))))
   `(button ((,class (:underline t :foreground "#006DAF"))))
   `(header-line ((,class (:foreground "black" :background "#F0F0F0"))))

   ;; diff (built-in)
   `(diff-added ((,class ,diff-added)))
   `(diff-changed ((,class ,diff-changed)))
   `(diff-context ((,class ,diff-none)))
   `(diff-file-header ((,class ,diff-header)))
   `(diff-file1-hunk-header ((,class (:foreground "dark magenta" :background "#EAF2F5"))))
   `(diff-file2-hunk-header ((,class (:foreground "#2B7E2A" :background "#EAF2F5"))))
   `(diff-function ((,class (:foreground "#CC99CC"))))
   `(diff-header ((,class ,diff-header)))
   `(diff-hunk-header ((,class ,diff-hunk-header)))
   `(diff-index ((,class ,diff-header)))
   `(diff-indicator-added ((,class (:foreground "#3A993A" :background "#CDFFD8"))))
   `(diff-indicator-changed ((,class (:background "#DBEDFF"))))
   `(diff-indicator-removed ((,class (:foreground "#CC3333" :background "#FFDCE0"))))
   `(diff-refine-added ((,class ,diff-refine-added)))
   `(diff-refine-change ((,class (:background "#DDDDFF"))))
   `(diff-refine-removed ((,class ,diff-refine-removed)))
   `(diff-removed ((,class ,diff-removed)))

   ;; smerge-mode (built-in)
   `(smerge-mine ((,class ,diff-changed)))
   `(smerge-other ((,class ,diff-added)))
   `(smerge-base ((,class ,diff-removed)))
   `(smerge-markers ((,class (:background "#FFE5CC"))))
   `(smerge-refined-change ((,class (:background "#AAAAFF"))))

   ;; ediff (built-in)
   `(ediff-current-diff-A ((,class (:background "#FFDDDD"))))
   `(ediff-current-diff-B ((,class (:background "#DDFFDD"))))
   `(ediff-current-diff-C ((,class (:background "cyan"))))
   `(ediff-even-diff-A ((,class (:background "light grey"))))
   `(ediff-even-diff-B ((,class (:background "light grey"))))
   `(ediff-fine-diff-A ((,class (:background "#FFAAAA"))))
   `(ediff-fine-diff-B ((,class (:background "#55FF55"))))
   `(ediff-odd-diff-A ((,class (:background "light grey"))))
   `(ediff-odd-diff-B ((,class (:background "light grey"))))

   ;; flyspell (built-in spell-checker)
   `(flyspell-duplicate ((,class (:underline (:style wave :color "#F4EB80") :background "#FAF7CC" :inherit nil))))
   `(flyspell-incorrect ((,class (:underline (:style wave :color "#FAA7A5") :background "#F4D7DA":inherit nil))))

   ; ace-jump-mode (jump to buffer positions)
   `(ace-jump-face-foreground ((,class (:weight bold :foreground "black" :background "#FEA500"))))

   ;; avy package (jump to buffer positions by typing)
   `(avy-background-face ((,class (:background "#A9A9A9"))))
   `(avy-lead-face ((,class (:weight bold :foreground "black" :background "#FEA500"))))

   ;; browse-kill-ring
   `(browse-kill-ring-separator-face ((,class (:foreground "red"))))

   ;; calendar (built-in)
   `(calendar-month-header ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(calendar-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(calendar-weekday-header ((,class (:weight bold :foreground "#1662AF"))))
   `(calendar-weekend-header ((,class (:weight bold :foreground "#4E4E4E"))))

   ;; calfw (A large calendar view)
   `(cfw:face-annotation ((,class (:foreground "green" :background "red"))))
   `(cfw:face-day-title ((,class (:foreground "#C9C9C9"))))
   `(cfw:face-default-content ((,class (:foreground "#2952A3"))))
   `(cfw:face-default-day ((,class (:weight bold))))
   `(cfw:face-disable ((,class (:foreground "DarkGray"))))
   `(cfw:face-grid ((,class (:foreground "#DDDDDD"))))
   `(cfw:face-header ((,class (:foreground "#1662AF" :background "white" :weight bold))))
   `(cfw:face-holiday ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(cfw:face-periods ((,class (:foreground "white" :background "#668CD9" :slant italic))))
   `(cfw:face-saturday ((,class (:foreground "#4E4E4E" :background "white" :weight bold))))
   `(cfw:face-select ((,class (:foreground "#4A95EB" :background "#EDF1FA"))))
   `(cfw:face-sunday ((,class (:foreground "#4E4E4E" :background "white" :weight bold))))
   `(cfw:face-title ((,class (:height 2.0 :foreground "#676767" :weight bold :inherit variable-pitch))))
   `(cfw:face-today ((,class (:foreground "#4F4A3D" :background "#FFFFCC"))))
   `(cfw:face-today-title ((,class (:foreground "white" :background "#1766B1"))))
   `(cfw:face-toolbar ((,class (:background "white"))))
   `(cfw:face-toolbar-button-off ((,class (:foreground "#CFCFCF" :background "white"))))
   `(cfw:face-toolbar-button-on ((,class (:foreground "#5E5E5E" :background "#F6F6F6"))))

   ;; company (auto completion)
   ;; http://company-mode.github.io/
   `(company-tooltip-common-selection ((,class (:weight bold :foreground "#0474B6" :inherit company-tooltip-selection))))
   `(company-tooltip-selection ((,class (:weight bold :foreground "black" :background "#C1E0FD"))))
   `(company-tooltip-annotation-selection ((,class (:weight bold :foreground "#818181"))))
   `(company-tooltip-common ((,class (:weight normal :foreground "#0474B6" :inherit company-tooltip))))
   `(company-tooltip ((,class (:foreground "black" :background "#F7F7F7")))) ; tooltip suffix.
   `(company-tooltip-annotation ((,class (:weight normal :foreground "#818181")))) ; Annotation.
   `(company-preview ((,class ,completion-inline)))
   `(company-preview-common ((,class ,completion-inline)))
   `(company-scrollbar-bg ((,class (:background "#EBF4FE"))))
   `(company-scrollbar-fg ((,class (:background "#D1DAE4"))))

   ;; compare-windows (built-in)
   `(compare-windows ((,class (:background "#FFFF00"))))

   ;; compilation mode (built-in)
   `(compilation-error ((,class (:foreground "#ED1C24"))))
   `(compilation-warning ((,class (:foreground "#FFC20E"))))
   `(compilation-info ((,class (:foreground "#202020"))))
   `(compilation-line-number ((,class (:foreground "#A535AE"))))
   `(compilation-mode-line-exit ((,class (:foreground "white"))))
   `(compilation-mode-line-fail ((,class (:weight bold :foreground "black"))))
   `(compilation-mode-line-run ((,class (:weight bold :foreground "black"))))

   ;; css-mode (built-in)
   `(css-property ((,class (:foreground "#00AA00"))))
   `(css-selector ((,class (:weight bold :foreground "blue"))))

   ;; custom (built-in, interactive customization interface)
   `(custom-button ((,class (:box (:line-width 2 :style released-button) :foreground "#eeeeee" :background "#335EA8"))))
   `(custom-button-mouse ((,class (:box (:line-width 2 :style released-button) :foreground "white" :background "#638BCF"))))

   `(custom-button-pressed ((,class (:box (:line-width 2 :style pressed-button) :foreground "black" :background "light grey"))))
   `(custom-button-pressed-unraised ((,class (:underline t :foreground "magenta4"))))
   `(custom-button-unraised ((,class (:underline t))))
   `(custom-changed ((,class (:foreground "white" :background "blue"))))
   `(custom-comment ((,class (:background "gray85"))))
   `(custom-comment-tag ((,class (:foreground "blue4"))))
   `(custom-documentation ((,class (nil))))
   `(custom-face-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold))))
   `(custom-group-tag ((,class (:height 1.2 :weight bold :foreground "blue1"))))
   `(custom-group-tag-1 ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "red1"))))
   `(custom-invalid ((,class (:foreground "yellow" :background "red"))))
   `(custom-link ((,class (:underline t :foreground "blue1"))))
   `(custom-modified ((,class (:foreground "white" :background "blue"))))
   `(custom-rogue ((,class (:foreground "pink" :background "black"))))
   `(custom-saved ((,class (:underline t))))
   `(custom-set ((,class (:foreground "blue" :background "white"))))
   `(custom-state ((,class (:foreground "green4"))))
   `(custom-themed ((,class (:foreground "white" :background "blue1"))))
   `(custom-variable-button ((,class (:weight bold :underline t))))
   `(custom-variable-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "blue1"))))
   `(custom-visibility ((,class ,link)))
   `(diff-hl-change ((,class (:foreground "blue3" :background "#DBEDFF"))))
   `(diff-hl-delete ((,class (:foreground "red3" :background "#FFDCE0"))))
   `(diff-hl-dired-change ((,class (:weight bold :foreground "black" :background "#FFA335"))))
   `(diff-hl-dired-delete ((,class (:weight bold :foreground "#D73915"))))
   `(diff-hl-dired-ignored ((,class (:weight bold :foreground "white" :background "#C0BBAB"))))
   `(diff-hl-dired-insert ((,class (:weight bold :foreground "#B9B9BA"))))
   `(diff-hl-dired-unknown ((,class (:foreground "white" :background "#3F3BB4"))))
   `(diff-hl-insert ((,class (:foreground "green4" :background "#CDFFD8"))))
   `(diff-hl-unknown ((,class (:foreground "white" :background "#3F3BB4"))))
   `(diary-face ((,class (:foreground "#87C9FC"))))

   ;; dired (built-in)
   `(dired-directory ((,class ,directory)))
   `(dired-header ((,class ,directory)))
   `(dired-ignored ((,class (:strike-through t :foreground "red"))))
   `(dired-mark ((,class ,marked-line)))
   `(dired-marked ((,class ,marked-line)))
   `(dired-symlink ((,class (:foreground "#1F8DD6"))))
   `(diredfl-compressed-file-suffix ((,class (:foreground "#000000" :background "#FFF68F"))))
   `(diredp-compressed-file-suffix ((,class (:foreground "red"))))
   `(diredp-date-time ((,class (:foreground "purple"))))
   `(diredp-dir-heading ((,class ,directory)))
   `(diredp-dir-name ((,class ,directory)))
   `(diredp-dir-priv ((,class ,directory)))
   `(diredp-exec-priv ((,class (:background "#03C03C"))))
   `(diredp-executable-tag ((,class (:foreground "ForestGreen" :background "white"))))
   `(diredp-file-name ((,class (:foreground "black"))))
   `(diredp-file-suffix ((,class (:foreground "#C0C0C0"))))
   `(diredp-flag-mark-line ((,class ,marked-line)))
   `(diredp-ignored-file-name ((,class ,shadow)))
   `(diredp-read-priv ((,class (:background "#0A99FF"))))
   `(diredp-write-priv ((,class (:foreground "white" :background "#FF4040"))))

   ;; eldoc (built-in emacs lisp help for entering sexps)
   `(eldoc-highlight-function-argument ((,class (:weight bold :foreground "red" :background "#FFE4FF"))))

   ;; `file-name-shadow-mode' minor mode (built in, dims ignored part
    ;; of filename in minibuffer)
   `(file-name-shadow ((,class ,shadow)))

   ;; flycheck  (alternative to built-in flymake)
   `(flycheck-error ((,class (:underline (:color "#FE251E" :style wave)))))
   `(flycheck-error-list-filename ((,class (:foreground "#A535AE"))))
   `(flycheck-error-list-line-number ((,class (:foreground "#A535AE"))))
   `(flycheck-fringe-error ((,class (:foreground "#FE251E"))))
   `(flycheck-fringe-info ((,class (:foreground "#158A15"))))
   `(flycheck-fringe-warning ((,class (:foreground "#F4A939"))))
   `(flycheck-info ((,class (:underline (:color "#158A15" :style wave) :weight bold))))
   `(flycheck-warning ((,class (:underline (:color "#F4A939" :style wave)))))

   ;; auctex package (latex editing)
   `(font-latex-bold-face ((,class (:weight bold :foreground "black"))))
   `(font-latex-italic-face ((,class (:slant italic :foreground "#1A1A1A"))))
   `(font-latex-math-face ((,class (:foreground "blue"))))
   `(font-latex-sectioning-1-face ((,class (:family "Sans Serif" :height 2.7 :weight bold :foreground "cornflower blue"))))
   `(font-latex-sectioning-2-face ((,class ,ol1)))
   `(font-latex-sectioning-3-face ((,class ,ol2)))
   `(font-latex-sectioning-4-face ((,class ,ol3)))
   `(font-latex-sectioning-5-face ((,class ,ol4)))
   `(font-latex-sedate-face ((,class (:foreground "#FF5500"))))
   `(font-latex-string-face ((,class (:weight bold :foreground "#0066FF"))))
   `(font-latex-verbatim-face ((,class (:foreground "#000088" :background "#FFFFE0" :inherit nil))))

   ;; git-timemachine package
   `(git-timemachine-commit ((,class ,diff-removed)))
   `(git-timemachine-minibuffer-author-face ((,class ,diff-added)))
   `(git-timemachine-minibuffer-detail-face ((,class ,diff-header)))

   ;; `highlight-changes-mode' (built-in, highlight modifications in buffer)
   `(highlight-changes ((,class (:foreground nil :background "yellow"))))
   `(highlight-changes-delete ((,class (:strike-through nil :foreground nil :underline t))))

   ;; hl-line (built-in, highlight current line)
   `(hl-line ((,class :background "#CCE8FF")))
   `(hl-tags-face ((,class (:background "#E8E8FF"))))
   `(holiday-face ((,class (:foreground "#777777" :background "#E4EBFE"))))
   `(html-helper-bold-face ((,class (:weight bold :foreground "black"))))
   `(html-helper-italic-face ((,class (:slant italic :foreground "black"))))
   `(html-helper-underline-face ((,class (:underline t :foreground "black"))))
   `(html-tag-face ((,class (:foreground "blue"))))
   `(ilog-non-change-face ((,class (:height 2.0 :foreground "#6434A3"))))
   `(ilog-change-face ((,class (:height 2.0 :foreground "#008200"))))
   `(ilog-echo-face ((,class (:height 2.0 :foreground "#006FE0"))))
   `(ilog-load-face ((,class (:foreground "#BA36A5"))))
   `(ilog-message-face ((,class (:foreground "#808080"))))
   `(indent-guide-face ((,class (:foreground "#D3D3D3"))))

   ;; info (built-in for info nodes, as shown by "M-x info" for e.g.
   ;; emacs lisp manual)
   `(info-file ((,class (:family "Sans Serif" :height 1.8 :weight bold :box (:line-width 1 :color "#0000CC") :foreground "cornflower blue" :background "LightSteelBlue1"))))
   `(info-header-node ((,class (:underline t :foreground "orange"))))
   `(info-header-xref ((,class (:underline t :foreground "dodger blue"))))
   `(info-index-match ((,class (:weight bold :foreground nil :background "#FDBD33"))))
   `(info-menu-header ((,class ,ol2)))
   `(info-menu-star ((,class (:foreground "black"))))
   `(info-node ((,class (:underline t :foreground "blue"))))
   `(info-quoted-name ((,class ,code-inline)))
   `(info-string ((,class ,string)))
   `(info-title-1 ((,class ,ol1)))
   `(info-title-2 ((,class ,ol2)))
   `(info-title-3 ((,class ,ol3)))
   `(info-title-4 ((,class ,ol4)))
   `(info-xref ((,class (:underline t :foreground "#006DAF"))))
   `(info-xref-visited ((,class (:underline t :foreground "magenta4"))))

   ;; linum package (built-in, display line numbers in left margin)
   `(linum ((,class (:foreground "#9A9A9A" :background "#EDEDED"))))
   ;; `log-view-mode' (built in, view svn, rcs logs et. al)
   `(log-view-file ((,class (:foreground "#0000CC" :background "#EAF2F5"))))
   `(log-view-message ((,class (:foreground "black" :background "#EDEA74"))))

   ;; `lsp-mode' package (language server integration)
   `(lsp-modeline-code-actions-preferred-face ((,class (:foreground "#000000" :background "#FFF68F"))))
   `(lsp-ui-doc-background ((,class (:background "#F6FECD"))))
   `(lsp-ui-sideline-code-action ((,class (:foreground "#000000" :background "#FFF68F"))))

   ;; Magit package (git interface)
   `(magit-blame-heading ((,class (:overline "#A7A7A7" :foreground "red" :background "#E6E6E6"))))
   `(magit-blame-hash ((,class (:overline "#A7A7A7" :foreground "red" :background "#E6E6E6"))))
   `(magit-blame-name ((,class (:overline "#A7A7A7" :foreground "#036A07" :background "#E6E6E6"))))
   `(magit-blame-date ((,class (:overline "#A7A7A7" :foreground "blue" :background "#E6E6E6"))))
   `(magit-blame-summary ((,class (:overline "#A7A7A7" :weight bold :foreground "#707070" :background "#E6E6E6"))))
   `(magit-diff-added ((,class ,diff-added)))
   `(magit-diff-removed ((,class ,diff-removed)))
   `(magit-diff-file-heading ((,class (:weight bold :foreground "#4183C4"))))
   `(magit-diff-hunk-heading ((,class ,diff-hunk-header)))
   `(magit-diff-context ((,class ,diff-none)))

   ;; Some magit header-lines indicate possible key-presses (e.g. for
   ;; the interactive rebase log selection) and the
   ;; "help-keybinding"-face looks bad on many types of backgrounds,
   ;; so just use a boring gray for the header.
   `(magit-header-line ((,class (:foreground "black" :background "#F0F0F0"))))
   `(magit-header-line-log-select ((,class (:weight bold :foreground "black" :background "#F0F0F0"))))

   `(magit-item-mark ((,class ,marked-line)))
   `(magit-section-highlight ((,class (:background  "#F6FECD"))))
   `(magit-section-heading ((,class (:height 1.1 :foreground "black" :background "white"))))
   `(magit-section-secondary-heading ((,class (:height 1.1 :foreground "black" :background "white"))))
   `(magit-section-child-count ((,class (:height 1.1 :foreground "black" :background "white"))))
   `(magit-branch-local ((,class (:weight bold :foreground "SkyBlue4"))))
   `(magit-branch-remote ((,class (:weight bold :foreground "DarkOliveGreen4"))))
   `(magit-branch-current ((,class (:box 1 :inherit magit-branch-local))))

   ;; `makefile-mode' (built-in)
   `(makefile-space ((,class (:background "hot pink"))))
   `(makefile-targets ((,class (:weight bold :foreground "blue"))))

   ;; `markdown-mode' (https://jblevins.org/projects/markdown-mode/)
   `(markdown-bold-face ((,class (:inherit bold))))
   `(markdown-header-face-1 ((,class ,ol1)))
   `(markdown-header-face-2 ((,class ,ol2)))
   `(markdown-header-face-3 ((,class ,ol3)))
   `(markdown-header-face-4 ((,class ,ol4)))
   `(markdown-header-face-5 ((,class ,ol5)))
   `(markdown-header-face-6 ((,class ,ol6)))
   `(markdown-inline-code-face ((,class ,code-inline)))
   `(markdown-italic-face ((,class (:inherit italic))))
   `(markdown-language-keyword-face ((,class (:inherit org-block-begin-line))))
   `(markdown-link-face ((,class ,link-no-underline)))
   `(markdown-pre-face ((,class (:foreground "#9C5A3C" :family "courier new"))))
   `(markdown-code-face ((,class (:foreground "#9C5A3C" :family "courier new"))))
   `(markdown-url-face ((,class ,link)))
   `(markdown-hr-face ((,class (:inherit org-block))))

   ;; Basic face, used for grep matches.
   `(match ((,class ,match)))

   ;; multiple-cursors (https://github.com/magnars/multiple-cursors.el)
   `(mc/cursor-bar-face ((,class (:height 1.0 :foreground "#1664C4" :background "#1664C4"))))
   `(mc/cursor-face ((,class (:inverse-video t))))
   `(mc/region-face ((,class (:inherit region))))

   ;; nxml-mode (built-in)
   `(nxml-attribute-local-name-face ((,class ,xml-attribute)))
   `(nxml-attribute-value-delimiter-face ((,class (:foreground "green4"))))
   `(nxml-attribute-value-face ((,class (:foreground "green4"))))
   `(nxml-comment-content-face ((,class (:slant italic :foreground "red"))))
   `(nxml-comment-delimiter-face ((,class (:foreground "red"))))
   `(nxml-element-local-name ((,class ,xml-tag)))
   `(nxml-element-local-name-face ((,class (:foreground "blue"))))
   `(nxml-processing-instruction-target-face ((,class (:foreground "purple1"))))
   `(nxml-tag-delimiter-face ((,class (:foreground "blue"))))
   `(nxml-tag-slash-face ((,class (:foreground "blue"))))

   ;; org-mode
   `(org-agenda-block-count ((,class (:weight bold :foreground "#A5A5A5"))))
   `(org-agenda-calendar-event ((,class (:weight bold :foreground "#3774CC" :background "#E4EBFE"))))
   `(org-agenda-calendar-sexp ((,class (:foreground "#327ACD" :background "#F3F7FC"))))
   `(org-agenda-clocking ((,class (:foreground "black" :background "#EEC900"))))
   `(org-agenda-column-dateline ((,class ,column)))
   `(org-agenda-current-time ((,class (:underline t :foreground "#1662AF"))))
   `(org-agenda-date ((,class (:weight bold :foreground "#1662AF"))))
   `(org-agenda-date-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(org-agenda-date-weekend ((,class (:weight bold :foreground "#4E4E4E"))))
   `(org-agenda-diary ((,class (:weight bold :foreground "green4" :background "light blue"))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground "gold2"))))
   `(org-agenda-done ((,class (:foreground "#555555"))))
   `(org-agenda-filter-category ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-filter-effort ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-filter-regexp ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-filter-tags ((,class (:weight bold :foreground "orange"))))
   `(org-agenda-restriction-lock ((,class (:background "#E77D63"))))
   `(org-agenda-structure ((,class (:weight bold :foreground "#1F8DD6"))))
   `(org-archived ((,class (:foreground "gray70"))))
   `(org-beamer-tag ((,class (:box (:line-width 1 :color "#FABC18") :foreground "#2C2C2C" :background "#FFF8D0"))))
   `(org-block ((,class (:foreground "#000000" :background "#E9E9E9"))))
   `(org-block-background ((,class (:background "#FFFFE0"))))
   `(org-block-begin-line ((,class (:foreground "#FFFFFF" :background "#335EA8"))))
   `(org-block-end-line
     ;; Note: Unlike the org-block-begin-line, I don't set a
     ;; background for the org-block-end-line because when a section
     ;; containing a source block at the end is collapsed, the heading
     ;; gets that color trailing.
     ;; Maybe that's a bug in  org (version was 9.5.2), could also be due
     ;; to some other package I use
     ((,class (:weight bold :foreground "#335EA8"))))
   `(org-clock-overlay ((,class (:foreground "white" :background "SkyBlue4"))))
   `(org-code ((,class ,code-inline)))
   `(org-column ((,class ,column)))
   `(org-column-title ((,class ,column)))
   `(org-date ((,class (:underline t :foreground "#00459E"))))
   `(org-default ((,class (:foreground "#333333" :background "#FFFFFF"))))
   `(org-dim ((,class (:foreground "#AAAAAA"))))
   `(org-document-info ((,class (:foreground "#484848"))))
   `(org-document-info-keyword ((,class (:weight bold :foreground "#464646"))))
   `(org-document-title ((,class (:weight bold :foreground ,heading-color :height 1.2))))
   `(org-done ((,class (:weight bold :box (:line-width 2 :color "#1DAC08") :foreground "#1DAC08" :background "#A8FA9C"))))
   `(org-drawer ((,class (:weight bold :foreground "#787878" :background "#EAFFEA"))))
   `(org-ellipsis
     ;; Set as many properties as possible to avoid the collapsed
     ;; outline indicator taking on the style of elements around it.
     ;; The ➤-symbol I use for ellipsis gets very ugly in italic :)
     ((,class (:underline nil :foreground "#335EA8" :slant normal :box nil :background: ,buffer-background-color))))
   `(org-example ((,class (:foreground "blue" :background "#EAFFEA"))))
   `(org-footnote ((,class (:underline t :foreground "#008ED1"))))
   `(org-formula ((,class (:foreground "chocolate1"))))
   `(org-headline-done ((,class (:height 1.0))))
   `(org-hide ((,class (:foreground "#FFFFFF"))))
   `(org-inlinetask ((,class (:box (:line-width 1 :color "#EBEBEB") :foreground "#777777" :background "#FFFFD6"))))
   `(org-latex-and-related ((,class (:foreground "#336699" :background "white"))))
   `(org-level-1 ((,class ,ol1)))
   `(org-level-2 ((,class ,ol2)))
   `(org-level-3 ((,class ,ol3)))
   `(org-level-4 ((,class ,ol4)))
   `(org-level-5 ((,class ,ol5)))
   `(org-level-6 ((,class ,ol6)))
   `(org-level-7 ((,class ,ol7)))
   `(org-level-8 ((,class ,ol8)))
   `(org-link ((,class ,link)))
   `(org-list-dt ((,class (:weight bold :foreground "#335EA8"))))
   `(org-macro ((,class (:weight bold :foreground "#EDB802"))))
   `(org-meta-line ((,class (:weight bold :slant normal :foreground "#464646" :background ,buffer-background-color))))
   `(org-mode-line-clock ((,class (:box (:line-width 1 :color "#335EA8") :foreground "black" :background "#FFA335"))))
   `(org-mode-line-clock-overrun ((,class (:weight bold :box (:line-width 1 :color "#335EA8") :foreground "white" :background "#FF4040"))))
   `(org-number-of-items ((,class (:weight bold :foreground "white" :background "#79BA79"))))
   `(org-property-value ((,class (:foreground "#00A000"))))
   `(org-quote ((,class (:slant italic :foreground "dim gray" :background "#FFFFE0"))))
   `(org-scheduled ((,class (:foreground "#333333"))))
   `(org-scheduled-previously ((,class (:foreground "#1466C6"))))
   `(org-scheduled-today ((,class (:weight bold :foreground "#4F4A3D" :background "#FFFFCC"))))
   `(org-sexp-date ((,class (:foreground "#3774CC"))))
   `(org-special-keyword ((,class (:weight bold :foreground "#606060" :background "#EAFFEA"))))
   `(org-table ((,class (:foreground "dark green" :background "#EAFFEA")))) ;; :inherit fixed-pitch))))
   `(org-tag ((,class (:weight normal :slant italic :foreground "#9A9FA4" :background "white"))))
   `(org-target ((,class (:foreground "#FF6DAF"))))
   `(org-time-grid ((,class (:foreground "#B8B8B8"))))
   `(org-todo ((,class (:weight semi-bold :box (:line-width 2 :color "#D8ABA7") :foreground "#ED1C24" :background "#FFC1CB"))))
   `(org-upcoming-deadline ((,class (:foreground "#FF5555"))))
   `(org-verbatim ((,class (:family "courier new" :foreground "#0066CC" :background "#F7FDFF"))))
   `(org-verse ((,class (:slant italic :foreground "dim gray" :background "#EEEEEE"))))
   `(org-warning ((,class (:weight bold :foreground "black" :background "#CCE7FF"))))
   `(org-checkbox ((,class (:height 1.0))))

   ;; Custom faces for my org-keywords
   `(luk-org-todo-started ((,class (:foreground "#6F3198" :background ,buffer-background-color))))
   `(luk-org-todo-canceled ((,class (:foreground "#000000" :background ,buffer-background-color))))

   ;; My hydra-faces
   `(luk-hydra-caption-face ((,class (:weight bold :foreground ,heading-color))))

   `(luk-modeline-flash-face ((,class (:background "#CCA157"))))

   `(luk-mode-line-modified ((,class (:foreground "#F25E63"))))

   ;; outline-mode (built-in)
   `(outline-1 ((,class ,ol1)))
   `(outline-2 ((,class ,ol2)))
   `(outline-3 ((,class ,ol3)))
   `(outline-4 ((,class ,ol4)))
   `(outline-5 ((,class ,ol5)))
   `(outline-6 ((,class ,ol6)))
   `(outline-7 ((,class ,ol7)))
   `(outline-8 ((,class ,ol8)))

   ;; Faces indicating parenthesis match/mismatch
   `(paren-face-match ((,class ,paren-matched)))
   `(paren-face-mismatch ((,class ,paren-unmatched)))
   `(paren-face-no-match ((,class ,paren-unmatched)))

   ;; rng-valid package (XML validation with RELAX NG)
   `(rng-error ((,class (:weight bold :foreground "red" :background "#FBE3E4"))))

   ;; `sh-mode' (built-in mode for editing shell scripts)
   `(sh-heredoc ((,class (:foreground "blue" :background "#EEF5FE"))))
   `(sh-quoted-exec ((,class (:foreground "#FF1493"))))

   ;; Basic face for shadowed text.
   `(shadow ((,class ,shadow))) ; Used for grep context lines.

   ;; Basic faces for parenthesis match/mismatch indications
   `(show-paren-match ((,class ,paren-matched)))
   `(show-paren-mismatch ((,class ,paren-unmatched)))

   ;; term-mode default face
   `(term ((,class (:foreground "#333333" :background ,buffer-background-color))))

   ;; tex-mode
   `(tex-verbatim ((,class (:foreground "blue"))))

   ;; built-in ui-elements
   `(tool-bar ((,class (:box (:line-width 1 :style released-button) :foreground "black"))))
   `(tooltip ((,class (:foreground "black" :background "light yellow"))))

   ;; vc (built in package: version control)
   `(vc-annotate-face-3F3FFF ((,class (:foreground "#3F3FFF" :background "black"))))
   `(vc-annotate-face-3F6CFF ((,class (:foreground "#3F3FFF" :background "black"))))
   `(vc-annotate-face-3F99FF ((,class (:foreground "#3F99FF" :background "black"))))
   `(vc-annotate-face-3FC6FF ((,class (:foreground "#3F99FF" :background "black"))))
   `(vc-annotate-face-3FF3FF ((,class (:foreground "#3FF3FF" :background "black"))))
   `(vc-annotate-face-3FFF56 ((,class (:foreground "#4BFF4B" :background "black"))))
   `(vc-annotate-face-3FFF83 ((,class (:foreground "#3FFFB0" :background "black"))))
   `(vc-annotate-face-3FFFB0 ((,class (:foreground "#3FFFB0" :background "black"))))
   `(vc-annotate-face-3FFFDD ((,class (:foreground "#3FF3FF" :background "black"))))
   `(vc-annotate-face-56FF3F ((,class (:foreground "#4BFF4B" :background "black"))))
   `(vc-annotate-face-83FF3F ((,class (:foreground "#B0FF3F" :background "black"))))
   `(vc-annotate-face-B0FF3F ((,class (:foreground "#B0FF3F" :background "black"))))
   `(vc-annotate-face-DDFF3F ((,class (:foreground "#FFF33F" :background "black"))))
   `(vc-annotate-face-F6FFCC ((,class (:foreground "black" :background "#FFFFC0"))))
   `(vc-annotate-face-FF3F3F ((,class (:foreground "#FF3F3F" :background "black"))))
   `(vc-annotate-face-FF6C3F ((,class (:foreground "#FF3F3F" :background "black"))))
   `(vc-annotate-face-FF993F ((,class (:foreground "#FF993F" :background "black"))))
   `(vc-annotate-face-FFC63F ((,class (:foreground "#FF993F" :background "black"))))
   `(vc-annotate-face-FFF33F ((,class (:foreground "#FFF33F" :background "black"))))

   ;; web-mode
   ;; https://web-mode.org/
   `(web-mode-current-element-highlight-face ((,class (:background "#99CCFF"))))
   `(web-mode-folded-face ((,class (:box (:line-width 1 :color "#777777") :foreground "#9A9A6A" :background "#F3F349"))))
   `(web-mode-html-attr-name-face ((,class ,xml-attribute)))
   `(web-mode-html-tag-bracket-face ((,class ,xml-tag)))
   `(web-mode-html-tag-face ((,class ,xml-tag)))
   `(web-mode-part-face ((,class (:background "#FFFFE0"))))
   `(web-mode-script-face ((,class (:background "#EFF0F1"))))

   ;; `which-key'-package (displays possible follow-up keys for a keypress after a delay)
   `(which-key-local-map-description-face ((,class (:weight bold :background "#F3F7FC" :inherit which-key-command-description-face))))

   ;; which-function-mode (built-in, shows the function, or say
   ;; org-heading, point is inside on mode-line)
   `(which-func ((,class (:foreground "#99D9EA"))))

   ;; widget (built-in, see wid-edit.el)
   `(widget-button ((,class ,link)))
   `(widget-button-pressed ((,class (:foreground "red"))))
   `(widget-documentation ((,class (:foreground "green4"))))
   `(widget-field ((,class (:background "gray85"))))
   `(widget-inactive ((,class (:foreground "#202020"))))
   `(widget-single-line-field ((,class (:background "gray85"))))

   ;; yasnippet (interactively expand templates from abbreviations)
   `(yas-field-debug-face ((,class (:foreground "white" :background "#A62154"))))
   `(yas-field-highlight-face ((,class (:box (:line-width 1 :color "#838383") :foreground "black" :background "#D4DCD8"))))

   ;; Face for the bookmark-package fringe indicator
   `(bookmark-face ((,class (:foreground "#335EA8" :background ,fringe-background-color))))

   ;; Anzu package (current match/num-matches in mode-line for
   ;; isearch)
   `(anzu-mode-line ((,class (:foreground "yellow"))))

   `(luk-appt-popup ((,class (:foreground "black" :background "#F5E49C"))))
   `(luk-appt-popup-border ((,class (:foreground "black" :background "#787878"))))

   ;; re-builder (built-in)
   ;;
   ;; Match groups using distinct (rather than tasteful-) colors:
   ;; - yellow background for the default group
   ;; - a grayscale progression for the background of groups 1-3
   ;; - black or white foreground as appropriate for the various backgrounds
   `(reb-match-0 ((,class (:foreground "#000000" :background "#FFF542"))))
   `(reb-match-1 ((,class (:foreground "#000000" :background "#9F9F9F"))))
   `(reb-match-2 ((,class (:foreground "#FFFFFF" :background "#606060"))))
   `(reb-match-3 ((,class (:foreground "#FFFFFF" :background "#202020"))))))

(provide-theme 'luk-bright)
