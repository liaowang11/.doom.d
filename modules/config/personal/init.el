;;; config/personal/init.el -*- lexical-binding: t; -*-

;;theme
(setq doom-theme 'doom-molokai)

;;font settings
(set-face-attribute 'default nil :font
                    (format   "%s:pixelsize=%d"  "Iosevka" 15))

(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "PingFang SC" :size 15)))

;; Use the OS X Emoji font for Emoticons
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))

(setq doom-font (font-spec :family "Iosevka" :size 15)
      doom-variable-pitch-font (font-spec :family "SF Pro Text")
      doom-unicode-font (font-spec :family "PingFang SC")
      doom-big-font (font-spec :family "Iosevka" :size 19))

(after! evil
  (setq evil-move-beyond-eol t)
  ;; http://spacemacs.org/doc/FAQ#prevent-the-visual-selection-overriding-my-system-clipboard
  (fset 'evil-visual-update-x-selection 'ignore))

(after! neotree
 (setq neo-show-hidden-files nil))


