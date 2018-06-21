;;;  -*- lexical-binding: t; -*-

(defun personal|osx-gls ()
  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.  We must look for `gls' after `exec-path-from-shell' was
  ;; initialized to make sure that `gls' is in `exec-path'
  (when IS-MAC
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first")))))

(add-hook! 'doom-init-hook #'personal|osx-gls)

(after! magit
  (setq magit-revision-show-gravatars nil))

(after! magit-hub
  (setq magithub-clone-default-directory "~/Sources"))

(after! evil
  ;; http://spacemacs.org/doc/FAQ#prevent-the-visual-selection-overriding-my-system-clipboard
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-ex-define-cmd "buffers" 'ibuffer))

(after! ivy
  (setq ivy-use-selectable-prompt t))

(after! org
  (setq +org-dir (expand-file-name "~/Documents/orgs")))

;; packages
(def-package! help-fns+
  :commands (describe-keymap))

(def-package! launchctl
  :commands (launchctl)
  :config
  (set-evil-initial-state! 'launchctl-mode 'emacs))

(def-package! evil-cleverparens
  :init
  (setq evil-cleverparens-use-regular-insert t)
  :config
  (add-hook! (clojure-mode emacs-lisp-mode) #'evil-cleverparens-mode))

(def-package! persistent-scratch
  :hook (doom-init . persistent-scratch-setup-default)
  :init
  (setq persistent-scratch-save-file (concat doom-cache-dir ".persistent-scratch")))

(def-package! ibuffer-vc
  :defer t
  :init
  (after! ibuffer
    (define-ibuffer-column size-h
      (:name "Size"
             :inline t
             :summarizer
             (lambda (column-strings)
               (let ((total 0))
                 (dolist (string column-strings)
                   (setq total
                         ;; like, ewww ...
                         (+
                          (let ((number (float (string-to-number string))))
                            (cond
                             ((string-match-p "K" string)
                              (* number 1000))
                             ((string-match-p "M" string)
                              (* number 1000000))
                             (t number)))
                          total)))
                 (file-size-human-readable total 'si))))
      (file-size-human-readable (buffer-size) 'si))
    (setq ibuffer-formats
          '((mark modified vc-status-mini read-only
                  " " (name 25 25 :left :elide)
                  " " (size-h 9 -1 :right)
                  " " (mode 16 16 :left :elide)

                  " " filename-and-process)
            (mark " " (name 30 -1)
                  " " filename)))
    (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)))

(set-evil-initial-state! 'process-menu-mode 'emacs)

;; mappings
(map!
 :i "A-/"    #'+company/complete


 (:leader
   (:prefix "b"
     :desc "Switch to Scratch Buffer" :n "S" #'doom/switch-to-scratch-buffer
     :desc "Create Sratch Buffer" :n "N" #'doom/create-scratch-buffer)
   (:prefix "o"
     :desc "Treemacs" :n "n" #'+treemacs/toggle
     :desc "Treemacs: find file" :n "N" #'treemacs-find-file))

 (:after evil-cleverparens
   (:map evil-cleverparens-mode-map
     :n "]" nil
     :n "[" nil))

 (:after counsel
   (:leader
     (:prefix "f"
       :desc "Locate" :n "L" #'counsel-locate))
   (:map counsel-ag-map
     "M-RET" #'ivy-call-and-recenter))

 (:after dired
   (:map dired-mode-map
     :n "SPC" nil))

 (:after ivy
   (:leader
     (:prefix "/"
       :desc "Last Search" :nv "/" #'ivy-resume))

   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "A->"    #'end-of-buffer
   "A-<"    #'beginning-of-buffer
   "M-O"    #'ivy-call-and-recenter
   "M-z"    #'undo
   "A-v"    #'ivy-scroll-down-command
   "C-v"    #'ivy-scroll-up-command
   "C-h"    (kbd "DEL"))
 (:after ibuffer
   (:leader
     (:prefix "b"
       :desc "IBuffer" :nv "i" #'ibuffer-jump
       :desc "Counsel IBuffer" :nv "I" #'counsel-ibuffer)))

 (:after company-box
   (:map company-box-mode-map
     :i "C-k" #'company-box--prev-line))

 (:prefix "go"
   :nv "c" #'avy-goto-char
   :nv "C" #'avy-goto-char-2
   :nv "o" #'avy-goto-char-timer
   :nv "w" #'avy-goto-word-1
   :nv "W" #'avy-goto-word-0
   :nv "l" #'avy-goto-line
   :nv "s" #'avy-isearch
   :nv "u" #'link-hint-open-link
   :nv "U" #'link-hint-copy-link))

;;Tweaks
;;
;; disable recentf cleaning up message
(after! recentf
  (defun *recentf-cleanup (orig-fn &rest args)
    (quiet! (apply orig-fn args)))
  (advice-add #'recentf-cleanup :around #'*recentf-cleanup))
