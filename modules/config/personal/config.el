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

(push '("\\.sc" . scala-mode) auto-mode-alist)

(add-hook! 'doom-init-hook #'personal|osx-gls)

(after! magit
  (setq magit-revision-show-gravatars nil))

(after! magit-hub
  (setq magithub-clone-default-directory "~/Sources"))

(after! evil
  ;; http://spacemacs.org/doc/FAQ#prevent-the-visual-selection-overriding-my-system-clipboard
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-ex-define-cmd "buffers" 'ibuffer)
  ;;(setq x-select-enable-clipboard t)
  )

(after! ivy
  (setq ivy-use-selectable-prompt t))

(after! projectile
  (setq projectile-require-project-root t))


;; packages
(def-package! launchctl
  :commands (launchctl)
  :config
  (set-evil-initial-state! 'launchctl-mode 'emacs))

;; (def-package! evil-cleverparens
;;   :init
;;   (setq evil-cleverparens-use-regular-insert t)
;;   :config
;;   (add-hook! (clojure-mode emacs-lisp-mode) #'evil-cleverparens-mode))

(def-package! persistent-scratch
  :hook (doom-post-init . persistent-scratch-setup-default)
  :init
  (setq persistent-scratch-save-file (concat doom-cache-dir ".persistent-scratch")))

(def-package! org-journal
  :defer t
  :commands (org-journal-new-entry org-journal-search-forever)
  :init
  (setq org-journal-dir "~/Dropbox (Personal)/orgs/journal/"
        org-journal-find-file 'find-file
        org-journal-carryover-items "TODO=\"TODO\"|TODO=\"[ ]\"")
  :config
  (map! :map org-journal-mode-map
        :localleader
        :n "n" #'org-journal-open-next-entry
        :n "p" #'org-journal-open-previous-entry))

(def-package! cheat-sh
  :defer t
  :commands (cheat-sh cheat-sh-region cheat-sh-search cheat-sh-list cheat-sh-search-topic)
  :config
  (set-popup-rule! "^\\*cheat.sh" :size 0.4 :quit 'current :select t))

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

(def-package! ammonite-term-repl
  :defer t
  :commands (run-ammonite))

;; mappings
(map!
 :i "M-/"    #'+company/complete
 :i "M-?"    #'dabbrev-expand

 ;; (:after evil-cleverparens
 ;;   (:map evil-cleverparens-mode-map
 ;;     :n "]" nil
 ;;     :n "[" nil))

 (:after counsel
   (:leader
     (:prefix "f"
       :desc "Locate" :n "L" #'counsel-locate))
   (:map counsel-ag-map
     [s-return] #'ivy-call-and-recenter))

 (:after ivy
   :map ivy-minibuffer-map
   [s-return]    #'ivy-call-and-recenter
   "C-v" #'ivy-scroll-up-command)

 (:after ibuffer
   (:leader
     (:prefix "b"
       :desc "IBuffer" :nv "i" #'ibuffer-jump
       :desc "Counsel IBuffer" :nv "I" #'counsel-ibuffer)))

 (:leader
   :prefix "o"
   :desc "Org Journal" :nv "j" #'org-journal-new-entry)

 (:after evil-easymotion
   :map evilem-map
   "c" #'avy-goto-char
   "C" #'avy-goto-char-2
   "d" #'avy-goto-word-1
   "D" #'avy-goto-word-0
   "l" #'avy-goto-line
   "u" #'link-hint-open-link
   "U" #'link-hint-copy-link))

;;Tweaks
;;
;; disable recentf cleaning up message
(after! recentf
  (defun *recentf-cleanup (orig-fn &rest args)
    (quiet! (apply orig-fn args)))
  (advice-add #'recentf-cleanup :around #'*recentf-cleanup))

(after! org-mode
  (add-to-list 'org-link-abbrev-alist '("nor" . "https://n.alibaba-inc.com/ops/info/host?host=%s")))

(after! dash-docs
  (setq dash-docs-docsets-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
