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

(after! magit-hub
  (setq magithub-clone-default-directory "~/Sources"))

;; packages
(def-package! help-fns+
  :commands (describe-keymap))

(def-package! evil-cleverparens
  :init
  (setq evil-cleverparens-use-regular-insert t)
  :config
  (add-hook! (clojure-mode emacs-lisp-mode) #'evil-cleverparens-mode))

;; mappings
(map!
 :i "A-/"    #'+company/complete

 (:after evil-cleverparens
   (:map evil-cleverparens-mode-map
     :n "]" nil
     :n "[" nil))

 (:after counsel
   (:map counsel-ag-map
     "M-RET" #'ivy-call-and-recenter))

 (:after ivy
   (:leader
     (:prefix "/"
       :desc "Last Search" :nv "/" #'ivy-resume))

   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "M-O"  #'ivy-call-and-recenter
   "M-z"    #'undo
   "A-v"    #'ivy-scroll-down-command
   "C-v"    #'ivy-scroll-up-command))
