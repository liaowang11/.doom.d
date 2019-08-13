;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


(setq json-reformat:indent-width 2
      js-indent-level 2
      org-directory (expand-file-name "~/Dropbox (Personal)/orgs")
      magit-repository-directories '(("~/Sources" . 2) ("~/Documents/AliSrc" . 2))
      +ivy-buffer-icons t
      projectile-project-search-path '("~/Sources" "~/Fiddle" "~/Documents/AliSrc/lippi"))

(setq sql-connection-alist
      '((bwh-fiddle
         (sql-user "root")
         (sql-product 'mysql)
         (sql-port 33066)
         (sql-server "172.96.231.108")
         (sql-database "fiddle"))))
