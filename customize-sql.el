
;;关联ddl后缀到sql-mode
(setq auto-mode-alist
      (append '(("\\.ddl$" . sql-mode))
              auto-mode-alist))
