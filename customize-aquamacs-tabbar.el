(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "s-1") 'tabbar-select-tab-1) ; Command + 1
      (global-set-key (kbd "s-2") 'tabbar-select-tab-2)
      (global-set-key (kbd "s-3") 'tabbar-select-tab-3)
      (global-set-key (kbd "s-4") 'tabbar-select-tab-4)
      (global-set-key (kbd "s-5") 'tabbar-select-tab-5)
      (global-set-key (kbd "s-6") 'tabbar-select-tab-6)
      (global-set-key (kbd "s-7") 'tabbar-select-tab-7)
      (global-set-key (kbd "s-8") 'tabbar-select-tab-8)
      (global-set-key (kbd "s-9") 'tabbar-select-tab-9))
  (progn
    (global-set-key (kbd "M-1") 'tabbar-select-tab-1) ; Alt + 1
    (global-set-key (kbd "M-2") 'tabbar-select-tab-2)
    (global-set-key (kbd "M-3") 'tabbar-select-tab-3)
    (global-set-key (kbd "M-4") 'tabbar-select-tab-4)
    (global-set-key (kbd "M-5") 'tabbar-select-tab-5)
    (global-set-key (kbd "M-6") 'tabbar-select-tab-6)
    (global-set-key (kbd "M-7") 'tabbar-select-tab-7)
    (global-set-key (kbd "M-8") 'tabbar-select-tab-8)
    (global-set-key (kbd "M-9") 'tabbar-select-tab-9)))

;; Workaround for performance issue
;; http://stackoverflow.com/questions/8520531/tabbar-mode-brings-editing-to-a-crawling-halt
;; (setq tabbar-use-images nil)

(defun my-tabbar-buffer-groups ()
  ;; 把buffer分为两组：
  ;; 一组名为emacs组，它的buffer名字以*开始(dired-mode和diary-mode也为emacs组)；
  ;; 其它的为另一组（名为user）。
  (list (cond ((eq major-mode 'term-mode) "user")
              ((eq major-mode 'diary-mode) "other")
              ;; ((eq major-mode 'refine-interaction-mode) "user")
              ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; 为切换tab设置快捷键
;; (global-set-key [M-right] 'tabbar-forward-tab)
;; (global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [(control tab)] 'tabbar-forward-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab) ; Ctrl+Shift+Tab
