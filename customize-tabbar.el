(require 'tabbar)

(defun my-tabbar-buffer-groups ()
  ;;把buffer分为两组，一组名为emacs组，它的buffer名字以*开始(dired-mode和diary-mode也为emacs组)；
  ;;其它的为另一组（名为user）。
  (list (cond ((eq major-mode 'term-mode) "user")
              ((eq major-mode 'diary-mode) "other")
              ;((eq major-mode 'refine-interaction-mode) "user")
              ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; 为切换tab设置快捷键
(global-set-key [M-right] 'tabbar-forward-tab)
(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [(control tab)] 'tabbar-forward-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab) ; Ctrl+Shift+Tab
;; (global-set-key (kbd "M-2") 'tabbar-forward-tab)
;; (global-set-key (kbd "M-1") 'tabbar-backward-tab)


;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)


;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
 That is, a string used to represent it on the tab bar."
  (let ((label (if tabbar--buffer-show-groups
                   (format "[%s] " (tabbar-tab-tabset tab))
                 (format "%s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)
