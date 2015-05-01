;; 设置默认打开文件时，显示整个目录
;; (setq org-startup-folded 'content)

;; 大纲以缩进的方式显示。
;;(setq org-startup-indented t)

;; 导出html时，abc_def中的def变会为abc的下标形式。10^24中的24会变为10的上标形式。
;; 下面配置将禁止这种转换：
;;(setq-default org-use-sub-superscripts nil) ;; deprecated configuration
(setq org-export-with-sub-superscripts nil)

;; 设置导出时保留换行，也可以在文件中设置 #+OPTIONS: \n:t
(setq org-export-preserve-breaks t)

;; 设置图片宽度不一定为实际大小
(setq org-image-actual-width nil)

;; 保留代码块中的缩进空格。
(setq org-src-preserve-indentation t)

;; fontify code in code blocks
;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
;; 让源代码在org-mode中变得好看
(setq org-src-fontify-natively t)

;; 格式化代码块
;; http://stackoverflow.com/questions/15773354/indent-code-in-org-babel-src-blocks
(defun my-indent-org-block-automatically ()
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(add-hook 'org-mode-hook (lambda ()
                           ;; 在org-mode中设置当一行太行时自动换行，而不是隐藏。
                           (setq truncate-lines nil)
                           ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs
                           (define-key org-mode-map "\M-q" 'toggle-truncate-lines)

                           ;; 让不同级别的标题采用不同大小的字体
                           (set-face-attribute 'org-level-1 nil :height 1.3 :bold t)
                           (set-face-attribute 'org-level-2 nil :height 1.2 :bold t)
                           (set-face-attribute 'org-level-3 nil :height 1.1 :bold t)

                           (define-key org-mode-map
                             "\C-\M-\\" 'my-indent-org-block-automatically)
                           ))

