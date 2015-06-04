;; 设置默认打开文件时，显示整个目录
;; (setq org-startup-folded 'content)

;; 大纲以缩进的方式显示。
;; (setq org-startup-indented t)

;; 导出html时，abc_def中的def变会为abc的下标形式。10^24中的24会变为10的上标形式。
;; 下面配置将禁止这种转换：
;; (setq-default org-use-sub-superscripts nil) ;; deprecated configuration
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

(add-hook 'org-mode-hook
          (lambda ()
            ;; 在org-mode中设置当一行太行时自动换行，而不是隐藏。
            (setq truncate-lines nil)

            ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs
            (define-key org-mode-map "\M-q" 'toggle-truncate-lines)

            (define-key org-mode-map
              "\C-\M-\\" 'my-indent-org-block-automatically)

            (auto-fill-mode -1) ;; disable auto-fill-mode

            ;; 让不同级别的标题采用不同大小的字体
            (set-face-attribute 'org-level-1 nil :height 1.3 :bold t)
            (set-face-attribute 'org-level-2 nil :height 1.2 :bold t)
            (set-face-attribute 'org-level-3 nil :height 1.1 :bold t)))

;; Disable link about "Validate" in html
(setq org-html-validation-link nil)

(setq org-html-postamble t) ;; t means use string from org-html-postamble-format
(setq org-html-postamble-format  ;; change this to your style
      '(("en"
         "<p class=\"author\">Author: <a href=\"http://www.aandds.com\"> %a</a></p>\n
<p class=\"date\">Created: %d</p>\n
<p class=\"date\">Last updated: <span class=\"timestamp-wrapper\">
<span class=\"timestamp\">&lt;%C&gt;</span></span></p>\n
<p class=\"creator\">Creator: %c</p>\n
<p class=\"validation\">%v</p>")))

;; Refer to: http://orgmode.org/manual/Publishing.html
(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("org-notes"               ;; Used to export .org file
         :base-directory "~/www/"  ;; directory holds .org files
         :base-extension "org"     ;; process .org file only
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap t
         :sitemap-filename "sitemap0-auto.org"
         :sitemap-title "Archives"
         :sitemap-file-entry-format "%t (%d)"     ;; %t: title, %d: date
         :sitemap-sort-files anti-chronologically ;; newer date first
         :table-of-contents t
         :html-preamble "
      <nav class=\"navbar navbar-inverse navbar-fixed-top\">
          <div class=\"container-fluid\">
              <div class=\"navbar-header\">
                  <a class=\"navbar-brand\" href=\"#\">AandDS</a>
              </div>
              <div>
                  <ul class=\"nav navbar-nav\">
                      <li class=\"active\"><a href=\"/\">Home</a></li>
                      <li><a href=\"/archives.html\">Archives</a></li>
                      <li><a href=\"/about.html\">About</a></li>
                  </ul>
              </div>
          </div>
      </nav>"                        ;; change this to your style
         :style-include-default nil  ;; Disable the default css style
         )

        ("org-static"                ;; Used to publish static files
         :base-directory "~/www/"
         :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ;; combine "org-static" and "org-static" into one function call
        ("org" :components ("org-notes" "org-static"))))
