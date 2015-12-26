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

;; Configure languages which can be evaluated in Org-mode buffers.
;; By default only Emacs Lisp can be evaluated.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa . t)
   (dot . t)
   (gnuplot . t)))

;; 执行代码时的不用用户确认
(setq org-confirm-babel-evaluate nil)

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

            (define-key org-mode-map
              (kbd "C-c C-f") 'recentf-open-files)

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
                      <li><a href=\"/\">Home</a></li>
                      <li><a href=\"/categories.html\">Categories</a></li>
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
         :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ;; combine "org-static" and "org-static" into one function call
        ("org" :components ("org-notes" "org-static"))))

;; These helper functions can be used in batch mode of emacs.
(defun my-export-html (&optional force)
  (org-publish-project "org" force))

(defun my-force-export-html ()
  (my-export-html t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 下面为导出latex/pdf相关设置

;; Use XeLaTeX to export PDF in Org-mode
;; Note that 'org-latex-to-pdf-process' now is renamed to 'org-latex-pdf-process'
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Export source code using the listings package
(add-to-list 'org-latex-listings '("" "listings"))
(add-to-list 'org-latex-listings '("" "color"))

;; 自定义新的LaTex导出模板
;; https://github.com/w0mTea/An.Emacs.Tutorial.for.Vim.User/blob/master/An.Emacs.Tutorial.for.Vim.User.zh-CN.org
;; Add a template for article
(add-to-list 'org-latex-classes
          '("my-org-article-zh"
"\\documentclass{article}
\\usepackage[slantfont, boldfont]{xeCJK}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{ifplatform}  % Note: \iflinux \ifmacosx \ifcygwin don't work if shell escape is disabled.
\\ifmacosx
\\setCJKmainfont{STSong}  % Default chinese font in Mac OS
\\else
\\setCJKmainfont{SimSun}
\\fi

% \\setmainfont{DejaVu Sans} % 英文衬线字体
% \\setsansfont{DejaVu Serif} % 英文无衬线字体
% \\setmonofont{DejaVu Sans Mono} % 英文等宽字体
% \\punctstyle{DejaVu Sans} % 开明式标点格式

% \\parindent 2em
% \\usepackage{indentfirst} % 首段缩进
\\usepackage{parskip}  % 不要段缩进

\\defaultfontfeatures{Mapping=tex-text} %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。

% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

\\usepackage{listings}      % listings能方便处理程序源码
\\usepackage{caption}
\\usepackage[colorlinks,    % 将超链接以颜色来标识，而并非使用默认的方框来标识
            linkcolor=black,
            anchorcolor=black,
            citecolor=black,
            urlcolor=black
            ]{hyperref}
\\usepackage{graphicx}

% 代码设置
\\lstset{
%language=C,
basicstyle=\\ttfamily,
%columns=fixed,
numbers=left,          % where to put the line-numbers
numberstyle=\\tiny,
breaklines=true,       % sets automatic line breaking
frame=tb               % adds a frame around the code
}

[EXTRA]
"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Add a template for book
;; 和my-org-article-zh的设置差不多，增加了chapter相关内容。
(add-to-list 'org-latex-classes
             '("my-org-book-zh"
               "\\documentclass{book}
\\usepackage[slantfont, boldfont]{xeCJK} % 允许斜体和粗体
% chapter set
\\usepackage[Lenny]{fncychap}
[NO-DEFAULT-PACKAGES]
[PACKAGES]

% \\setCJKmainfont{SimSun} % 设置缺省中文字体

\\usepackage{ifplatform}  % Note: \iflinux \ifmacosx \ifcygwin don't work when shell escape is disabled.
\\ifmacosx
\\setCJKmainfont{STSong}  % Default Chinese font in Mac OS
\\else
\\setCJKmainfont{SimSun}
\\fi

% \\setmainfont{DejaVu Sans} % 英文衬线字体
% \\setsansfont{DejaVu Serif} % 英文无衬线字体
% \\setmonofont{DejaVu Sans Mono} % 英文等宽字体

% \\parindent 2em
% \\usepackage{indentfirst} % 首段缩进
\\usepackage{parskip}  % 不要段缩进

\\defaultfontfeatures{Mapping=tex-text} % 如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。

% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

\\usepackage{listings}      % listings能方便处理程序源码
\\usepackage{caption}
\\usepackage[colorlinks,    % 将超链接以颜色来标识，而并非使用默认的方框来标识
            linkcolor=black,
            anchorcolor=black,
            citecolor=black,
            urlcolor=black
            ]{hyperref}
\\usepackage{graphicx}

% 代码设置
\\lstset{
%language=C,
basicstyle=\\ttfamily,
%columns=fixed,
numbers=left,          % where to put the line-numbers
numberstyle=\\tiny,
breaklines=true,       % sets automatic line breaking
frame=tb               % adds a frame around the code
}

[EXTRA]
"
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; 默认使用自定义的LaTex导出模板my-org-article-zh
;; 在org文件中单独设置LATEX_CLASS可使用指定的模板
(setq org-latex-default-class "my-org-article-zh")


;; 定制在org-mode中，拖拽图片文件到emacs时的处理方式
;; 参考：http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/
;; NOTE: 下面代码仅在OSX系统中有效。
(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|svg\\|jp[e]?g\\)\\>"))
    (cond
     ;; In case of C-drag-n-drop, insert "#+ATTR_HTML: :width 500px\n" addtionally
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_HTML: :width 500px\n")
      (insert (concat  "#+CAPTION: " (file-name-base fname) "\n"))
      (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname))))
      (let ((target-dir (concat (file-name-directory (buffer-file-name)) "./images/")))
        (if (file-exists-p target-dir)
            (copy-file fname target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir))))
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (concat  "#+CAPTION: " (file-name-base fname) "\n"))
      (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname))))
      (let ((target-dir (concat (file-name-directory (buffer-file-name)) "./images/")))
        (if (file-exists-p target-dir)
            (copy-file fname target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir))))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))

(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
