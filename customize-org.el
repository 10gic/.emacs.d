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

;; 设置导出latex时，默认的图片宽度。默认为.9\textwidth，这里设置得更窄一些。
(setq org-latex-image-default-width ".7\\textwidth")

;; 忽略应用于文本页的审美条件，试图用最严格的标准来放置浮动图形，这样图片会尽量出现在它放置的位置附近。
(setq org-latex-default-figure-position '!htbp)

;; 设置latex表格为“三线表”。
(setq org-latex-tables-booktabs t)

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

            ;; 设置org-table中使用字体集fontset-myfixed，这样org-table中的中英文就能对齐显示了。
            (if (display-graphic-p)
                (set-face-attribute 'org-table nil :fontset "fontset-myfixed"))

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
<p class=\"date\">Created: <span class=\"timestamp-wrapper\"><span class=\"timestamp\">&lt;%d&gt;</span></span></p>\n
<p class=\"date\">Last updated: <span class=\"timestamp-wrapper\"><span class=\"timestamp\">&lt;%C&gt;</span></span></p>\n
<p class=\"creator\">Creator: %c</p>\n
<p class=\"validation\">%v</p>")))

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(setq my-html-preamble (get-string-from-file "~/.emacs.d/html-preamble.tmpl"))

;; Refer to: http://orgmode.org/manual/Publishing.html
(require 'ox-publish)
(setq org-publish-project-alist
      `(
        ("org-notes-html"          ;; Used to export .org file
         :base-directory "~/www/"  ;; directory holds .org files
         :base-extension "org"     ;; process .org file only
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil         ;; NOTE: set :auto-sitemap to t would lead to publish very slow if there are many org files
         :sitemap-filename "sitemap0-auto.org"
         :sitemap-title "Archives"
         :sitemap-file-entry-format "%t (%d)"     ;; %t: title, %d: date
         :sitemap-sort-files anti-chronologically ;; newer date first
         :table-of-contents t
         :html-preamble ,my-html-preamble         ;; change this to your style
         :style-include-default nil  ;; Disable the default css style
         )

        ("org-static-html"            ;; Used to publish static files
         :base-directory "~/www/"
         :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|ico\\|svg"
         :publishing-directory "~/public_html/"
         :recursive t
         :exclude "blog/images/.*\\|attachments/.*"
         :publishing-function org-publish-attachment
         )

        ("org-static-blog-img"         ;; Used to publish static files
         :base-directory "~/www/blog/images/"
         :base-extension "png\\|jpg\\|gif\\|ico\\|svg"
         :publishing-directory "~/public_html/blog/images/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org-static-others"            ;; Used to publish static files
         :base-directory "~/www/attachments/"
         :base-extension "png\\|pdf\\|tex\\|c"
         :publishing-directory "~/public_html/attachments/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("pdf"
         :base-directory "~/www2pdf.tmp/"
         :base-extension "org"
         :publishing-directory "~/public_pdf/"
         :recursive t
         :exclude "categories.org\\|archives.org\\|sitemap0-auto.org"
         :publishing-function org-latex-publish-to-pdf
         )

        ;; combine multiple projects into one project
        ("html" :components ("org-notes-html" "org-static-html" "org-static-blog-img" "org-static-others"))
        ("html-static" :components ("org-notes-html" "org-static-html"))
        ))

;; export all html related files
(defun my-export-html (&optional force)
  (org-publish-project "html" force))

;; same as my-export-html, but more faster
(defun my-export-html-fast (&optional force)
  (progn
    (org-publish-project "html-static" force)
    (message-no-newline (shell-command-to-string "rsync -a --delete --exclude '*.pdf' --exclude '*.tex' --exclude '*.sh' --out-format='updating %f' ~/www/blog/images/ ~/public_html/blog/images/"))
    (message-no-newline (shell-command-to-string "rsync -a --delete --out-format='updating %f' ~/www/attachments/ ~/public_html/attachments/"))
    ))

(defun my-export-pdf (&optional force)
  (org-publish-project "pdf" force))

;; original function `message' would append newline
;; this function would not append newline
(defun message-no-newline (msg)
  (if (not (string= msg ""))    ; if msg is empty, do nothing
      (message (replace-regexp-in-string "\n$" "" msg))))

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
\\setcounter{secnumdepth}{4}   % 设置对4级目录进行编号（默认只对三级目录进行编号）

\\usepackage{geometry}
\\geometry{left=3.0cm,right=2.5cm,top=2.5cm,bottom=2.5cm}   % 调整页边距

\\usepackage[PunctStyle=kaiming]{xeCJK}      % 设置标点为“开明”格式（即句末点号用全角，其他半角）
[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\setmainfont{SimSun}

% \\setmainfont{DejaVu Sans} % 英文衬线字体
% \\setsansfont{DejaVu Serif} % 英文无衬线字体
% \\setmonofont{DejaVu Sans Mono} % 英文等宽字体

\\xeCJKsetup{
  CJKecglue  = \\hskip 0.2em plus 0.08\\baselineskip,   % 设置自动增加的中英文之间的间隔的宽度（默认值太宽）
  xCJKecglue = true}                                    % 让上面的设置对人为输入的中英文之间的空格也有效

% \\parindent 2em
% \\usepackage{indentfirst} % 首段缩进
\\usepackage{parskip}  % 不要段缩进

\\defaultfontfeatures{Mapping=tex-text} % 如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。

% 中文断行
\\XeTeXlinebreaklocale \"zh\"   % allow linebreaks
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

\\usepackage{amsmath}       % 这个宏包提供了很多扩展数学公式的环境，如cases
\\usepackage{amssymb}       % 引入这个宏包后，可以使用 \mathbb{R} 等

\\usepackage{caption}
\\usepackage[colorlinks,    % 将超链接以颜色来标识，而并非使用默认的方框来标识
            linkcolor=black,
            anchorcolor=black,
            citecolor=black,
            urlcolor=black
            ]{hyperref}

\\usepackage{graphicx}

\\usepackage{svg}
%% org-mode中插入svg图片时，需要上面的包。
%% 不过由于\pdffilemoddate没有在XeTex中实现等原因，无法正常用xelatex导出svg图片。
%% 参考：http://tex.stackexchange.com/questions/84837/svg-from-inkscape-doesnt-work-in-xelatex）

\\usepackage{animate}        % 可以用它“支持”gif动画

\\usepackage{tabularx}       % 定制表格时可能会用到，如 #+ATTR_LaTeX: :environment tabularx :width \textwidth :align l|l|X

\\usepackage{booktabs}    % 定制org-latex-tables-booktabs时，需要这个包。

\\usepackage{listings}      % listings能方便处理程序源码
\\newcommand*{\\mycommentstyle}[1]{%
  \\begingroup
    \\fontseries{lc}%
    \\fontshape{it}%
    \\selectfont
    \\lstset{columns=fullflexible}%
    #1%
  \\endgroup
}

% 代码设置
\\lstset{
%language=C,
basicstyle=\\ttfamily,
commentstyle=\\mycommentstyle,   % 这个格式使得代码中的注释部分显得更加紧凑
%columns=fixed,
numbers=left,          % where to put the line-numbers
numberstyle=\\tiny,
showstringspaces=false,
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

;; 设置默认的LaTex导出模板
;; 如果在org文件中单独设置LATEX_CLASS，可以覆盖这个设置
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
         (img-regexp "\\(png\\|gif\\|svg\\|jp[e]?g\\)\\>"))
    (cond
     ;; In case of C-drag-n-drop, insert "#+ATTR_HTML: :width 500px\n" addtionally
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_HTML: :width 500px\n")
      (insert (concat  "#+CAPTION: " (file-name-base fname) "\n"))
      (insert (concat  "#+NAME: fig:" (file-name-base fname) "\n"))
      (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname))))
      (let ((target-dir (concat (file-name-directory (buffer-file-name)) "./images/")))
        (if (file-exists-p target-dir)
            (copy-file fname target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir))))
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (concat  "#+CAPTION: " (file-name-base fname) "\n"))
      (insert (concat  "#+NAME: fig:" (file-name-base fname) "\n"))
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

;; In emacs-w32 in Cygwin. drag-n-drop event looks like
;; (drag-n-drop (#<window 26 on 1.org> 466 (323 . 240) 779451523 nil 466 (29 . 10) nil (323 . 0) (11 . 24)) (C:\Users\user1\Desktop\1.png))
;; the first element is type of event,
;; the second element is position,
;; the 3rd element is the files.
(if (memq system-type '(cygwin))
  (defun my-dnd-func (event)
    (interactive "e")
    (goto-char (nth 1 (event-start event)))
    (x-focus-frame nil)
    (let* ((fname (car (caddr event)))
           ;; Change C:\Users\user1\Desktop\1.png to /cygdrive/c/Users/user1/Desktop/1.png
           ;;(fname-unix (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "cygpath --unix '" fname "'"))))
           (fname-unix (cygwin-convert-file-name-from-windows fname))
           (img-regexp "\\(png\\|gif\\|svg\\|jp[e]?g\\)\\>")
           (target-dir (concat (file-name-directory (buffer-file-name)) "./images/")))
      (cond
       ;; In case of C-drag-n-drop, insert "#+ATTR_HTML: :width 500px\n" addtionally
       ((and  (eq 'C-drag-n-drop (car event))
              (string-match img-regexp fname))
        (insert "#+ATTR_HTML: :width 500px\n")
        (insert (concat  "#+CAPTION: " (file-name-base fname-unix) "\n"))
        (insert (concat  "#+NAME: fig:" (file-name-base fname-unix) "\n"))
        (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname-unix))))
        (if (file-exists-p target-dir)
            (copy-file fname-unix target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir)))
       ((and  (eq 'drag-n-drop (car event))
              (string-match img-regexp fname))
        (insert (concat  "#+CAPTION: " (file-name-base fname-unix) "\n"))
        (insert (concat  "#+NAME: fig:" (file-name-base fname-unix) "\n"))
        (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname-unix))))
        (if (file-exists-p target-dir)
            (copy-file fname-unix target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir)))
       ;; regular drag and drop on file
       (t
        (insert (format "[[%s]]\n" fname))))))
  )

(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
