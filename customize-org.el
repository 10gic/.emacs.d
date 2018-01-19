;; 设置默认打开文件时，显示整个目录
;; (setq org-startup-folded 'content)

;; 大纲以缩进的方式显示。
;; (setq org-startup-indented t)

;; 导出html时，abc_def中的def变会为abc的下标形式。10^24中的24会变为10的上标形式。
;; 下面配置将禁止这种转换：
;; (setq-default org-use-sub-superscripts nil) ; deprecated configuration
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
            ;; 在org-mode中设置当一行太长时自动换行，而不是隐藏。
            (setq truncate-lines nil)

            ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs
            (define-key org-mode-map "\M-q" 'toggle-truncate-lines)

            (define-key org-mode-map
              "\C-\M-\\" 'my-indent-org-block-automatically)

            (define-key org-mode-map
              (kbd "C-c C-f") 'recentf-open-files)

            ;; 使用M-g跳转到指定行时，展开整个文档
            (define-key org-mode-map
              "\M-g" (lambda (n) (interactive "nGoto line: ") (outline-show-all) (goto-line n)))

            (auto-fill-mode -1) ;; disable auto-fill-mode

            ;; 让不同级别的标题采用不同大小的字体
            (set-face-attribute 'org-level-1 nil :height 1.3 :bold t)
            (set-face-attribute 'org-level-2 nil :height 1.2 :bold t)
            (set-face-attribute 'org-level-3 nil :height 1.1 :bold t)))

;; Disable link about "Validate" in html
(setq org-html-validation-link nil)

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(setq my-header-file "~/www/header.tpl")
(setq my-footer-file "~/www/footer.tpl")

(setq org-html-preamble (get-string-from-file my-header-file))
(setq org-html-postamble (get-string-from-file my-footer-file))

;; Refer to: http://orgmode.org/manual/Publishing.html
(require 'ox-publish)
(setq org-publish-project-alist
      `(
        ("org-notes-html"          ; Used to export .org file
         :base-directory "~/www/"  ; directory holds .org files
         :base-extension "org"     ; process .org file only
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap nil         ; NOTE: set :auto-sitemap to t would lead to publish very slow if there are many org files
         :sitemap-filename "sitemap0-auto.org"
         :sitemap-title "Archives"
         :sitemap-file-entry-format "%t (%d)"     ; %t: title, %d: date
         :sitemap-sort-files anti-chronologically ; newer date first
         :table-of-contents t
         :style-include-default nil ; Disable the default css style
         )

        ("org-static-html"          ; Used to publish static files
         :base-directory "~/www/"
         :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|ico\\|svg"
         :publishing-directory "~/public_html/"
         :recursive t
         :exclude "blog/images/.*\\|attachments/.*"
         :publishing-function org-publish-attachment
         )

        ("org-static-blog-img"      ; Used to publish static files
         :base-directory "~/www/blog/images/"
         :base-extension "png\\|jpg\\|gif\\|ico\\|svg"
         :publishing-directory "~/public_html/blog/images/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org-static-others"        ; Used to publish static files
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
        ("html-static" :components ("org-notes-html" "org-static-html"))))

;; export all html related files
(defun my-export-html (&optional force)
  (org-publish-project "html" force))

;; same as my-export-html, but more faster
(defun my-export-html-fast (&optional force)
  (progn
    (org-publish-project "html-static" force)
    (message-no-newline
     (shell-command-to-string "rsync -a --delete --exclude '*.pdf' --exclude '*.tex' --exclude '*.sh' --out-format='updating %f' ~/www/blog/images/ ~/public_html/blog/images/"))
    (message-no-newline
     (shell-command-to-string "rsync -a --delete --out-format='updating %f' ~/www/attachments/ ~/public_html/attachments/"))))

(defun my-export-pdf (&optional force)
  (org-publish-project "pdf" force))

(defun message-no-newline (msg)
  "This function is similar to function `message', but without append newline."
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

;;;; 通过设置\\setmainfont[Scale=1.04, Mapping=]{Times New Roman}可以禁止Ligatures，不再需要下面代码
;; ;; 使用org-export-smart-quotes-alist中的设置对英文双引号和英文单引号进行转换
;; (setq org-export-with-smart-quotes t)
;; ;; 原样地输出英文双引号和英文单引号
;; (add-to-list 'org-export-smart-quotes-alist
;;              '("en"
;;                (primary-opening   :latex "{\\ttfamily\"}")   ; {\ttfamily"}
;;                (primary-closing   :latex "{\\ttfamily\"}")
;;                (secondary-opening :latex "{\\ttfamily'}")
;;                (secondary-closing :latex "{\\ttfamily'}")
;;                (apostrophe        :latex "{\\ttfamily'}")))

;; Replace verbatim env by lstlisting env for example block
;; 导出latex时，默认 #+BEGIN_EXAMPLE...#+END_EXAMPLE 会导出为 \begin{verbatim}...\end{verbatim}
;; 这里把它换为 \begin{lstlisting}...\end{lstlisting}
;; 此外，还做下面处理：
;; 1、如果 #+BEGIN_EXAMPLE...#+END_EXAMPLE 中是emacs table，则对其进行居中显示，
;; 2、当表格很宽时使用较小的字体。
;; 下面是emacs table的例子：
;; #+BEGIN_EXAMPLE
;; +-----------+------------+---------------------------+
;; | XPath     | Results    | Note                      |
;; +-----------+------------+---------------------------+
;; | //@id     | id="b1"    | Select all attributes id  |
;; |           | id="b2"    |                           |
;; +-----------+------------+---------------------------+
;; #+END_EXAMPLE
(defun my-latex-export-example-blocks (text backend info)
  "Export example blocks as lstlisting env."
  (when (org-export-derived-backend-p backend 'latex)
    (with-temp-buffer
      (insert text)
      ;; (princ text)      ; just for debugging
      (goto-char (point-min))
      (let* ((second-line (nth 1 (split-string text "[\n\r]+")))
             (third-line (nth 2 (split-string text "[\n\r]+")))
             ;; first line is \\begin{verbatim}, treat it emacs table if
             ;; second line is "+--------- ...... ---------+"
             ;;  third line is "|          ......          |"
             (is-emacs-tbl
              (and (and (string-prefix-p "+--" second-line) (string-suffix-p "--+" second-line))
                   (and (string-prefix-p "|" third-line) (string-suffix-p "|" third-line))))
             (tbl-width (length second-line))
             (full-line-size 90)                      ; 假设一行占90个字符（多于90个字符就不用居中）
             (tbl-leftmargin (max 0 (- 0.5 (/ tbl-width (* full-line-size 2.0)))))  ; (1 - width/90.0)/2
             (tbl-font-scriptsize-threshold 100)      ; 表格宽度大于 100 时使用字体 \\scriptsize
             (tbl-font-tiny-threshold 120)            ; 表格宽度大于 120 时使用字体 \\tiny
             (tbl-fontsize "\\footnotesize"))         ; 默认字体 \\footnotesize
        (if is-emacs-tbl
            (progn
              (cond ((> tbl-width tbl-font-tiny-threshold) (setq tbl-fontsize "\\tiny"))
                    ((> tbl-width tbl-font-scriptsize-threshold) (setq tbl-fontsize "\\scriptsize")))
              ;; (princ (format "tbl-fontsize=%s\n" tbl-fontsize))   ; just for debugging
              ;; 有一些使表格居中的办法（如“\begin{center} \begin{tabular}{c} \begin{lstlisting}”），但当表格很多行时，无法跨页
              ;; 这里使用设置 xleftmargin 的方式让表格“居中”显示
              (replace-string "\\begin{verbatim}"
                              ;; numbers=none 不显示行号
                              ;; frame=none 不显示边框
                              ;; breaklines=false 不换行
                              (format "\\begin{lstlisting}[style=myverbatimstyle,basicstyle=\\ttfamily%s,numbers=none,frame=none,breaklines=false,xleftmargin=%f\\textwidth]"
                                      tbl-fontsize
                                      tbl-leftmargin))
              (replace-string "\\end{verbatim}"
                              "\\end{lstlisting}"))
          (progn
            (replace-string "\\begin{verbatim}" "\\begin{lstlisting}[style=myverbatimstyle]")
            (replace-string "\\end{verbatim}" "\\end{lstlisting}"))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(add-to-list 'org-export-filter-example-block-functions
         'my-latex-export-example-blocks)

;; 导出pdf时，避免中文标点符号出现在“行首”。
;; xeCJK能避免中文标点符号出现在“行首”的情况，不过，如果“中文标点前面有空格”时它无法避免标点符号出现在行首。
;; 情况一：
;; org文件中的：$a^2 + b^2 = c^2$ 。   <-- 句号前有个空格！句号前的空格是必须的，否则无法正确导出数学公式。
;; 会导出为tex：\(a^2 + b^2 = c^2\) 。   <-- 句号前有个空格！这时，xeCJK不能保证句号不出现在行首。
;; 情况二：
;; org文件中的：~code~ 。   <-- 句号前有个空格！句号前的空格是必须的，否则无法正确导出“代码”。
;; 会导出为tex：\texttt{code} 。   <-- 句号前有个空格！这时，xeCJK不能保证句号不出现在行首。
;; 下面函数把上面两种情况中标点之前的空格去掉，xetex不会报错。
;; 如（句号前有个空格）：\(a^2 + b^2 = c^2\) 。
;; 会变为（删除了句号前的空格）：\(a^2 + b^2 = c^2\)。
(defun my-latex-export-avoid-punctuation-in-beginning (text backend info)
  "Avoid punctuation displayed in beginning of line."
  (when (org-export-derived-backend-p backend 'latex)
    (with-temp-buffer
      (insert text)
      ;; (princ text)      ; just for debugging
      (progn
        (goto-char (point-min))
        (replace-string "\) ，" "\)，")  ; 去掉了中文标点前面的空格，下同
        (goto-char (point-min))
        (replace-string "\) 。" "\)。")
        (goto-char (point-min))
        (replace-string "\) 、" "\)、")
        (goto-char (point-min))
        (replace-string "\) ？" "\)？")
        (goto-char (point-min))
        (replace-string "\) ！" "\)！")
        (goto-char (point-min))
        (replace-string "\) ”" "\)”")
        (goto-char (point-min))
        (replace-string "\) ）" "\)）")
        (goto-char (point-min))
        (replace-string "} ，" "}，")
        (goto-char (point-min))
        (replace-string "} 。" "}。")
        (goto-char (point-min))
        (replace-string "} 、" "}、")
        (goto-char (point-min))
        (replace-string "} ？" "}？")
        (goto-char (point-min))
        (replace-string "} ！" "}！")
        (goto-char (point-min))
        (replace-string "} ”" "}”")
        (goto-char (point-min))
        (replace-string "} ）" "}）"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(add-to-list 'org-export-filter-body-functions
             'my-latex-export-avoid-punctuation-in-beginning)

;; 自定义新的LaTex导出模板
;; https://github.com/w0mTea/An.Emacs.Tutorial.for.Vim.User/blob/master/An.Emacs.Tutorial.for.Vim.User.zh-CN.org
;; Add a template for article
(add-to-list 'org-latex-classes
             '("my-org-article-zh"
               "\\documentclass{article}
\\setcounter{secnumdepth}{4}   % 设置对4级目录进行编号（默认只对三级目录进行编号）

\\usepackage{geometry}
\\geometry{left=3.0cm,right=2.5cm,top=2.5cm,bottom=2.5cm}   % 调整页边距

\\usepackage[AutoFallBack]{xeCJK}            % 设置AutoFallBack后，可通过\\setCJKfallbackfamilyfont设置中文备用字体
% \\usepackage[AutoFallBack, PunctStyle=kaiming]{xeCJK}      % 设置标点为“开明”格式（即句末点号用全角，其他半角）

[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\setmainfont[Scale=1.04, Mapping=]{Times New Roman}
% Times New Roman比Source Han Serif SC字形小很多，中英文混合时难看，这里把Times New Roman放大些（Scale=1.04）。
% “Mapping=”的作用是把Mapping设置为空，这样就可以禁止Ligatures了。
% Ligatures=TeX（或Mapping=tex-text）的作用是对字符做转换，比如：
%  ``  -->  “
%  ''  -->  ”
%  --  -->  en-dash
%  --- -->  em-dash
% 注1：在TeX Live 2014及以后版本中，Ligatures=TeX是默认设置，不想使用它就需要明确禁止。
% 注2：使用设置\\defaultfontfeatures{Mapping=tex-text}也可实现Ligatures=TeX的功能，但不建议使用它，因为它还会改变“等宽字体”的设置
\\setsansfont[Mapping=]{Arial}
\\setmonofont{Source Code Pro}
% 上面三种字体设置的说明如下：
% \\setmainfont{XXX}    % 英文衬线字体，\\rmfamily或者\\textrm{}所使用字体（rm表示romain）
% \\setsansfont{XXX}    % 英文无衬线字体，\\sffamily或者\\textsf{}所使用字体
% \\setmonofont{XXX}    % 英文等宽字体，\\ttfamily或者\\texttt{}所使用字体（tt表示typewriter）

\\setCJKmainfont{Source Han Serif SC}
% 思源宋体，可从 https://github.com/adobe-fonts/source-han-serif 下载
% 尝试了其它字体，都不完美。如“SimSun”没有粗体和斜体，“FandolSong”（TeXLive自带，xeCJK默认使用它）缺了很多字形（如“啰嗦”中的“啰”字就没有字形）。

% \\setCJKsansfont{XXX}                % 一般不会用到
\\setCJKmonofont{Source Han Serif SC}

\\xeCJKsetup{
% 下面设置CJKecglue后，发现一个奇怪的问题：汉字中如果有单词MINUS会报错，如文件中含“DB2可以使用EXCEPT或MINUS关键字”时无法正确生成pdf。
% 所以暂时注释掉对CJKecglue的设置。
%  CJKecglue  = \\hskip 0.2em plus 0.08\\baselineskip,   % 设置自动增加的中英文之间的间隔的宽度（默认值太宽）
  xCJKecglue = true}                                    % 让上面的设置对人为输入的中英文之间的空格也有效

\\normalspacedchars{•‘’-—–/`ˇ}    % 这里出现的字符两端不会自动添加空格。比如，It’s中的’后面就不会有过宽的空格了。
\\xeCJKsetwidth{“”（）}{0.7em}    % 把这些标点的宽度设置得窄些（默认占一个中文宽度）。

% 为了避免找不到字体（pdf中会以“豆腐块”形状字符代替），把备用字体\\myfallbackfont设置为“Arial Unicode MS”，
% 它支持的字符很全，且在Mac和Windows平台都内置。
% 如果你认为“Arial Unicode MS”支持的字体还不够，可以使用Code2000字体，或者Google的Noto字体，不过它们都要单独安装。
% 参考：https://en.wikipedia.org/wiki/Unicode_font
% https://tex.stackexchange.com/questions/224584/define-fallback-font-for-specific-unicode-characters-in-lualatex
\\usepackage{fontspec, newunicodechar}
\\newfontfamily{\\myfallbackfont}{Arial Unicode MS}
\\DeclareTextFontCommand{\\textfallback}{\\myfallbackfont}
% 目前没有自动化，需要先找到无法显示的字符，再通过\\newunicodechar重新换一个字体。如下所示：
\\newunicodechar{☆}{\\textfallback{☆}}
\\newunicodechar{❑}{\\textfallback{❑}}
\\newunicodechar{⇔}{\\textfallback{⇔}}
\\newunicodechar{⇒}{\\textfallback{⇒}}
\\newunicodechar{↖}{\\textfallback{↖}}
\\newunicodechar{↗}{\\textfallback{↗}}
\\newunicodechar{↘}{\\textfallback{↘}}
\\newunicodechar{↙}{\\textfallback{↙}}
\\newunicodechar{①}{\\textfallback{①}}
\\newunicodechar{②}{\\textfallback{②}}
\\newunicodechar{③}{\\textfallback{③}}
\\newunicodechar{④}{\\textfallback{④}}
\\newunicodechar{⑤}{\\textfallback{⑤}}
\\newunicodechar{Ⅰ}{\\textfallback{Ⅰ}}   % ROMAN NUMERAL ONE
\\newunicodechar{Ⅱ}{\\textfallback{Ⅱ}}   % ROMAN NUMERAL TWO
\\newunicodechar{Ⅲ}{\\textfallback{Ⅲ}}   % ROMAN NUMERAL THREE
\\newunicodechar{Ⅳ}{\\textfallback{Ⅳ}}   % ROMAN NUMERAL FOUR
\\newunicodechar{Ⅴ}{\\textfallback{Ⅴ}}   % ROMAN NUMERAL FIV
\\newunicodechar{⌘}{\\textfallback{⌘}}   % Mac Command key
\\newunicodechar{⇧}{\\textfallback{⇧}}   % Mac Shift key
\\newunicodechar{⌥}{\\textfallback{⌥}}   % Mac Option key
\\newunicodechar{⌃}{\\textfallback{⌃}}   % Mac Control key
\\newunicodechar{⇪}{\\textfallback{⇪}}   % Mac Caps Lock key
% 如果还有其它字符显示为“豆腐块”，你可以在这里增加相应条目，设置它使用“Arial Unicode MS”字体。
% 注：尝试过ucharclasses包中的\\setTransitionsForXXXX方法来避免“豆腐块”，但这种方法当无法显示的字符左右没有空格，直接夹中文之间时还是会无法显示。

% \\parindent 2em
% \\usepackage{indentfirst}     % 首段缩进
\\usepackage{parskip}           % 不要段缩进

% https://tex.stackexchange.com/questions/7735/how-to-get-straight-quotation-marks
% 引入下面包后，英文双引号会原样地直接输出。不过它不会让英文单引号原样输出。但它有副作用，代码中的单引号、反引号不会原样输出
% \\usepackage[T1]{fontenc}

% 中文断行
\\XeTeXlinebreaklocale \"zh\"   % allow linebreaks
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

\\usepackage{amsmath}       % 这个宏包提供了很多扩展数学公式的环境，如cases
\\usepackage{amssymb}       % 引入这个宏包后，可以使用 \mathbb{R} 等

% 使用 titlesec 包来设置第4级标题（\paragraph）的格式，默认\paragraph之后没有换行（不好看），这里增加换行
% 参考 https://tex.stackexchange.com/questions/60209/how-to-add-an-extra-level-of-sections-with-headings-below-subsubsection/60212
\\usepackage{titlesec}
\\titleformat{\\paragraph}{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}
\\titlespacing*{\\paragraph}{0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}

\\usepackage{caption}

\\usepackage[hyphens]{url}  % 设置长url中的断行可以发生在“-”符号处
                            % 参考 https://tex.stackexchange.com/questions/49788/hyperref-url-long-url-with-dashes-wont-break

\\usepackage[colorlinks,    % 将超链接以颜色来标识，而并非使用默认的方框来标识
             linkcolor=black,
             anchorcolor=black,
             citecolor=black,
             urlcolor=black
             ]{hyperref}

\\urlstyle{rm}              % 对url使用\\rmfamily字体

% 参考 https://tex.stackexchange.com/questions/117267/strange-bug-with-hyperref-percent-symbol-and-cyrillic-character
\\usepackage{bookmark}      % 包hyperref可以生成toc，不过包bookmark功能更强。它可以处理标题中含有“%”的情况

%% org中的 _underlined_ （下划线）和 +strike-through+ （删除线）会导出为 \\uline{underlined} 和 \\sout{strike-through}
\\usepackage[normalem]{ulem} % 包ulem中有 \\uline{} 和 \\sout{}

\\usepackage{graphicx}

\\usepackage{svg}
%% org-mode中插入svg图片时，需要上面的包。
%% 不过由于\pdffilemoddate没有在XeTex中实现等原因，无法正常用xelatex导出svg图片。
%% 参考：http://tex.stackexchange.com/questions/84837/svg-from-inkscape-doesnt-work-in-xelatex）

\\usepackage{animate}        % 可以用它“支持”gif动画

\\usepackage{tabularx}       % 定制表格时可能会用到，如 #+ATTR_LaTeX: :environment tabularx :width \textwidth :align l|l|X
\\usepackage{longtable}      % 能跨页显示的表格。如果表格有很多行，一页放在下，可以使用它。

\\usepackage{booktabs}       % 定制org-latex-tables-booktabs时，需要这个包。

\\usepackage{listings}       % listings能方便处理程序源码

% listings包相关设置
\\lstset{
%language=C,
%basicstyle=\\ttfamily,
basicstyle=\\ttfamily\\footnotesize,   % 把字体设置得更小
%columns=fixed,
%numbers=left,          % where to put the line-numbers
%numberstyle=\\tiny,
showstringspaces=false,
breaklines=true,        % sets automatic line breaking
postbreak=\\mbox{$\\hookrightarrow$\\space},      % 在break的下一行显示一个箭头
frame=tb                % 在top/bottom位置显示边框（横线）
}

\\lstdefinelanguage{myverbatim}{       % 定义新语言，什么都不设置。作为模拟verbatim环境的语言
  identifierstyle=                     % plain identifiers for verbatim
}

\\lstdefinestyle{myverbatimstyle} {
language=myverbatim,
basicstyle=\\ttfamily\\footnotesize,   % 把字体设置得更小
keepspaces=true,
showspaces=false,
showstringspaces=false,
breaklines=true,
postbreak=\\mbox{$\\hookrightarrow$\\space},     % 在break的下一行显示一个箭头
% 设置literate，让换行可以发生在“数字”、“字母”、“-”位置，这样很长的数字或字母或dash也可以换行了
% https://tex.stackexchange.com/questions/153104/automatic-line-breaks-of-long-numbers-in-listings
literate={0}{0}{1}{1}{1}{1}{2}{2}{1}{3}{3}{1}{4}{4}{1}{5}{5}{1}{6}{6}{1}{7}{7}{1}{8}{8}{1}{9}{9}{1}%
{a}{a}{1}{b}{b}{1}{c}{c}{1}{d}{d}{1}{e}{e}{1}{f}{f}{1}{g}{g}{1}{h}{h}{1}{i}{i}{1}{j}{j}{1}{k}{k}{1}%
{l}{l}{1}{m}{m}{1}{n}{n}{1}{o}{o}{1}{p}{p}{1}{q}{q}{1}{r}{r}{1}{s}{s}{1}{t}{t}{1}{u}{u}{1}{v}{v}{1}%
{w}{w}{1}{x}{x}{1}{y}{y}{1}{z}{z}{1}%
{A}{A}{1}{B}{B}{1}{C}{C}{1}{D}{D}{1}{E}{E}{1}{F}{F}{1}{G}{G}{1}{H}{H}{1}{I}{I}{1}{J}{J}{1}{K}{K}{1}%
{L}{L}{1}{M}{M}{1}{N}{N}{1}{O}{O}{1}{P}{P}{1}{Q}{Q}{1}{R}{R}{1}{S}{S}{1}{T}{T}{1}{U}{U}{1}{V}{V}{1}%
{W}{W}{1}{X}{X}{1}{Y}{Y}{1}{Z}{Z}{1}%
{-}{-}{1},
numbers=none,
frame=tb                               % 在top/bottom位置显示边框（横线）
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
     ;; In case of C-drag-n-drop
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
     ;; In case of drag-n-drop
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_HTML: :width 300px\n")
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
       ;; In case of C-drag-n-drop
       ((and  (eq 'C-drag-n-drop (car event))
              (string-match img-regexp fname))
        (insert "#+ATTR_HTML: :width 500px\n")
        (insert (concat  "#+CAPTION: " (file-name-base fname-unix) "\n"))
        (insert (concat  "#+NAME: fig:" (file-name-base fname-unix) "\n"))
        (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname-unix))))
        (if (file-exists-p target-dir)
            (copy-file fname-unix target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir)))
       ;; In case of drag-n-drop
       ((and  (eq 'drag-n-drop (car event))
              (string-match img-regexp fname))
        (insert "#+ATTR_HTML: :width 300px\n")
        (insert (concat  "#+CAPTION: " (file-name-base fname-unix) "\n"))
        (insert (concat  "#+NAME: fig:" (file-name-base fname-unix) "\n"))
        (insert (format "[[%s]]\n" (concat "./images/" (file-name-nondirectory fname-unix))))
        (if (file-exists-p target-dir)
            (copy-file fname-unix target-dir)
          (message "%s does not exist, cannot copy image into it." target-dir)))
       ;; regular drag and drop on file
       (t
        (insert (format "[[%s]]\n" fname)))))))

(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
