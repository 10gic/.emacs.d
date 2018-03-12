;;; init.el --- GNU Emacs/Aquamacs configuration file.

(unless (version< "25.1" emacs-version)
  (error "This init.el requires emacs version 25.1 or later"))

(setq gc-cons-threshold 100000000) ; 调大gc阈值，可显著加快启动速度

(setq inhibit-startup-message t)  ; 启动emacs时不显示GNU Emacs窗口。
(setq initial-scratch-message "") ; scratch信息中显示为空。

(if (not (boundp 'aquamacs-version))
    ;; 如果不是Aquamacs，就关闭tool-bar（Aquamacs的tool-bar比较好看，保留着）
    (tool-bar-mode -1))

;; (tool-bar-mode -1) ; Note: (tool-bar-mode nil) cannot work in Ubuntu 14.04
;; (menu-bar-mode -1) ; Note: (memu-bar-mode nil) cannot work in Ubuntu 14.04

(setq kill-whole-line t) ; 在行首C-k时，同时删除换行符。

(defalias 'yes-or-no-p 'y-or-n-p) ; 设置y/n可代替yes/no

(run-with-idle-timer
 1                                      ; after idle 1 second
 nil                                    ; no repeat, runs just once
 (lambda ()
   ;; 启用ffap-bindings比较耗时，放在空闲时间加载
   (ffap-bindings)))

(which-function-mode 1) ; displays the current function name in the mode line
(setq which-func-unknown "n/a") ; Change ??? to n/a

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'ido)
(ido-mode t) ; 启动ido-mode。如：键入C-x b时，可用ido快速地切换buffer

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; This is your old M-x.

;; 打开文件时回到上次打开文件的位置
(save-place-mode +1)                    ; save-place-mode在Emacs 25.1中引入

;; Winner Mode is a global minor mode. When activated, it allows you to “undo”
;; (and “redo”) changes in the window configuration with the key commands
;; ‘C-c left’ and ‘C-c right’.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Use "Dired Extrs" instead of Dired.
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))
(setq dired-listing-switches "-lhFG") ; ls options in dired

;; Recentf is a minor mode that builds a list of recently opened files.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-c C-f") 'recentf-open-files)

;; set comment char to '#' in assembly mode (GNU Assembler style)
(setq asm-comment-char 35)

;; Overrride the default function, put related file into directory
;; ~/.emacs.d/cache
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; 用flyspell，必须安装aspell-en(sudo apt-get install aspell-en)，
;; 否则提示错误No word lists can be found for the language “en_US”
;; 在text-mode下打开flyspell，使用M-$更正错误
;; (add-hook 'text-mode-hook 'flyspell-mode)
(setq ispell-personal-dictionary "~/.emacs.d/ispell-personal-dict.txt")

;; wgrep（Writable grep）可直接在grep mode中编辑找到的结果，改动可保存到原文件中
;; 比如，我们在10个文件中找到了AAA，想把它们都改为BBB，直接在grep mode中修改即可
;; 修改完后按 `C-x C-s` 可保存，再执行wgrep-save-all-buffers可把改动保存到原文件
;; https://github.com/mhayashi1120/Emacs-wgrep
;; Some tips:
;; 在grep mode中按 `g` (它来自compilation-mode中的recompile)可重新执行一次相同查
;; 找以确认改动生效；如果要修改grep（比如增加参数）再执行，可以按`C-u g`。
(require 'wgrep)

;; 把启动wgrep的快捷键变为字母e
;; 默认，在grep mode中输入 `C-c C-p` 可启动wgrep
(setq wgrep-enable-key "e")

;; 自动保存文件，相当于自动执行M-x wgrep-save-all-buffers
(setq wgrep-auto-save-buffer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t) ; 让光标闪烁。若不设置，在deamon方式下，以图形方式启动时光标不闪烁
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso))) ; adwaita/misterioso, etc
 '(display-time-24hr-format t) ; 可避免因显示中文“上午”或“下午”而导致mode line跨行显示
 '(display-time-mode t)
 '(git-gutter:update-interval 2) ; https://github.com/syohex/emacs-git-gutter
 '(git-gutter:lighter "") ; mode-line中不显示GitGutter字样
 '(mouse-wheel-mode t)
 '(ns-tool-bar-size-mode (quote small) t) ; 设置Aquamacs工具栏使用小图标
 '(scroll-bar-mode (quote nil))
 '(show-paren-mode t)
 '(size-indication-mode t) ; 在Mode line中显示当前Buffer的大小
 '(xterm-mouse-mode t))

;; 在Aquamcs使用“深色”主题（如misterioso）会有问题，下面是一个workaround
;; 参考：;; https://emacs.stackexchange.com/questions/26957/how-can-i-load-the-solarized-dark-theme-correctly-in-aquamacs-from-emacs
(if (and window-system (featurep 'aquamacs))
    (setq default-frame-alist nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-linum-hl ((t (:inherit linum
                             :background "controlHighlightColor"
                             :foreground "alternateSelectedControlColor"))))
 '(which-func ((t (:foreground nil))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "pink1")))))

;; 说明：
;; ;; 把face my-linum-hl设置为misterioso主题下的区分度高的颜色搭配，
;; ;; face my-linum-hl是后面定义的face，用于定制当前行号的样式。
;; ;; 在misterioso主题下，which-func前景色是蓝色，不好区分，下面设置取消其前景色
;; '(which-func ((t (:foreground nil))))
;; ;; 下面设置让注释看起来颜色暗些。
;; '(font-lock-comment-face ((t (:foreground "dim gray"))))
;; ;; org-mode中level-4的face继承的是font-lock-comment-face，太暗，调亮一些
;; '(org-level-4 ((t (:inherit outline-4 :foreground "pink1")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置自动备份
(setq
 version-control t ; 启用版本控制，即可以备份多次。
 backup-by-copying t ; 用复制（而不是mv）的方式备份文件。这可维持文件的硬链接。
 backup-directory-alist '(("." . "~/.emacs.backups")) ; 设置自动备份目录。
 delete-old-versions t ; 自动删除旧的备份。
 kept-new-versions 3 ; 保留最近的3个备份。
 kept-old-versions 2) ; 保留最早的2个备份，即第1次编辑前的文件和第2次编辑前的文件。

;; add "~/bin", "~/go/bin" to PATH and exec-path
(when (file-exists-p "~/bin")
  (setenv "PATH" (concat (getenv "PATH") ":~/bin"))
  (setq exec-path (append exec-path '("~/bin"))))
(when (file-exists-p "~/go/bin")
  (setenv "PATH" (concat (getenv "PATH") ":~/go/bin"))
  (setq exec-path (append exec-path '("~/go/bin"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-local相关变量，用setq-default设置
(setq-default indent-tabs-mode nil) ; 只使用空格进行缩进，不使用tab键。
(setq-default cursor-type 'bar) ; 在X窗口下，光标将变成一根竖线，而不是方块。
(setq-default require-final-newline t) ; Always end a file with a newline.
(setq-default fill-column 80) ; Change fill-column (default is 70) to 80.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 空格和tabs键显示设置
;; 显示尾部的空格。用whitespace-mode也可以显示尾部空格，但这个更方便
(setq-default show-trailing-whitespace t)

;; 在下面这些mode中不显示trailing whiterspace
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'Info-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; Draw tabs with the same color as trailing whitespace in prog-mode
;; From http://emacswiki.org/emacs/ShowWhiteSpace
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (derived-mode-p 'go-mode)    ; prog-mode（除go-mode外）中特别显示"\t"
              (font-lock-add-keywords
               nil
               '(("\t" 0 'trailing-whitespace prepend))))))

;; (global-whitespace-mode t) ; 全局打开whitespace-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 常规键绑定设置
;; 格式化当前行后缩进下行
(global-set-key "\C-m" 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-<return>") 'newline)

;; (global-set-key (kbd "<f8>") 'xterm-mouse-mode)
;; (global-set-key (kbd "<f9>") 'view-mode)
;; (global-set-key [C-f10] 'menu-bar-mode)

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase) ; 放大字体
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease) ; 缩小字体

(global-set-key (kbd "C-x C-b") 'ibuffer) ; 用ibuffer替换BuferMenu

;; 由于C-@按键输入太麻烦，设置C-2为设置标记
(global-set-key (kbd "C-2") 'set-mark-command)

;; kill current buffer without confirmation unless the buffer has been modified.
(global-set-key [(control x) (k)] 'kill-this-buffer)

(global-set-key [M-up] 'windmove-up) ; 移动到上一个窗口
(global-set-key [M-down] 'windmove-down) ; 移动到下一个窗口
(global-set-key [M-left] 'windmove-left) ; 移动到左一个窗口
(global-set-key [M-right] 'windmove-right) ; 移动到右一个窗口

(global-set-key (kbd "M-g") 'goto-line) ; 设置M-g为goto-line

(global-set-key (kbd "M-n") 'next-error) ; go to next error (or next match in grep mode)
(global-set-key (kbd "M-p") 'previous-error) ; go to previous error (or next match in grep mode)

;; 绑定M-/到hippie-expand，它比dabbrev-expand更强大
(global-set-key [(meta ?/)] 'hippie-expand)
;; 变量hippie-expand-try-functions-list控制hippie-expand的补全信息来源及顺序，但
;; 其默认值不够智能。修改hippie-expand-try-functions-list调整补全信息来源的顺序。
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 下面是字体相关设置
(defconst font-size-pair-list-unix
  '((6    8)
    (8   10)
    (10  12)
    (12  14)
    (14  16)
    (15  18)
    (16  20)
    (18  22)
    (20  24)
    (22  26)
    (24  28)
    (25  30)
    (26  32)
    (28  34)
    (30  36)
    (32  38)
    (34  40)
    (35  42)
    (36  44)
    (38  46)
    (40  48)
    (42  50)
    (44  52)
    (45  54)
    (46  56)
    (48  58))
  "一个列表，元素为“字体大小对”：(英文字号 中文字号)，Mac/Linux(Redhat)下测试可实现2个英文为1个中文等宽.")

(defconst font-size-pair-list-ms
  '((8    8)
    (10  10)
    (11  12)
    (12  14)
    (14  16)
    (16  18)
    (18  20)
    (20  22)
    (22  24)
    (24  26)
    (26  28)
    (28  30)
    (30  32)
    (31  34)
    (32  36)
    (34  38)
    (36  40)
    (38  42)
    (40  44)
    (42  46)
    (44  48)
    (46  50)
    (48  52)
    (50  54)
    (51  56)
    (52  58))
  "一个列表，元素为“字体大小对”：(英文字号 中文字号)，Windows 8.1下测试可实现2个英文为1个中文等宽.")

(defun get-mix-mono-font-size-pair ()
  (if (or (string-equal system-type "cygwin")
          (string-equal system-type "windows-nt"))
      font-size-pair-list-ms   ; MS Windows
    font-size-pair-list-unix)) ; Linux, Mac

(defun get-font-size-for-face-default ()
  "Get the font size of face default."
  (aref
   ;; query-font返回字体信息相关数组，数组第3个元素为字体大小
   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
   (query-font (face-attribute 'default :font)) 2))

(defun my-increase-font-size ()
  "增加字体大小，同时确保1个中文和两个英文等宽"
  (interactive)
  (let ((size-pair (seq-find            ; 取第一个“大于”目前字体的“字体大小对”
                    (lambda (x) (> (car x) (get-font-size-for-face-default)))
                    (get-mix-mono-font-size-pair))))
    (if size-pair
        (progn
          (message "Set English/Chinese font size to %s/%s."
                   (nth 0 size-pair)
                   (nth 1 size-pair))
          (my-set-font-size (nth 0 size-pair) (nth 1 size-pair)))
      (message "Already biggest font size, no bigger font size pair is found."))))

(defun my-decrease-font-size ()
  "减小字体大小，同时确保1个中文和两个英文等宽"
  (interactive)
  (let ((size-pair (car (last ; 取结果的最后一个“字体大小对” (car (last list)) -> "last element in list"
                         (seq-take-while ; 只取list中“小于”当前英文字体大小的“字体大小对”
                          (lambda (x) (< (car x) (get-font-size-for-face-default)))
                          (get-mix-mono-font-size-pair))))))
    (if size-pair
        (progn
          (message "Set English/Chinese font size to %s/%s."
                   (nth 0 size-pair)
                   (nth 1 size-pair))
          (my-set-font-size (nth 0 size-pair) (nth 1 size-pair)))
      (message "Already smallest font size, no smaller font size pair is found."))))

(defun my-set-font-size (english-font-size chinese-font-size)
  "Set English and Chinese font size."
  (interactive
   (list
    (read-number "English font size: ")
    (read-number "Chinese font size: ")))
  (when (display-graphic-p)
    (cond
     ;; 设置英文字体，优先使用排在前面的字体
     ((find-font (font-spec :name "Source Code Pro"))
      ;; set-frame-font第2个参数为nil（或者t）表示不维持（维持）窗口大小
      ;; set-frame-font第3个参数为t表示在新frame中也有效
      ;; 写法"Source Code Pro-14"，在RedHat上有问题
      ;; 应该写为"Source Code Pro:pixelsize=14"
      (set-frame-font (format "Source Code Pro:pixelsize=%s" english-font-size) t t))
     ((find-font (font-spec :name "Ubuntu Mono"))
      (set-frame-font (format "Ubuntu Mono:pixelsize=%s" english-font-size) t t))
     ((find-font (font-spec :name "DejaVu Sans Mono"))
      (set-frame-font (format "DejaVu Sans Mono:pixelsize=%s" english-font-size) t t))
     ((find-font (font-spec :name "Consolas"))     ; 微软等宽字体
      (set-frame-font (format "Consolas:pixelsize=%s" english-font-size) t t))
     ((find-font (font-spec :name "Courier New"))  ; Courier New存在于Mac和Windows中
      (set-frame-font (format "Courier New:pixelsize=%s" english-font-size) t t)))
    (cond
     ;; 下面设置中文字体，优先使用排在前面的字体
     ;; 注：cygwin中使用emacs-X11时无法加载Windows默认路径中字体（使用emacs-w32
     ;; 则没有问题），如果你使用emacs-X11，请把相应的中文字体复制到~/.fonts
     ((find-font (font-spec :name "Microsoft Yahei"))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "Microsoft Yahei"
                                     :size chinese-font-size))))
     ((find-font (font-spec :name "STHeiti"))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "STHeiti"  ; Mac中内置STHeiti
                                     :size chinese-font-size))))
     ((find-font (font-spec :name "WenQuanYi Zen Hei Mono"))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "WenQuanYi Zen Hei Mono"
                                     :size chinese-font-size)))))))

;;; frame相关的设置
;; 字体的设置必须在after-make-frame-functions的hook中进行，否则其设置对用
;; emacsclient启动的窗口无效。
(defun my-set-frame()
  ;; 英文为16/15，中文为18，可实现Windows/Unix下1个中文和2个英文等宽，
  ;; 从get-mix-mono-font-size-pair中，可以找到其它中英文等宽的“字体大小对”
  (if (or (string-equal system-type "cygwin")
          (string-equal system-type "windows-nt"))
      (my-set-font-size 16 18)
    (my-set-font-size 15 18))

  ;; (setq frame-title-format "%b") ; %b让标题栏显示buffer的名字。
  (set-cursor-color "red") ; 定制光标颜色，这样在“深色”主题下更醒目
  (setq frame-title-format `(,(user-login-name) "@" ,(system-name) " %f" )))

;; 设置daemon方式和非deamon方式启动时都执行my-set-frame
(if (and (fboundp 'daemonp) (daemonp))
    ;; 以daemon方式启动时，要使与X相关配置生效，应使用这个hook
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my-set-frame))))
  (my-set-frame))

(defun my-occur-symbol-at-point ()
  "Find keyword (using occur) in current buffer."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        (push (regexp-quote sym) regexp-history)) ; regexp-history defvared in replace.el
    (call-interactively 'occur)))

(defun my-find-symbol-at-point-in-current-buffer (search-regex arg)
  "Find keyword (using grep or occur) at point in current buffer."
  (interactive
   (list (read-string
          (format "Find keyword matching regexp %s: "
                  (if (thing-at-point 'symbol)
                      (concat "(default " (thing-at-point 'symbol) ")")
                    ""))
          nil nil (thing-at-point 'symbol))
         current-prefix-arg))
  (let* ((current-file (if buffer-file-name buffer-file-name ""))
         (default-directory (file-name-directory current-file))
         (other-args (if current-prefix-arg
                         (read-string "Other grep options (default '-w'): " nil nil "-w")
                       nil))
         (grep-program (if (string-suffix-p ".gz" current-file) "zgrep" "grep"))
         (grep-cmd
          ;; `i` case insensitive, `n` print line number,
          ;; `I` ignore binary files,
          ;; `H` show file name always,
          ;; `E` extended regular expressions,
          (concat
           grep-program
           " "
           "-inIHE --color=always"
           (if other-args (concat " " other-args) "")
           " -- "
           (shell-quote-argument search-regex)
           " "
           (shell-quote-argument (file-name-nondirectory current-file)))))
    (cond
     ((string= "" search-regex) (message "Your regexp is empty, do nothing."))
     ((string= "" current-file)
      ;; 如果当前buffer不关联文件，则使用occur查找光标下单词
      (call-interactively 'my-occur-symbol-at-point))
     (t (compilation-start grep-cmd 'grep-mode (lambda (mode) "*grep*") nil)))))

(defun my-find-symbol-at-point-in-project (arg)
  "Recursively grep keyword in project root, use ripgrep (more faster than grep) if found."
  (interactive "p")
  (let* ((project-dir (ignore-errors (projectile-project-root)))
         (target-dir (if current-prefix-arg
                         ;; 若提供了前缀参数，则总让用户输入查找目录
                         (read-directory-name "Find keyword in directory: ")
                       (if project-dir
                           ;; 若没有提供前缀参数，且当前文件在工程中，则直接查找工程根目录
                           project-dir
                         ;; 若没有提供前缀参数，且当前文件不在工程中，则提示用户输入查找目录
                         (read-directory-name "Find keyword in directory: "))))
         (search-regex (read-string
                        (format "Keyword regexp %s: "
                                (if (thing-at-point 'symbol)
                                    (concat "(default " (thing-at-point 'symbol) ")")
                                  ""))
                        nil nil (thing-at-point 'symbol)))
         (use-ripgrep-p (executable-find "rg"))
         (other-args-prompt (if use-ripgrep-p "Other ripgrep options (default '-w'): "
                              "Other grep options (default '-w'): "))
         (other-args (if current-prefix-arg
                         ;; 当提供了前缀参数时，提示用户输入“额外的参数”
                         (read-string other-args-prompt nil nil "-w")
                       nil)))
    (let* ((default-directory (file-name-as-directory target-dir))
           (grep-cmd
            ;; `i` case insensitive, `n` print line number,
            ;; `I` ignore binary files, `E` extended regular expressions,
            ;; `r` recursive
            ;; `--exclude-dir`排除目录，较新Linux/Mac的grep支持这个选项
            (concat
             grep-program
             " "
             "-inIEr --color=always --exclude-dir='.*'"
             (if other-args (concat " " other-args) "")
             " -- "
             (shell-quote-argument search-regex)
             " "
             (shell-quote-argument (expand-file-name default-directory))))
           (ripgrep-cmd
            (concat
             "rg"
             " "
             "-in --color=always --no-heading --with-filename"
             (if other-args (concat " " other-args) "")
             " -- "
             (shell-quote-argument search-regex))))
      (cond
       ((string= "" search-regex) (message "Your regex is empty, do nothing."))
       (use-ripgrep-p
        (compilation-start ripgrep-cmd 'grep-mode (lambda (mode) "*grep*") nil))
       (t
        (compilation-start grep-cmd 'grep-mode (lambda (mode) "*grep*") nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun define-mac-hyper-key (key fun)
  (when (eq system-type 'darwin)
      (progn
        (cond
         ((boundp 'aquamacs-version)
          ;; Aquamacs中，Command键为A
          (define-key osx-key-mode-map (kbd (concat "A-" key)) fun))
         (t
          ;; GNU Emacs，Command键为s
          (global-set-key (kbd (concat "s-" key)) fun))))))

(when (eq system-type 'darwin)
  ;; 设置Mac下增加/减小字体大小的快捷键
  (when (display-graphic-p)
    (define-mac-hyper-key "=" 'my-increase-font-size)  ; Command + =
    (define-mac-hyper-key "-" 'my-decrease-font-size)) ; Command + -

  ;; 在当前文件中查找光标下单词
  (define-mac-hyper-key "f" 'my-find-symbol-at-point-in-current-buffer) ; Command + f
  ;; 在当前工程中查找光标下单词，没找到工程就在文件所在目录中查找
  (define-mac-hyper-key "F" 'my-find-symbol-at-point-in-project)) ; Command + Shift + f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings for Aquamacs
;;
;; ~/Library/Preferences/Aquamacs Emacs/
;;
;; Useful links for Aquamacs
;; http://www.emacswiki.org/emacs/AquamacsFAQ
;; http://www.emacswiki.org/emacs/CustomizeAquamacs

(when (boundp 'aquamacs-version)
  ;; open *help* in current frame
  (setq special-display-regexps (remove "[ ]?\\*[hH]elp.*"
                                        special-display-regexps))

  ;; 打开aquamacs-autoface-mode时，Aquamacs默认使用Monaco字体，但Monaco没有粗体
  ;; 这里禁止aquamacs-autoface-mode
  (if (fboundp 'aquamacs-autoface-mode) (aquamacs-autoface-mode -1))

  ;; Aquamacs内置有python mode，但它没有实现imenu的接口，打开py文件会提示：
  ;; imenu-unavailable The mode ‘Py’ does not support Imenu
  ;; Emacs中内置也有python mode，它实现了imenu接口
  ;; 下面在Aquamacs中启用Emacs中内置的python mode
  (require 'python))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppress error "directory ~/.emacs.d/server is unsafe" on emacs-w32 (issue
;; found in windows 8.1).
;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
;; http://www.emacswiki.org/emacs/EmacsW32
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; column-marker有严重的性能问题：打开一个5M左右C程序可能会卡超过1分钟
;; (require 'column-marker)
;; (add-hook 'prog-mode-hook (lambda ()
;;                             (column-marker-1 80)
;;                             (column-marker-2 90)
;;                             (column-marker-3 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 在第81列显示一个标尺，使用`M-x fci-mode`可打开或关闭它
(ignore-errors
  ;; fill-column-indicator需要Emacs 25，否则会显式抛异常，下面方式无法抑制异常
  ;; (require 'fill-column-indicator nil :noerror)
  (require 'fill-column-indicator))
(defun my-toggle-rule-on-column-81 ()
  "Toggle rule on column 81 (using fill-column-indicator)."
  (interactive)
  (call-interactively 'fci-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/syohex/emacs-git-gutter
(require 'git-gutter)

;; Enable global minor mode
(global-git-gutter-mode t)

;; Use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; 设置“跳到上一个（或下一个）修改位置”的快捷键
(global-set-key (kbd "C-M-S-<up>") 'git-gutter:previous-hunk) ; Ctrl + Alt + Shift + ↑
(global-set-key (kbd "C-M-S-<down>") 'git-gutter:next-hunk)   ; Ctrl + Alt + Shift + ↓
;; 设置“取消（Revert）当前位置的修改”的快捷键
(global-set-key (kbd "C-M-S-<left>") 'git-gutter:revert-hunk) ; Ctrl + Alt + Shift + <-
;; 设置在新buffer中显示当前位置修改的快捷键
(global-set-key (kbd "C-M-S-<right>") 'git-gutter:popup-hunk) ; Ctrl + Alt + Shift + ->

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 加载其它包及设置
;; Extract packages into ~/.emacs.d/packages/extract/
(setq my-pkg-path "~/.emacs.d/packages/extract/")

(if (file-exists-p "~/.emacs.d/packages/extract.done")
    (message "File ~/.emacs.d/packages/extract.done exist, skip executing extract.sh.")
  (let* ((script-path (expand-file-name "~/.emacs.d/packages/extract.sh"))
         (output (process-lines "sh" script-path)))
    (dolist (line output)
      (message "%s" line))))

(setq my-org-path1 (car (file-expand-wildcards (concat my-pkg-path "org-*/lisp"))))
(setq my-org-path2 (car (file-expand-wildcards (concat my-pkg-path "org-*/contrib/lisp"))))
(setq my-tabbar-path (concat my-pkg-path "tabbar-master"))
(setq my-multiple-cursors-path (concat my-pkg-path "multiple-cursors.el-master"))
(setq my-flycheck-path (concat my-pkg-path "flycheck-master"))
(setq my-auto-complete-path (car (file-expand-wildcards (concat my-pkg-path "auto-complete-*"))))
(setq my-auto-complete-dict-path (car (file-expand-wildcards (concat my-pkg-path "auto-complete-*/dict"))))
(setq my-all-the-icons-path (car (file-expand-wildcards (concat my-pkg-path "all-the-icons*"))))
(setq my-neotree-path (car (file-expand-wildcards (concat my-pkg-path "emacs-neotree-*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use newer org-mode, the builtin version is too old.
(setq load-path (cons my-org-path1 load-path))
(setq load-path (cons my-org-path2 load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aquamacs-tabbar(from https://github.com/dholm/tabbar)比原始的tabbar更友好
;; Mac中Auqamacs内置有tabbar，且默认配置很好，无需再配置。
;; 下面在非Auqamacs中加载aquamacs-tabbar
(when (not (boundp 'aquamacs-version))
  (add-to-list 'load-path my-tabbar-path)
  (require 'aquamacs-tabbar)
  (tabbar-mode 1)
  (load-file "~/.emacs.d/customize-aquamacs-tabbar.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors for Emacs. https://github.com/magnars/multiple-cursors.el
(add-to-list 'load-path my-multiple-cursors-path)
(require 'multiple-cursors)

(require 'region-bindings-mode)
(region-bindings-mode-enable)
;; setup integrating the multiple-cursors package
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this) ; 选择所有
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this) ; 选择下一个
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this) ; 选择前一个
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "u" 'mc/unmark-next-like-this) ; 取消当前选择
(define-key region-bindings-mode-map "s" 'mc/skip-to-next-like-this) ; 取消当前选择，选择下一个

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load flycheck.
;; It requires:
;; gcc 4.8 or newer.
;; let-alist (comes built-in with emacs 25.1).
;; dash (a modern list api for Emacs).
(add-to-list 'load-path my-flycheck-path)
;; 仅在第一次进入cc-mode时加载flycheck
(with-eval-after-load 'cc-mode (load "flycheck"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配置auto-complete
;; http://cx4a.org/software/auto-complete/
;; http://blog.csdn.net/winterttr/article/details/7524336
(add-to-list 'load-path my-auto-complete-path)
(run-with-idle-timer
 1                                      ; after idle 1 second
 nil                                    ; no repeat, runs just once
 (lambda ()
   ;; 加载auto-complete比较耗时，放在空闲时间加载
   (require 'auto-complete-config)
   (ac-config-default)
   (add-to-list 'ac-dictionary-directories my-auto-complete-dict-path)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile: “工程管理”插件，可快速访问项目里任何文件，快速在项目中搜索关键字
;; 所谓“工程”就是一些文件的集合，默认projectile支持git，mercurial，bazaar等工程；
;; 手动创建一个工程的方法：在工程根目录中创建一个名为.projectile的空文件即可。
;; See https://github.com/bbatsov/projectile
(require 'projectile)
(projectile-mode)

;; 定制mode-line中的显示字符（把Projectile改为Prj，节省空间）
(setq projectile-mode-line '(:eval (format " Prj[%s]" (projectile-project-name))))

;; projectile中默认快捷键前缀为`C-c p`
;; projectile最常用的两个快捷键：
;; `C-c p f`   ：在工程中查找文件
;; `C-c p s g` ：在工程中查找关键字
;;
;; 还有很多其它快捷键绑定，如果记不住，你可以通过下面命令查看这些绑定：
;; `C-c p C-h` ：查看projectile的快捷键绑定

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path my-all-the-icons-path)
;; 注意：使用all-the-icons前需要安装相应字体。
;; 方法一：手动安装其fonts子目录中的字体；
;; 方法二：使用函数all-the-icons-install-fonts在线安装最新的相关字体。
;; https://github.com/domtronn/all-the-icons.el
(require 'all-the-icons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path my-neotree-path)
(require 'neotree)

;; 禁止自动刷新，自动刷新可能使根目录变为文件父目录
(setq neo-autorefresh nil)
(setq neo-vc-integration (quote (face)))
(setq neo-window-fixed-size nil)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow)) ; 依赖于all-the-icons

;; 下面设置禁止neotree-find提示如下信息
;; "File not found in root path, do you want to change root?"
(setq neo-force-change-root t)

(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root.
Using find-file-in-project, or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;; Pick one: projectile or find-file-in-project
           (projectile-project-root)
           ;; (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (save-selected-window
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

(global-set-key (kbd "<f9>") 'neotree-project-dir-toggle)

(add-hook 'neotree-mode-hook
          (lambda()
            (if (boundp 'tabbar-local-mode)  ; 检测tabbar-local-mode是否存在
                (tabbar-local-mode 1))))     ; 在neotree-mode中关闭tabbar

(require 'switch-buffer-functions)
;; 每次切换buffer后都会执行下面hook
(add-hook 'switch-buffer-functions
          (lambda (prev-buffer cur-buffer)
            ;; 若当前buffer名不以*号开始，且neotree处于打开状态，则进一步处理
            (if (and (not (string-prefix-p "*" (buffer-name cur-buffer)))
                     (neo-global--window-exists-p))
                (let ((project-dir
                       (ignore-errors
                         (projectile-project-root)))
                      (file-name (buffer-file-name cur-buffer)))
                  (save-selected-window
                    (if project-dir
                        (neotree-dir project-dir))
                    (if file-name
                        (neotree-find file-name)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(require 'unicad) ; 该插件能自动识别文件编码

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 禁止grep mode中的line wrap，使用M-x toggle-truncate-lines可以再打开line wrap
;; https://www.reddit.com/r/emacs/comments/6zvw4d/grep_buffer_hide_find_command/
(add-hook 'grep-mode-hook (lambda ()
                            (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook
          (lambda ()

            (unless (derived-mode-p 'css-mode) ; CSS mode不支持Imenu
              (imenu-add-menubar-index))       ; 启用Imenu（显示index菜单）
            (linum-mode 1)                     ; 显示行号
            ;; electric-pair-local-mode在Emacs 25.1中引入
            ;; 打开自动输入匹配括号功能
            (if (fboundp 'electric-pair-local-mode)
                (electric-pair-local-mode 1))))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(run-with-idle-timer
 1                                      ; after idle 1 second
 nil                                    ; no repeat, runs just once
 (lambda ()
   ;; 加载highlight-symbol比较耗时，放在空闲时间加载
   (require 'highlight-symbol)
   (highlight-symbol-nav-mode)
   (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
   ;; (add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))

   (setq highlight-symbol-idle-delay 0.3
         highlight-symbol-on-navigation-p t)

   ;; Mac专有设置
   (define-mac-hyper-key "g" 'highlight-symbol-next) ; Command + g, 跳转到下一个相同符号
   (define-mac-hyper-key "G" 'highlight-symbol-prev) ; Command + Shift + g, 跳转到上一个相同符号
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 加载xcscope(Cscope的emacs扩展，依赖于Cscope)
;; debian下可以这样安装xcscope: apt-get install cscope-el
;; Refer to: https://github.com/dkogan/xcscope.el
(require 'xcscope)
(cscope-setup)
;; 默认cscope-setup时只会为c/c++ mode启用cscope-minor-mode
;; 下面为golang/java也启用cscope-minor-mode
(add-hook 'go-mode-hook (function cscope-minor-mode))
(add-hook 'java-mode-hook (function cscope-minor-mode))
;; (setq cscope-display-cscope-buffer nil) ; 不显示*cscope* buffer

(setq cscope-option-use-inverted-index t) ; 使用反向索引，即cscope的-q选项
(add-to-list 'cscope-indexer-suffixes "*.go") ; 增加go后缀，默认仅索引c/c++相关文件
(add-to-list 'cscope-indexer-suffixes "*.java") ; 增加java后缀，默认仅索引c/c++相关文件

(defun generate-cscopes-files-in-current-project ()
  "如果工程的根目录没有cscope.files，则生成该文件（当cscope.files存在时，xcscope会自动建立索引）"
  (let ((project-dir (ignore-errors (projectile-project-root))))
    (if (and project-dir
             (not (file-exists-p (concat (file-name-as-directory project-dir)
                                         "cscope.files"))))
        (cscope-create-list-of-files-to-index project-dir))))

(add-hook 'cscope-minor-mode-hooks
          (lambda ()
            (generate-cscopes-files-in-current-project)))

(defun erase-cscope-buffer (&rest args)
  (ignore-errors
    (save-excursion
      (set-buffer "*cscope*")
      (erase-buffer))))

(defun advice-before-query-cscope ()
  "cscope查询时会把新结果append到buffer *cscope*中，这里通过adivce-add的方式
在每次查询前清空buffer *cscope*"
  (let ((query-functions
         '(cscope-find-this-symbol
           cscope-find-global-definition
           cscope-find-global-definition-no-prompting
           cscope-find-assignments-to-this-symbol
           cscope-find-functions-calling-this-function
           cscope-find-called-functions
           cscope-find-this-text-string
           cscope-find-egrep-pattern
           cscope-find-this-file
           cscope-find-files-including-file)))
    (dolist (func query-functions)
      (advice-add func
                  :before
                  #'erase-cscope-buffer))))

(with-eval-after-load 'xcscope
  (advice-before-query-cscope))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; For C
;; 默认M-x compiler时，make命令会提取下面环境变量。
(setenv "CFLAGS" "-ggdb3 -Wall")
(setenv "CXXFLAGS" "-ggdb3 -Wall")

(setq-default c-basic-offset 4  ; 设置缩进为4个空格
              tab-width 4)      ; 显示tab为4个空格

;; 格式化C/C++程序代码
;; 参考：
;; https://en.wikipedia.org/wiki/Indent_style
;; http://algo13.net/clang/clang-format-style-oputions.html
;; http://clang.llvm.org/docs/ClangFormat.html
(defun my-reformat-c-buffer()
  "Use external tool `clang-format' (if not find, try to use `indent') to
reformat current entire buffer."
  (interactive)
  (when (and (not (executable-find "clang-format"))
             (not (executable-find "indent")))
    (error "Error: cannot find external tool `clang-format' or `indent'"))
  (let (sh-indent-command)
    (if (eq system-type 'darwin)
        ;; Mac OS中自带的indent不支持-kr等选项，比GNU indent功能少很多
        (setq sh-indent-command (concat "indent -nbad -bap -nbc -br -brs "
                                        "-c33 -cd33 -ncdb -ce -ci4 -cli0 "
                                        "-d0 -di1 -nfc1 -i4 -ip0 -l80 -lp "
                                        "-npcs -npsl -nsc -nsob -nut"))
      ;; 假设GNU indent
      (setq sh-indent-command "indent -kr -nut"))
    ;; 如果发现系统中有clang-format，则优先使用它
    (when (executable-find "clang-format")
      (setq sh-indent-command "clang-format -style=LLVM"))
    (save-buffer)
    (shell-command-on-region
     (point-min)
     (point-max)
     sh-indent-command
     (buffer-name)
     nil
     "*Error*"
     t
     )
    (save-buffer)))

;; 当buffer较大时，semantic分析时会非常慢，下面设置一个阈值，超过阈值不会分析
(setq semantic-idle-scheduler-max-buffer-size 1048576) ; 1M

(with-eval-after-load 'cc-mode
  ;; srefactor is a C/C++ refactoring tool based on Semantic parser framework.
  ;; https://github.com/tuhdo/semantic-refactor
  ;; https://github.com/10gic/semantic-refactor
  ;; 说明：(require 'srefactor)比较耗时。
  ;; 为加快启动，把它放到eval-after-load中，这样仅第一次加载cc-mode时会比较慢
  (require 'srefactor)
  (semantic-mode 1) ; this is needed by srefactor
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

;;;; For perl
(defalias 'perl-mode 'cperl-mode) ; 设置默认使用cperl-mode代替perl-mode

(add-hook 'cperl-mode-hook
          (function (lambda ()  ;only work in GUI.
                      (local-set-key [f1] 'cperl-perldoc-at-point))))

;;;; For sql
(add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-mode))

;;;; For java
(add-to-list 'auto-mode-alist '("\\.jj\\'" . java-mode))  ; javacc grammar file
(add-to-list 'auto-mode-alist '("\\.jjt\\'" . java-mode)) ; javacc jjtree file

(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)

(add-hook 'jtags-mode-hook (lambda ()
                             ;; jtags-show-declaration默认绑定到"M-,"
                             ;; 它覆盖了全局的绑定xref-pop-marker-stack
                             ;; 下面取消jtags-mode-map的中"M-,"绑定
                             (define-key jtags-mode-map (kbd "M-,") nil)))

;;;; For shell
(add-to-list 'auto-mode-alist '("setenv" . sh-mode)) ; 以setenv开头的文件使用sh-mode
(add-hook 'sh-mode-hook (function
                         (lambda ()
                           ;; sh-mode中把"C-c C-f"绑定到了sh-for，下面将其取消
                           ;; 注："C-c C-f"已经全局地绑定到了打开recent files
                           (define-key sh-mode-map (kbd "C-c C-f") nil)
                           (setq tab-width 4))))

;;;; For html
;; web-mode.el is an autonomous emacs major-mode for editing web templates.
(autoload 'web-mode "web-mode" "major-mode for editing web templates" t)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

;; Highlight current HTML element (highlight matched tags).
(setq web-mode-enable-current-element-highlight t)
;; Disable auto indentation
(setq web-mode-enable-auto-indentation nil)

;; Set indentation offset
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)

;;;; For makefile
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-mode)) ; 以makefile开头的文件使用makefile mode

;; For CMake
(autoload 'cmake-mode "cmake-mode" "cmake mode" t)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; For log4j (major mode for viewing log files)
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("catalina.out" . log4j-mode)) ; catalina.out is tomcat log file
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;; For nginx (major mode for editing nginx.conf)
(autoload 'nginx-mode "nginx-mode" nil t)
(add-to-list 'auto-mode-alist '("nginx.conf\\'" . nginx-mode))

;; 目前不开发lisp程序，暂时注释掉下面配置以加快启动速度
;; (load-file "~/.emacs.d/customize-lisp.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/progmodes/")

;;;; For go
(require 'go-mode-autoloads)  ; https://github.com/dominikh/go-mode.el
(with-eval-after-load "go-mode"
  ;; 下面可实现go补全，它依赖于gocode（安装方法`go get -u -v github.com/nsf/gocode`）
  (require 'go-autocomplete)

  ;; go-eldoc.el可以在modeline中显示当前光标位置变量或函数的相关信息
  (require 'go-eldoc)

  (if (executable-find "guru")
      (require 'go-guru)))              ; 提示输入scope时，输入点号.即可


;; 保存go文件前先格式化代码
(add-hook 'go-mode-hook
          (lambda ()

            (go-eldoc-setup)            ; for go-eldoc

            ;; (add-hook 'before-save-hook 'gofmt-before-save)

            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go build -v && go test -v && go vet"))

            ;; Key bindings specific to go-mode
            (local-set-key (kbd "M-.") 'godef-jump) ; Go to definition
            (local-set-key [f1] 'godoc-at-point)))

(autoload 'yaml-mode "yaml-mode" "yaml mode" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(autoload 'jcl-mode "jcl-mode" "jcl mode" t)
(add-to-list 'auto-mode-alist '("\\.jcl\\'" . jcl-mode))

(autoload 'refine-mode "refine-mode" "refine mode" t)
(add-to-list 'auto-mode-alist '("\\.re\\'" . refine-mode))

(autoload 'cobol-mode "cobol-mode" "cobol mode" t)
(autoload 'cobol-free-mode "cobol-free-mode" "cobol mode for free format" t)
(setq auto-mode-alist
      (append
       '(
         ("\\.cpy\\'" . cobol-mode)
         ("\\.cbl\\'" . cobol-mode)
         ("\\.cob\\'" . cobol-mode)
         ("\\.pco\\'" . cobol-mode)  ; File name ends in '.pco', Oracle Pro*COBOL files.
         ("\\.sqb\\'" . cobol-mode)) ; File name ends in '.sqb', Db2 files.
       auto-mode-alist))
;; 当关键字IDENTIFICATION前面的空格为0-6(不到7)个时，设置为cobol-free-mode
(add-to-list 'magic-mode-alist '("\\(^.*\n\\)*[ ]\\{0,6\\}IDENTIFICATION" . cobol-free-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 加载customize-org.el耗时比较多，仅在第一次进入org-mode时加载它
;; Using "org", because org-mode is defined in org.el
(with-eval-after-load "org" (load-file "~/.emacs.d/customize-org.el"))

(with-eval-after-load "tex-mode"
  (if (require 'tex-buf nil 'noerror)
      (load-file "~/.emacs.d/customize-latex.el")
    (message "Warn: tex-buf is not available, skip its configuring")))

;; 可通过安装emacs-goodies-el来安装folding
;; http://www.emacswiki.org/emacs/FoldingMode
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; Load htmlize
(require 'htmlize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idea from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(require 'term)
(defun visit-ansi-term (arg)
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one
     4) on a named term (i.e., not *ansi-term*) create a new one
     4) if called with CTRL-u create a new ansi-term regardless
     5) C-u C-u create a new ansi-term and prompt for name
   Within an existing ansi-term one need to use C-x C-u F2 for a new term"
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list nil))
    ((equal current-prefix-arg '(4))
     (list "*ansi-term*"))
    ((equal current-prefix-arg '(16))
     (list (read-string "Name (*ansi-term*):" nil nil "*ansi-term*")))
    ))
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        ;; (term-cmd (if (executable-find "zsh") "/bin/zsh" "/bin/bash"))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (cond
     ((string= arg nil)
      (if is-term
          (if is-running
              (if (string= "*ansi-term*" (buffer-name))
                  (call-interactively 'rename-buffer)
                (if anon-term
                    (switch-to-buffer "*ansi-term*")
                  (ansi-term term-cmd)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd))
        (if anon-term
            (if (term-check-proc "*ansi-term*")
                (switch-to-buffer "*ansi-term*")
              (kill-buffer "*ansi-term*")
              (ansi-term term-cmd))
          (ansi-term term-cmd))))
     ((string= arg "*ansi-term*")
      (ansi-term term-cmd))
     (t
      (ansi-term term-cmd arg)))))

;; Do not bind key "<f2>" when system is windows
(when (not (eq system-type 'windows-nt))
  (global-set-key (kbd "<f2>") 'visit-ansi-term))

;; term 模式下不绑定M-0, M-1等，它们有其它用处。
(with-eval-after-load "term"
  (define-key term-raw-map (kbd "M-0") nil)
  (define-key term-raw-map (kbd "M-1") nil)
  (define-key term-raw-map (kbd "M-2") nil)
  (define-key term-raw-map (kbd "M-3") nil)
  (define-key term-raw-map (kbd "M-4") nil)
  (define-key term-raw-map (kbd "M-5") nil)
  (define-key term-raw-map (kbd "M-6") nil)
  (define-key term-raw-map (kbd "M-7") nil)
  (define-key term-raw-map (kbd "M-8") nil)
  (define-key term-raw-map (kbd "M-9") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autoinsert)
(auto-insert-mode +1)          ; Enable auto-insert-mode
(setq auto-insert-query nil)   ; No prompt before insertion

(with-eval-after-load 'autoinsert
  (define-auto-insert '("\\.pl\\'" . "Perl skeleton")
    '(nil
      "#!/usr/bin/env perl" \n
      \n
      "use strict;" \n
      "use warnings;" \n \n
      _ \n))

  (define-auto-insert
    '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
    '(nil
      "#include <iostream>" \n \n
      "using namespace std;" \n \n
      "int main() {" \n
      _ \n
      "return 0;" \n
      "}" > \n))

  (define-auto-insert
    '("\\.c\\'" . "C skeleton")
    '(nil
      "#include <stdio.h>" \n \n
      "int main() {" \n
      _ \n
      "return 0;" \n
      "}" > \n))

  (define-auto-insert '("\\.org\\'" . "Org skeleton")
    '(nil
      "#+TITLE: " (file-name-nondirectory (file-name-base (buffer-file-name))) \n
      "#+DATE: " (format-time-string "<%Y-%m-%d %a>")\n
      "#+SETUPFILE: setup.inc" \n \n
      _ \n))

  (define-auto-insert
    '("\\.py\\'" . "Python skeleton")
    '(nil
      "#!/usr/bin/env python" \n
      "# -*- coding: utf-8 -*-" \n \n
      _ \n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://stackoverflow.com/questions/17755665/how-to-call-describe-function-for-current-word-in-emacs
(defun describe-function-or-variable ()
  (interactive)
  (let ((sym (intern-soft (current-word))))
    (unless
        (cond ((null sym))
              ((not (eq t (help-function-arglist sym)))
               (describe-function sym))
              ((boundp sym)
               (describe-variable sym)))
      (message "Nothing found"))))

;; Idea from http://yakko.cs.wmich.edu/~rattles/development/misc/.emacs
;; Get help on current word
(defun man-current-word ()
  "Manual entry for the current word."
  (interactive)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode))
      (describe-function-or-variable)
    (manual-entry (current-word))))

(defun info-current-word ()
  "Info page for the current word."
  (interactive)
  (info-lookup-symbol (current-word)))

(global-set-key [f1]   'man-current-word)  ; Show man page for current word
(global-set-key [C-f1] 'info-current-word) ; Show info page for current word


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 当没有选择文本时，改变M-w的行为为复制当前行。
;; Idea from http://www.emacswiki.org/emacs/WholeLineOrRegion
;; 在slime-repl-mode下可能有问题，如何修复参见上面的链接。
(defun kill-ring-save-alternatively (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
          (goto-char end)
        (goto-char beg))
      (sit-for blink-matching-delay))))

(global-set-key [remap kill-ring-save] 'kill-ring-save-alternatively)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun save-buffers-kill-terminal-alternatively ()
  (interactive)
  ;; 对于GUI窗口，为防止误操作，不小心关掉所有tab，让C-x C-c无法退出emacs。
  (if window-system
      (progn
        (message "%s" "Please type 'C-x 5 0' or use mouse to close frame!"))
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal-alternatively)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 其它实用函数及设置
;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;; 重命名当前文件名
(defun my-rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

;; 摘自Writing GNU Emacs Extensions
(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-up (prefix-numeric-value n)))
(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-down (prefix-numeric-value n)))
;; 绑定向下移动屏幕一行
(global-set-key "\C-z" 'scroll-n-lines-ahead)
;; 绑定向上移动屏幕一行，但默认quoted-insert绑定在C-q
(global-set-key "\C-q" 'scroll-n-lines-behind)
;; 重新绑定quoted-insert，默认dired-toggle-read-only绑定在C-x C-q
;; (global-set-key "\C-x\C-q" 'quoted-insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux中，在终端模式下，利用xsel实现clipboard共享
;; Idea from:
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; Know Issue: 如果用/sudo:root@localhost:打开文件时，则clipboard不可用，会出错。
(if (string-equal system-type "gnu/linux")
    (if (and (executable-find "xsel") (getenv "DISPLAY"))
        (progn
          (defun xsel-cut-function (text &optional push)
            (with-temp-buffer
              (insert text)
              (call-process-region (point-min) (point-max)
                                   "xsel" nil 0 nil "--clipboard" "--input")))
          (defun xsel-paste-function()
            (let ((xsel-output
                   (shell-command-to-string "xsel --clipboard --output")))
              (unless (string= (car kill-ring) xsel-output)
                xsel-output )))
          (setq interprogram-cut-function 'xsel-cut-function)
          (setq interprogram-paste-function 'xsel-paste-function))
      (message "Warn: Tool xsel does not works, skip configuring clipboard")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://emacswiki.org/emacs/RecreateScratchBuffer
(defun switch-to-scratch-and-back ()
  "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (switch-to-buffer scratch-buffer-name))))

(global-set-key (kbd "M-0") 'switch-to-scratch-and-back)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 放弃更改，加载文件内容到buffer
(defun my-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 把当前行向上／下移动一行
;; Idea from http://blog.csdn.net/g9yuayon/article/details/1520466
;; (global-set-key (kbd "M-<up>") 'move-line-up)
;; (global-set-key (kbd "M-<down>") 'move-line-down)
(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col)))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy full file path into clipboard
;;
;; http://stackoverflow.com/questions/18812938/copy-full-file-path-into-copy-paste-clipboard
;; If you are in dired mode already, you don't need it. w will copy the file
;; name, c-u w will copy the relative file name and c-u 0 w will copy the
;; absolute file name
(defun my-copy-path (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      ;; For `Command + v`
      (if (eq system-type 'darwin) (paste-to-osx new-kill-string))
      ;; For `Ctrl + y`
      (kill-new new-kill-string))))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/DosToUnix
(defun my-dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invalid emacs config may break the normal behavior of kill-emacs such that it
;; stops at an error. In this case, we need a reliable method to kill emacs.
(defun my-kill-emacs-ignore-hooks ()
  "Like `kill-emacs', but ignores `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook) ; set kill-emacs-hook to nil before calling kill-emacs
    (kill-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 内置的hl-line-mode会高亮整个当前行，有点突兀。
;; 下面通过advice的方式仅高亮当前行行号，代码摘自：
;; https://stackoverflow.com/questions/10591334/colorize-current-line-number
(require 'hl-line)
(defface my-linum-hl
  `((t :inherit linum :background ,(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(defvar my-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-format)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-with-idle-timer
 1                                      ; after idle 1 second
 nil                                    ; no repeat, runs just once
 (lambda ()
   (load-file "~/.emacs.d/customize-jump-to-definition.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load other personal setting
(if (file-exists-p "~/.emacs-personal-init.el")
    (load-file "~/.emacs-personal-init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can run ‘M-x emacs-init-time’ to check emacs initialize time.
;;
;; 使用工具profile-dotemacs.el，可以检查哪段代码执行比较耗时。
;; $ emacs -Q -l ~/.emacs.d/profile-dotemacs.el -f profile-dotemacs
;; 参考：http://www.emacswiki.org/emacs/ProfileDotEmacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 如果某个步骤有性能问题，可以使用profiler检查问题
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Profiling.html
;;
;; M-x profiler-start
;; do something......
;; M-x profiler-report
;; 注：在报告中加号位置按回车可以展开更详细的报告，`Ctrl + u i`可以展开所有报告
