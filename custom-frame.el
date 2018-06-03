;; 字体相关设置
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
      font-size-pair-list-ms            ; MS Windows
    font-size-pair-list-unix))          ; Linux, Mac

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

  (set-cursor-color "red") ; 定制光标颜色，这样在“深色”主题下更醒目
  (setq-default frame-title-format
                '(:eval
                  (format "%s"
                          (cond
                           (buffer-file-name buffer-file-name)
                           (dired-directory dired-directory)
                           (t (buffer-name)))))))

;; 设置daemon方式和非deamon方式启动时都执行my-set-frame
(if (and (fboundp 'daemonp) (daemonp))
    ;; 以daemon方式启动时，要使与X相关配置生效，应使用这个hook
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my-set-frame))))
  (my-set-frame))

(when (boundp 'aquamacs-version)
  (define-key osx-key-mode-map (kbd "A-=") 'my-increase-font-size)  ; Cmd + =
  (define-key osx-key-mode-map (kbd "A--") 'my-decrease-font-size)) ; Cmd + -

(add-hook 'before-make-frame-hook
          #'(lambda () (load-file "~/.emacs.d/init.el")))
