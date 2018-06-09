;; 保存文件后，执行add-timer-for-generating-tags，它会在空闲时间自动更新TAGS
(add-hook 'after-save-hook
          (lambda ()
            (if (derived-mode-p 'prog-mode)
                (add-timer-for-generating-tags nil))))

;; 打开程序后，执行add-timer-for-generating-tags，当工程根目录下TAGS不存在时会在
;; 空闲时间自动生成它；当工程根目录下TAGS存在时什么都不做
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode))
              (add-timer-for-generating-tags t))))

(defun my-update-tags-for-current-project ()
  "Update (or generate) TAGS file for current project.
   Firstly, try ctags, if fail, try `git grep --cached -Il '' | etags -`."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root))))
    (if project-dir
        ;; 在工程根目录生成TAGS文件
        ;; 说明：TAGS文件中的文件名要为绝对路径！否则jtags可能工作不正常
        (let* ((default-directory (file-name-as-directory project-dir))
               (tags-file (concat default-directory "TAGS")))
          (unless (ignore-errors
                    ;; -R选项是Exuberant Ctags才有，原始ctags不支持
                    ;; Mac下安装方法：`brew install ctags-exuberant`
                    ;; 如果ctags出错返回为非零，则process-lines会抛出异常
                    ;; 为了避免ctags运行很长时间导致Emacs hang，设置最长运行8秒
                    (let ((lines
                           (process-lines "perl"
                                          (expand-file-name
                                           "~/.emacs.d/bin/my_timeout.pl")
                                          "8" ; timeout value in seconds
                                          "ctags" "-Re" default-directory)))
                      ;; (dolist (line lines)
                      ;;  (message line)) ; only debug
                      )
                    (if (file-exists-p tags-file)
                        (message "Generated tags %s" tags-file)))
            (message "Generate TAGS fail (may be timeout or Exuberant Ctags is not installed), I will try etags")
            ;; 尝试使用etags生成TAGS（不过更推荐Exuberant Ctags）
            (if (file-exists-p (concat default-directory ".git"))
                ;; 若工程根目录存在.git目录，则使用下面命令生成TAGS
                ;; `git grep --cached -Il '' | etags -`
                ;; `git grep --cached -Il ''`是为了找到工程中所有非binary文件：
                ;; -I : don't match the pattern in binary files
                ;; -l : only show the matching file names, not matching lines
                ;; '' : empty string matches any file
                ;; 使用sed的目的是转换相对路径为绝对路径
                (progn
                  (shell-command-to-string
                   (concat "git grep --cached -Il '' | sed 's@^@" default-directory "@' | etags -"))
                  (if (file-exists-p tags-file)
                      (message "Generated tags %s" tags-file)))
              (message "Skip generating TAGS, only support git project"))))
      (message "Skip generating TAGS as fail to auto-detect project root"))))


;; 下面列表保存着工程名字，这些工程都存在还未执行的更新TAGS文件的timer
(setq my-projects-with-pending-timer '())
;; 如果TAGS文件的大小大于下面阈值，则不更新（注：更新大文件会很慢）。
(setq tag-file-size-threshold 500000000) ; 约500M
(defun add-timer-for-generating-tags (skip-if-tags-exsits)
  "设置timer，它会在emacs空闲时调用my-update-tags-for-current-project更新工程TAGS"
  (let* ((project-dir (ignore-errors (projectile-project-root))))
    (when (and
           ;; 如果当前文件位于工程中
           project-dir
           ;; 如果当前工程名不在my-projects-with-pending-timer中
           (not (member project-dir my-projects-with-pending-timer))
           ;; 如果TAGS不存在，或者TAGS存在但skip-if-tags-exsits为nil
           (or (not (file-exists-p
                     (concat (file-name-as-directory project-dir) "TAGS")))
               (and (file-exists-p
                     (concat (file-name-as-directory project-dir) "TAGS"))
                    (not skip-if-tags-exsits))))
      ;; 把当前工程名增加到my-projects-with-pending-timer中
      (add-to-list 'my-projects-with-pending-timer project-dir)
      (run-with-idle-timer
       5                                ; after idle some second
       nil                              ; no repeat, runs just once
       (lambda (dir)
         (let* ((current-dir (ignore-errors (projectile-project-root)))
                (tag-file-attr (file-attributes (concat (file-name-as-directory dir) "TAGS")))
                ;; Note: tag-file-attr is nil when file not exist
                ;; the 7th element is size attr
                (small-than-threshold (if tag-file-attr
                                          (< (nth 7 tag-file-attr) tag-file-size-threshold)
                                        t)))
           (if (string= dir current-dir)
               (if small-than-threshold
                   (my-update-tags-for-current-project)
                 (message "Skip updating TAGS as your TAGS file is too big"))
             (message "Skip updating TAGS for project (%s), as it's not current project" dir))
           ;; 操作完成后，从my-projects-with-pending-timer中删除工程名
           (setq my-projects-with-pending-timer
                 (remove dir my-projects-with-pending-timer))))
       project-dir))))

(provide 'init-autogen-tags)
