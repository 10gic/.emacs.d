;;; customize-jump-to-definition.el ---              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  cig01

;; Author: cig01
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use multiple packages to enhance the functionality of jump to definition

;;; Code:

(require 'projectile)
(projectile-mode)

;; https://stackoverflow.com/questions/4096580/how-to-make-emacs-reload-the-tags-file-automatically
;; https://emacs.stackexchange.com/questions/14802/never-keep-current-list-of-tags-tables-also
(setq tags-revert-without-query 1)      ; TAGS文件改变后重新加载时不提示用户
(setq tags-add-tables nil) ; 切换目录时，不提示“keep current list of tags tables also”

;; 保存文件后，执行add-timer-for-generating-tags，它会在空闲时间自动更新TAGS
(add-hook 'after-save-hook
          (lambda ()
            (add-timer-for-generating-tags nil)))

;; 打开程序后，执行add-timer-for-generating-tags，当工程根目录下TAGS不存在时会在
;; 空闲时间自动生成它；当工程根目录下TAGS存在时什么都不做
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode))
              (add-timer-for-generating-tags t))))

(defun my-update-tags-for-current-project ()
  "Update TAGS file for current project, if TAGS don't exist, generate it
Firstly, try projectile-regenerate-tags, ff fail, try `git grep --cached -Il '' | etags -`."
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
                    (process-lines "ctags" "-Re" default-directory)
                    (if (file-exists-p tags-file)
                        (message "Generated tags %s" tags-file)))
            (message "Generate TAGS fail (may be Exuberant Ctags is not installed), I will try etags")
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
         (let ((current-dir (ignore-errors (projectile-project-root))))
           (if (string= dir current-dir)
               (my-update-tags-for-current-project)
             (message "Skip updating TAGS for project (%s), as it's not current project" dir))
           ;; 操作完成后，从my-projects-with-pending-timer中删除工程名
           (setq my-projects-with-pending-timer
                 (remove dir my-projects-with-pending-timer))))
       project-dir))))


;; See https://github.com/jacktasia/dumb-jump
(require 'dumb-jump)               ; Package-Requires: (f "0.20.0") (s "1.11.0")
(dumb-jump-mode)

;; 设置找不到工程根目录时的搜索目录，默认为$HOME（很可能太慢）
(setq dumb-jump-default-project ".")

;; Dumb Jump默认绑定的快捷键：
;; C-M-g (dumb-jump-go)         跳到光标下符号的定义处
;; C-M-p (dumb-jump-back)       回到跳转前位置
;; C-M-q (dumb-jump-quick-look) 以tooltip形式显示光标下符号的定义的相关信息

;; 取消Dumb Jump中的部分绑定，因为它覆盖了Emacs内置的绑定
(define-key dumb-jump-mode-map (kbd "C-M-p") nil)
(define-key dumb-jump-mode-map (kbd "C-M-q") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 用my-find-definition-p记录xref-find-definitions是否成功跳转
;; 后面函数my-jump-to-definition中用它判断是否通过xref-find-definitions找到定义
(setq my-find-definition-p nil)
(add-hook 'xref-after-jump-hook
          (lambda ()
            (setq my-find-definition-p t)))

;; 用my-find-definition-by-jtags-p记录jtags-show-declaration是否成功跳转
;; 后面函数my-jump-to-definition中用它判断是否通过jtags-show-declaration找到定义
(setq my-find-definition-by-jtags-p nil)
(add-hook 'find-tag-hook
          (lambda ()
            (setq my-find-definition-by-jtags-p t)))

(defun my-jump-to-definition ()
  "Jump to definition."
  (interactive)
  (let* ((id-at-point (thing-at-point 'symbol))
         (current-file (if buffer-file-name buffer-file-name
                         (user-error "Can only used in buffer associated with file")))
         (java-file (string-suffix-p ".java" current-file))
         (identifier
          (if java-file
              ;; 后面将使用jtags查找java中的tag，但jtags只支持查找光标下符号，
              ;; 不支持手动指定其它tag。
              ;; 所以若当前文件是java，则总使用光标下符号（请确保光标下有符号）
              id-at-point
            (if (and (not current-prefix-arg) id-at-point)
                ;; 如果用户没有提供前缀参数，而且当前光标下有符号，则直接使用它
                id-at-point
              ;; 如果用户提供了前缀参数，或者当前光标下无符号，则提示用户输入
              (read-string
               (format "Find definition of %s: "
                       (if id-at-point
                           (concat "(default " id-at-point ")")
                         ""))
               nil nil id-at-point))))
         (find-def-p nil))
    (when java-file
      (setq my-find-definition-by-jtags-p nil)
      ;; jtags-show-declaration找不到定义时会提示Tag not found!
      (jtags-show-declaration)
      (if my-find-definition-by-jtags-p
          (setq find-def-p t)))
    (message "my-jump-to-definition find-def-p here1 ? %s" find-def-p)  ; just for debugging
    (unless find-def-p
      ;; 尝试使用xref-find-definitions查找identifier的定义位置
      ;; 如果xref-find-definitions成功地找到定义，而且定义是唯一的，则会直接跳到定
      ;; 义位置；如果定义不是唯一的（有多个位置），则会把所有可能列到*xref* buffer
      ;; 中。
      ;; 下面重置my-find-definition-p为nil，如果xref-find-definitions找到了唯一的
      ;; 定义则会设置my-find-definition-p为t（这是通过xref-after-jump-hook实现的）
      (setq my-find-definition-p nil)
      ;; 下面当*xref* buffer存在时关闭它，如果xref-find-definitions找到定义的多个
      ;; 位置，则会重新创建*xref* buffer
      (if (get-buffer "*xref*")
          (let ((cb (current-buffer)))
            (kill-buffer "*xref*")
            (switch-to-buffer cb)))
      (ignore-errors
        ;; xref-find-definitions找不符号定义时，会产生user-error
        (let ((project-dir (ignore-errors (projectile-project-root))))
          (when project-dir
            ;; 调用visit-tags-table是为了设置变量tags-file-name为TAGS文件位置
            ;; 如果不设置变量tags-file-name，xref-find-definitions可能提示用户输入
            ;; TAGS文件位置，提示消息类似于：
            ;; "Visit tags table (default TAGS): ..."
            (visit-tags-table (concat (file-name-directory project-dir) "TAGS") t)
            (xref-find-definitions identifier))))
      (if (or my-find-definition-p (get-buffer "*xref*"))
          ;; 当my-find-definition-p为t时，说明xref-find-definitions找到了唯一定义
          ;; 当*xref* buffer存在时，说明xref-find-definitions找到了多个定义
          ;; 只要满足一个条件，则认为第一次尝试成功
          (setq find-def-p t)))
    (message "my-jump-to-definition find-def-p here2 ? %s" find-def-p)  ; just for debugging
    (unless find-def-p
      ;; 尝试使用semantic-ia-fast-jump查找光标下符号的定义（不一定是identifier）
      (setq find-def-p
            (ignore-errors
              ;; semantic-ia-fast-jump找不到符号定义时，可能询问用户是否查找光标
              ;; 下前一个位置的符号定义，下面通过cl-flet/cl-letf直接拒绝这种询问
              ;; （参考https://www.emacswiki.org/emacs/YesOrNoP）。找不到符号时
              ;; 也可能产生error，这时，ignore-errors会返回nil
              (cl-flet ((always-no (&rest _) nil))
                (cl-letf (((symbol-function 'y-or-n-p) #'always-no)
                          ((symbol-function 'yes-or-no-p) #'always-no))
                  (call-interactively 'semantic-ia-fast-jump))))))
    (message "my-jump-to-definition find-def-p here3 ? %s" find-def-p)  ; just for debugging
    (message nil) ; 清空minibuffer
    (unless find-def-p
      ;; 如果前面的尝试都失败，则调用dumb-jump-go作最后尝试
      (dumb-jump-go nil nil identifier))))

(if (boundp 'aquamacs-version)
    ;; 绑定my-jump-to-definition到Command + Ctrl + j （Aquamacs中，Command键为A）
    (define-key osx-key-mode-map (kbd "A-C-j") 'my-jump-to-definition))
;; 上面绑定仅在Aquamacs中有效，在Emacs中使用下面设定可绑定到Command + Ctrl + j
;; (global-set-key (kbd "C-s-<268632074>") 'my-jump-to-definition) ; ; Command + Ctrl + j

;; (provide 'customize-jump-to-definition)
;;; customize-jump-to-definition.el ends here
