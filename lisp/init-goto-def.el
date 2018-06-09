;;; init-goto-def.el ---              -*- lexical-binding: t; -*-

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

(projectile-mode)

;; 用my-find-definition-p记录xref-find-definitions是否成功跳转
;; 后面函数my-goto-def中用它判断是否通过xref-find-definitions找到定义
(setq my-find-definition-p nil)
(add-hook 'xref-after-jump-hook
          (lambda ()
            (setq my-find-definition-p t)))

;; 用my-find-definition-by-jtags-p记录jtags-show-declaration是否成功跳转
;; 后面函数my-goto-def中用它判断是否通过jtags-show-declaration找到定义
(setq my-find-definition-by-jtags-p nil)
(add-hook 'find-tag-hook
          (lambda ()
            (setq my-find-definition-by-jtags-p t)))

(defun my-goto-def ()
  "Jump to definition."
  (interactive)
  (let* ((id-at-point (thing-at-point 'symbol t))
         (current-file
          (if buffer-file-name buffer-file-name
            (user-error "Cannot use my-goto-def on a buffer without a file name")))
         (java-file-p (string-suffix-p ".java" current-file))
         (go-file-p (string-suffix-p ".go" current-file))
         (identifier
          (if (or java-file-p go-file-p)
              ;; 后面将使用jtags查找java中的tag，但jtags只支持查找光标下符号，不
              ;; 支持手动指定其它tag。类似地，后面将使用godef-jump查找go中的tag，
              ;; 它也只支持查找光标下符号。所以若当前文件是java/go，则总使用光标
              ;; 下符号（请确保光标下有符号）
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
    (when java-file-p
      (setq my-find-definition-by-jtags-p nil)
      ;; jtags-show-declaration找不到定义时会提示Tag not found!
      (jtags-show-declaration)
      (if my-find-definition-by-jtags-p
          (setq find-def-p t)))
    (when go-file-p
      ;; 尝试使用godef-jump查找定义
      (when (and (executable-find "godef") (fboundp 'godef-jump))
        ;; 后面会检查*Messages* buffer中的最后一行，为了防止使用旧数据，特意写入
        ;; 下面这一行无关数据
        (message "my-goto-def, magic clear for godef-jump")
        (ignore-errors
          (call-interactively 'godef-jump))
        (save-excursion
          (set-buffer "*Messages*")
          (goto-char (point-max))
          (let ((last-line (thing-at-point 'line t)))
            ;; 当godef-jump找不到光标下符号定义时，
            ;; 会输出类似下面的信息到*Messages*中
            ;; godef: no identifier found
            ;; godef: no declaration found for xxx
            (message "my-goto-def, last-line=%s" last-line)
            (if (string-prefix-p "godef: no" last-line)
                (message "my-goto-def, godef-jump don't find definition")
              (setq find-def-p t)))))
      ;; 尝试使用go-guru-definition查找定义
      (unless find-def-p
        (when (fboundp 'go-guru-definition)
          ;; 测试发现，当go-guru-definition找不到定义时会抛异常，
          ;; 如果没有异常则认为go-guru-definition找到了定义
          (if (ignore-errors            ; 有异常时ignore-errors返回nil
                (call-interactively 'go-guru-definition))
              (setq find-def-p t)
            (message "my-goto-def, go-guru-definition don't find definition")))))
    (message "my-goto-def, find-def-p here1 ? %s" find-def-p) ; just for debugging
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
    (message "my-goto-def, find-def-p here2 ? %s" find-def-p) ; just for debugging
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
    (message "my-goto-def, find-def-p here3 ? %s" find-def-p) ; just for debugging
    (message nil)                                              ; 清空minibuffer
    (when (not find-def-p)
      ;; 如果前面的尝试都失败，则调用dumb-jump-go作最后尝试
      (require 'dumb-jump)
      (dumb-jump-mode)
      (dumb-jump-go nil nil identifier))))

(provide 'init-goto-def)
;;; init-goto-def.el ends here
