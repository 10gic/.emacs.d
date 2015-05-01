;;;

(setq jcl-syntax-table
      (let ((synTable (make-syntax-table)))

        ;;Emacs does not support comment strings of more than two characters in length.
        ;;Emacs不支持这种类型的注释：注释开始字符多于两个。
        ;;所以无法通过设置systax table的方式来标记注释。但可以通过设置font-lock-defaults来标记注释。

        ;;由于*和-常作为变量的一部分，应该把它们设置为w。
        ;;不设置的话，关键字会误匹配，如*integer*中的interger会误匹配为关键字。
        ;; (modify-syntax-entry ?* "w" synTable)
        ;; (modify-syntax-entry ?- "w" synTable)
        ;; (modify-syntax-entry ?_ "w" synTable)
        synTable))

(setq jcl-keywords
      `(
        (, (regexp-opt '("job" "dd"
                         "exec") 'words) . font-lock-keyword-face)
        (, (regexp-opt '("sysout" "dsn" "region"
                         "class" "msgclass" "parm" "unit" "disp" "space"
                         "pgm") 'words) . font-lock-variable-name-face)

        ;;The line beggin with //* is comment.
        ("^\/\/\\*.*" . font-lock-comment-face)
        ))

(define-derived-mode jcl-mode fundamental-mode "JCL" ;;JCL为mode的名称
  "jcl-mode is a major mode for JCL"
  :syntax-table jcl-syntax-table

  (setq font-lock-defaults '(jcl-keywords nil t nil nil))
  ;;font-lock-defaults的格式为(KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST [SYNTAX-BEGIN ...]]]])
  ;;;其中仅KEYWORDS是必须的，CASE-FOLD指示是否大小写敏感（这里设为了t,表示大小写不敏感）。
  )

(setq auto-mode-alist
      (append
       '(
           ("\\.jcl\\'" . jcl-mode))   ;; File name ends in '.jcl'.
         auto-mode-alist))

(provide 'jcl-mode)
