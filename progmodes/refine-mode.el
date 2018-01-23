;;; refine-mode.el --- Major mode for editing REFINE files

;; Copyright (C) 2012-2016 cig01

;; Author: cig01
;; Keywords: refine
;; Version: 0.0.1

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(setq refine-keywords
      `(
        ( ,(regexp-opt '("symbol" "integer" "tuple" "string" "real" "char" "boolean" "any-type" "map" "set" "seq"
                         ;;pseudoterminal name
                         "re::--integer--"
                         "re::--real--"
                         "re::--char--"
                         "re::--string--"
                         "re::--symbol--"
                         "re::--end--"
                         ;;type in lexical analyzer.
                         "Stream-Sequence"
                         "Parse-Control-Block"
                         "lex-value"
                         ) 'words) . font-lock-type-face)

        ( , (regexp-opt '(
                          ;;functions in common lisp
                          "abs"
                          "adjust-array"
                          "append"
                          "apply"
                          "aref"
                          "array-dimension"
                          "array-total-size"
                          "assert"
                          "boundp"
                          "break"
                          "caar"
                          "cadadr"
                          "cadr"
                          "car"
                          "catch"
                          "cdadr"
                          "cdar"
                          "cddr"
                          "cdr"
                          "ceiling"
                          "cerror"
                          "char-code"
                          "char-downcase"
                          "char-upcase"
                          "characterp"
                          "close"
                          "code-char"
                          "coerce"
                          "concatenate"
                          "cons"
                          "consp"
                          "copy-list"
                          "count"
                          "delete-file"
                          "delete-if"
                          "digit-char"
                          "digit-char-p"
                          "directory"
                          "directory-namestring"
                          "elt"
                          "enough-namestring"
                          "environment-variable"
                          "eq"
                          "equal"
                          "error"
                          "every"
                          "export"
                          "expt"
                          "fboundp"
                          "file-namestring"
                          "file-write-date"
                          "fill-pointer"
                          "find"
                          "find-package"
                          "find-symbol"
                          "first"
                          "floor"
                          "format"
                          "funcall"
                          "get"
                          "get-output-stream-string"
                          "get-universal-time"
                          "gethash"
                          "hash-table-count"
                          "hash-table-p"
                          "identity"
                          "in-package"
                          "incf"
                          "integerp"
                          "intern"
                          "length"
                          "list"
                          "listp"
                          "lower-case-p"
                          "make-array"
                          "make-hash-table"
                          "make-pathname"
                          "make-string"
                          "make-string-output-stream"
                          "map"
                          "mapc"
                          "mapcar"
                          "maphash"
                          "max"
                          "member"
                          "merge-pathnames"
                          "min"
                          "multiple-value-list"
                          "namestring"
                          "nconc"
                          "nreverse"
                          "nthcdr"
                          "null"
                          "open"
                          ;;"or"
                          "otherwise"
                          "package-name"
                          "parse-integer"
                          "pathname"
                          "pathname-directory"
                          "pathname-match-p"
                          "pathname-name"
                          "pathname-type"
                          "peek-char"
                          "pop"
                          "position"
                          "position-if"
                          "position-if-not"
                          "precompile-regexp"
                          "print"
                          "probe-file"
                          "push"
                          "rassoc"
                          "read"
                          "read-char"
                          "read-from-string"
                          "read-line"
                          "reduce"
                          "remhash"
                          "remove"
                          "remove-duplicates"
                          "remove-if"
                          "rename-file"
                          "rest"
                          "reverse"
                          "round"
                          "rplacd"
                          "search"
                          "second"
                          "setf"
                          "sleep"
                          "sort"
                          "string-capitalize"
                          "string-downcase"
                          "string-equal"
                          "string-lessp"
                          "string-trim"
                          "string-upcase"
                          "stringp"
                          "subseq"
                          "svref"
                          "symbol-name"
                          "symbol-package"
                          "symbolp"
                          "throw"
                          "truename"
                          "unexport"
                          "use-package"
                          "vector"
                          "warn"
                          "wild-pathname-p"
                          "write-string"

                          ) 'words) . font-lock-variable-name-face) ;;Use this face to color the functions come from common lisp.


        ( ,(regexp-opt '("concat" "filter" "image" "reduce" "closure-under" "set-to-seq" "set-to-map" "empty"  "reverse" "domain" "range" "last" "subseq" "insert" "delete" "append" "prepend" "seq-to-set" "seq-to-map" "map-to-set" "compose" "tclosure" "format" "print" "assert" "in-grammar"
                         "unique-names-class"
                         "find-object-class"
                         "instance-of"
                         "instances"
                         "class-superclasses"
                         "class-subclasses"
                         "cache"
                         "undefine-when-parsing"
                         "define-fun-converses"
                         "find-attribute"
                         "class-attributes"
                         "subclass-attributes"
                         "retrieve-attribute"
                         "store-attribute"
                         "set-attrs"
                         "name"
                         "name-of"
                         "find-object"
                         "define-tree-attributes"
                         "tree-attributes"
                         "kids"
                         "descendants"
                         "descendants-of-class"
                         "parent-expr"
                         "ancestors"
                         "ancestors-of-class"
                         "least-ancestor-of-class"
                         "up-to-root"
                         "copy-term"
                         "preorder-transform"
                         "fast-preorder-transform"
                         "postorder-transform"
                         "fast-postorder-transform"
                         "applies-map"
                         "make-object"
                         "erase-object"
                         ;;other functions
                         "parse-file"
                         "parse-from-file"
                         "parse-from-string"
                         "merge-input-dump-descs"
                         "make-dump-desc"
                         "pob-dump-file"
                         "pob-load-file"
                         ;;functions in lexical analyzer
                         "ss-empty"
                         "ss-first"
                         "ss-rest"
                         "ss-diff"
                         "ss-suffix"
                         "ss-prefix"
                         "ss-left-trim"
                         "ss-left-trim-until"
                         "ss-left-trim-match"
                         "ss-left-trim-until-match"
                         "keyword-legal"
                         "Pseudoterminal-Legal"
                         "lex-keyword"
                         "lex-grammar-prefix"
                         "lex-end-of-form"
                         "report-syntax-error"
                         ) 'words) . font-lock-function-name-face)


        ("\\<\\(defined\\|undefined\\|object\\|object-class\\|attribute\\|ancestor\\|term-equal\\)\\>\\?" . font-lock-function-name-face) ;;高亮以问号结尾的这些函数

        ;;        ("\\<lisp::[^(]*\\>" . font-lock-function-name-face) ;;以lisp::开头的函数高亮。
        ;;        ("\\<cl::[^(]*\\>" . font-lock-function-name-face) ;;以cl::开头的函数高亮。
        ;;        ("\\<re::[^(]*\\>" . font-lock-function-name-face) ;;以re::开头的函数高亮。

        ( ,(regexp-opt '("=>" "<=" "-->" "->" "<-" "::=" "##r" )) . font-lock-keyword-face)
        ( ,(regexp-opt '("true" "false" "constant" "let" "function" "rule" "form" "computed-using" "lambda" "if" "elseif" "then" "and-then" "else" "enumerate" "over" "do" "while" "replace" "or" "or-else" "and-then" "in" "subset" "union" "intersect" "setdiff" "with" "less" "size" "arb" "div" "mod" "fa" "ex" "some" "type" "var" "object-class" "subtype-of" "undefined"
                         ;;keyword in grammar
                         "grammar" "inherits-from" "local-nonterminals" "local-pseudoterminals" "productions" "precedence" "brackets" "matching" "end" "builds" "print-only" "semantics" "start-classes" "file-classes" "special-syntax" "in-all-ancestors" "at-or-above" "pattern-syntax" "no-patterns" "for" "same-level" "associativity" "production-precedence" "keyword-alternatives" "may-replace" "lexical-analysis-fn" "package-prefix" "symbol-start-chars" "symbol-continue-chars" "case-sensitive" "whitespace-chars" "use-rightmost-flattener" "comments" "matching" "nested"
                         ) 'words) . font-lock-keyword-face)
        ))

(setq refine-syntax-table
      (let ((synTable (make-syntax-table)))
        ;;设置注释
        (modify-syntax-entry ?% "< b" synTable)   ; % 开始直到遇到换行
        (modify-syntax-entry ?\n "> b" synTable)
        (modify-syntax-entry ?\# ". 14" synTable) ; #| 开始直到遇到 |#
        (modify-syntax-entry ?\| ". 23" synTable)

        ;;由于*和-常作为变量的一部分，应该把它们设置为w。
        ;;不设置的话，关键字会误匹配，如*integer*中的interger会误匹配上。
        (modify-syntax-entry ?* "w" synTable)
        (modify-syntax-entry ?- "w" synTable)
        (modify-syntax-entry ?_ "w" synTable)

        synTable))


(define-derived-mode refine-mode fundamental-mode "DSL-REFINE" ;;DSL-REFINE为mode的名称
  "refine-mode is a major mode for refine";;mode的说明
  :syntax-table refine-syntax-table

  (setq font-lock-defaults '(refine-keywords nil t nil nil))
  ;;font-lock-defaults的格式为(KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST [SYNTAX-BEGIN ...]]]])
  ;;;其中仅KEYWORDS是必须的，CASE-FOLD指示是否大小写敏感（这里设为了t,表示大小写不敏感）。

  ;;设置C-M-a和C-M-e时的跳转位置。
  (setq defun-prompt-regexp "^function.*")

  ;;设置imenu，它可以被speedbar，ecb等等工具使用。
  (setq imenu-generic-expression
        '( ("var" "^var +\\([^ :\n]*\\)" 1)
           ("constant" "^constant +\\([^ :\n]*\\)" 1)
           ("type" "^type +\\([^ =\n]*\\).*=.*" 1)  ;;其中.*＝.*表示必然本行含有等号"="。
           ("form" "^form +\\([^ \n]*\\)" 1)
           ("function" "^function +\\([^ (\n]*\\)" 1)
           ("rule" "^rule +\\([^ (\n]*\\)" 1)))
  ;;(imenu-add-to-menubar "REFINE-Index")
  )

(setq auto-mode-alist
      (append
       '(
         ("\\.re\\'" . refine-mode)   ;; File name ends in '.re'.
         )
       auto-mode-alist))

;;设置speedbar自动识别.re文件。
;;(speedbar-add-supported-extension ".re")

(provide 'refine-mode)
