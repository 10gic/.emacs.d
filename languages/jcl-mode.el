;;; jcl-mode.el --- A JCL major mode, it just set face for keywords/comments.

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

;; Author: 10gic <juhani@163.com>
;; Maintainer: 10gic <juhani@163.com>
;; Keywords: languages, JCL

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:
;;
;; Job Control Language (JCL) is a name for scripting languages used on IBM
;; mainframe operating systems to instruct the system on how to run a batch job
;; or start a subsystem.
;; Refer to: http://en.wikipedia.org/wiki/Job_Control_Language


(setq jcl-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; The comments line in JCL is begin with "//*" (three characters), But Emacs
        ;; does not support comment strings of more than two characters in length. So,
        ;; we cannot set comments style in syntax table. As a workaround, we can
        ;; change comments line as "font-lock-comment-face" in font-lock-defaults.

        synTable))

(defvar jcl-keywords-directives
  '( "DD"            "EXEC"          "JOB"
     ))

(defvar jcl-builtin-util-directives
  '( "IKJEFT01"      "IDCAMS"
     ))

(defvar jcl-variables-directives
  '( "SYSOUT"        "SYSIN"         "SYSTSIN"       "DSN"
     "REGION"        "CLASS"         "MSGCLASS"      "MSGLEVEL"
     "UNIT"          "DISP"          "SPACE"         "PARM"
     "NOTIFY"        "MAXCC"         "LASTCC"        "DCB"
     "PGM"
     ))

(setq jcl-keywords
      `(
        ;; The line beggin with //* is comments in JCL.
        ("^\/\/\\*.*" . font-lock-comment-face)

        ;; set face for jcl-keywords-directives
        (, (regexp-opt jcl-keywords-directives 'words) . font-lock-keyword-face)

        ;; set face for jcl-builtin-util-directives
        (, (regexp-opt jcl-builtin-util-directives 'words) . font-lock-builtin-face)

        ;; set face for jcl-variables-directives
        (, (regexp-opt jcl-variables-directives 'words)
           . font-lock-variable-name-face)
        ))

(define-derived-mode jcl-mode fundamental-mode "JCL" ;; "JCL" is mode name.
  "jcl-mode is a major mode for JCL"
  :syntax-table jcl-syntax-table

  (setq font-lock-defaults '(jcl-keywords nil t nil nil))
  ;; font-lock-defaults should be of the form:
  ;; (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST [SYNTAX-BEGIN ...]]]])
  ;; JCL is case insensitive, so CASE-FOLD is t.
  )

(setq auto-mode-alist
      (append
       '(
         ("\\.jcl\\'" . jcl-mode))   ;; File name ends in '.jcl'.
       auto-mode-alist))

(provide 'jcl-mode)

;;; jcl-mode.el ends here
