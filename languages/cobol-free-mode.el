;;
;;这个文件修改自cobol-mode.el，使用cobol-free-mode前必须先加载cobol-mode.el
;;因为cobol-mode-syntax-table,cobol-keywords-directives,cobol-keywords-reserved,cobol-keywords-std-fcns,cobol-keywords-builtin这五个变量来自文件cobol-mode.el
(require 'cobol-mode)

;; (defvar cobol-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     (modify-syntax-entry ?\n " " st)
;;     (modify-syntax-entry ?\! "." st)
;;     (modify-syntax-entry ?\" "." st)  ; wiki \" bug workaround comment
;;     (modify-syntax-entry ?\# "w" st)
;;     (modify-syntax-entry ?\$ "w" st)
;;     (modify-syntax-entry ?\% "'" st)
;;     (modify-syntax-entry ?\& "'" st)
;;     (modify-syntax-entry ?\' "." st)
;;     (modify-syntax-entry ?\( "()" st)
;;     (modify-syntax-entry ?\) ")(" st)
;;     (modify-syntax-entry ?\* "." st)
;;     (modify-syntax-entry ?\+ "." st)
;;     (modify-syntax-entry ?\, "." st)
;;     (modify-syntax-entry ?\- "w" st)
;;     (modify-syntax-entry ?\. "." st)
;;     (modify-syntax-entry ?\/ "." st)
;;     (modify-syntax-entry ?\: "." st)
;;     (modify-syntax-entry ?\; "." st)
;;     (modify-syntax-entry ?\< "." st)
;;     (modify-syntax-entry ?\= "." st)
;;     (modify-syntax-entry ?\> "." st)
;;     (modify-syntax-entry ?\? "." st)
;;     (modify-syntax-entry ?\@ "." st)
;;     (modify-syntax-entry ?\[ "(]" st)
;;     (modify-syntax-entry ?\\ "." st)
;;     (modify-syntax-entry ?\] ")[" st)
;;     (modify-syntax-entry ?^  "w" st)
;;     (modify-syntax-entry ?\_ "w" st)
;;     (modify-syntax-entry ?\{ "(}" st)
;;     (modify-syntax-entry ?\| "." st)
;;     (modify-syntax-entry ?\} "){" st)
;;     st)
;;   "Syntax table for `cobol-mode'.")

;; All keyword lists get sorted so new words can be anywhere within the
;; appropriate list.  The keywords are currently only used for highlighting but
;; more uses such as abbrev-mode are in progress.

;; (defvar cobol-keywords-directives
;;   '( "ANSI"                   "BLANK"                  "NOBLANK"
;;      "CALL-SHARED"            "CANCEL"                 "NOCANCEL"
;;      "CHECK"                  "CODE"                   "NOCODE"
;;      "COLUMNS"                "COMPACT"                "NOCOMPACT"
;;      "COMPILE"                "CONSULT"                "NOCONSULT"
;;      "CROSSREF"               "NOCROSSREF"             "DIAGNOSE-74"
;;      "NODIAGNOSE-74"          "DIAGNOSE-85"            "NODIAGNOSE-85"
;;      "DIAGNOSEALL"            "NODIAGNOSEALL"          "ENDIF"
;;      "ENDUNIT"                "ENV"                    "ERRORFILE"
;;      "ERRORS"                 "FIPS"                   "NOFIPS"
;;      "FMAP"                   "HEADING"                "HEAP"
;;      "HIGHPIN"                "HIGHREQUESTERS"         "ICODE"
;;      "NOICODE"                "IF"                     "IFNOT"
;;      "INNERLIST"              "NOINNERLIST"            "INSPECT"
;;      "NOINSPECT"              "LARGEDATA"              "LD"
;;      "LESS-CODE"              "LIBRARY"                "LINES"
;;      "LIST"                   "NOLIST"                 "LMAP"
;;      "NOLMAP"                 "MAIN"                   "MAP"
;;      "NOMAP"                  "NLD"                    "NONSTOP"
;;      "NON-SHARED"             "OPTIMIZE"               "PERFORM-TRACE"
;;      "PORT"                   "NOPORT"                 "RESETTOG"
;;      "RUNNABLE"               "RUNNAMED"               "SAVE"
;;      "SAVEABEND"              "NOSAVEABEND"            "SEARCH"
;;      "NOSEARCH"               "SECTION"                "SETTOG"
;;      "SHARED"                 "SHOWCOPY"               "NOSHOWCOPY"
;;      "SHOWFILE"               "NOSHOWFILE"             "SOURCE"
;;      "SQL"                    "NOSQL"                  "SQLMEM"
;;      "SUBSET"                 "SUBTYPE"                "SUPPRESS"
;;      "NOSUPPRESS"             "SYMBOLS"                "NOSYMBOLS"
;;      "SYNTAX"                 "TANDEM"                 "TRAP2"
;;      "NOTRAP2"                "TRAP2-74"               "NOTRAP2-74"
;;      "UL"                     "WARN"                   "NOWARN"
;;    )
;;   "List of COBOL compiler directives.
;; Used to create the `font-lock-keywords' table.")


;; (defvar cobol-keywords-reserved
;;   '( "ACCEPT"                 "ACCESS"                 "ADD"
;;      "ADDRESS"                "ADVANCING"              "AFTER"
;;      "ALL"                    "ALPHABET"               "ALPHABETIC"
;;      "ALPHABETIC-LOWER"       "ALPHABETIC-UPPER"       "ALPHANUMERIC"
;;      "ALPHANUMERIC-EDITED"    "ALSO"                   "ALTER"
;;      "ALTERNATE"              "AND"                    "ANY"
;;      "APPROXIMATE"            "AREA"                   "AREAS"
;;      "ASCENDING"              "ASSIGN"                 "AT"
;;      "AUTHOR"                 "BEFORE"                 "BINARY"
;;      "BLANK"                  "BLOCK"                  "BOTTOM"
;;      "BY"                     "CALL"                   "CANCEL"
;;      "CD"                     "CF"                     "CH"
;;      "CHARACTER"              "CHARACTERS"             "CHARACTER-SET"
;;      "CHECKPOINT"             "CLASS"                  "CLOCK-UNITS"
;;      "CLOSE"                  "COBOL"                  "CODE"
;;      "CODE-SET"               "COLLATING"              "COLUMN"
;;      "COMMA"                  "COMMON"                 "COMMUNICATION"
;;      "COMP"                   "COMP-3"                 "COMP-5"
;;      "COMPUTATIONAL"          "COMPUTATIONAL-3"        "COMPUTATIONAL-5"
;;      "COMPUTE"                "CONFIGURATION"          "CONTAINS"
;;      "CONTENT"                "CONTINUE"               "CONTROL"
;;      "CONTROLS"               "CONVERTING"             "COPY"
;;      "CORR"                   "CORRESPONDING"          "COUNT"
;;      "CURRENCY"               "DATA"                   "DATE"
;;      "DATE-COMPILED"          "DATE-WRITTEN"           "DAY"
;;      "DAY-OF-WEEK"            "DE"                     "DEBUG-CONTENTS"
;;      "DEBUG-ITEM"             "DEBUG-LINE"             "DEBUG-SUB-2"
;;      "DEBUG-SUB-3"            "DEBUGGING"              "DECIMAL-POINT"
;;      "DECLARATIVES"           "DEBUG-NAME"             "DEBUG-SUB-1"
;;      "DELETE"                 "DELIMITED"              "DELIMITER"
;;      "DEPENDING"              "DESCENDING"             "DESTINATION"
;;      "DETAIL"                 "DISABLE"                "DISPLAY"
;;      "DIVIDE"                 "DIVISION"               "DOWN"
;;      "DUPLICATES"             "DYNAMIC"                "EGI"
;;      "ELSE"                   "EMI"                    "ENABLE"
;;      "END"                    "END-ADD"                "END-COMPUTE"
;;      "END-DELETE"             "END-DIVIDE"             "END-EVALUATE"
;;      "END-IF"                 "END-MULTIPLY"           "END-OF-PAGE"
;;      "END-PERFORM"            "END-READ"               "END-RECEIVE"
;;      "END-RETURN"             "END-REWRITE"            "END-SEARCH"
;;      "END-START"              "END-STRING"             "END-SUBTRACT"
;;      "END-UNSTRING"           "END-WRITE"              "ENTER"
;;      "EOP"                    "EQUAL"                  "ERROR"
;;      "ESI"                    "EVALUATE"               "EVERY"
;;      "EXCEPTION"              "EXCLUSIVE"              "EXIT"
;;      "EXTEND"                 "EXTENDED-STORAGE"       "EXTERNAL"
;;      "FALSE"                  "FD"                     "FILE"
;;      "FILE-CONTROL"           "FILLER"                 "FINAL"
;;      "FIRST"                  "FOOTING"                "FOR"
;;      "FROM"                   "FUNCTION"               "GENERATE"
;;      "GENERIC"                "GIVING"                 "GLOBAL"
;;      "GO"                     "GREATER"                "GROUP"
;;      "GUARDIAN-ERR"           "HEADING"                "HIGH-VALUE"
;;      "HIGH-VALUES"            "I-O"                    "I-O-CONTROL"
;;      "IDENTIFICATION"         "IF"                     "IN"
;;      "INDEX"                  "INDEXED"                "INDICATE"
;;      "INITIAL"                "INITIALIZE"             "INITIATE"
;;      "INPUT"                  "INPUT-OUTPUT"           "INSPECT"
;;      "INSTALLATION"           "INTO"                   "INVALID"
;;      "IS"                     "JUST"                   "JUSTIFIED"
;;      "KEY"                    "LABEL"                  "LAST"
;;      "LEADING"                "LEFT"                   "LENGTH"
;;      "LESS"                   "LIMIT"                  "LIMITS"
;;      "LINAGE"                 "LINAGE-COUNTER"         "LINE"
;;      "LINE-COUNTER"           "LINKAGE"                "LOCK"
;;      "LOCKFILE"               "LOW-VALUE"              "LOW-VALUES"
;;      "MEMORY"                 "MERGE"                  "MESSAGE"
;;      "MODE"                   "MODULES"                "MOVE"
;;      "MULTIPLE"               "MULTIPLY"               "NATIVE"
;;      "NEGATIVE"               "NEXT"                   "NO"
;;      "NOT"                    "NULL"                   "NULLS"
;;      "NUMBER"                 "NUMERIC"                "NUMERIC-EDITED"
;;      "OBJECT-COMPUTER"        "OCCURS"                 "OF"
;;      "OFF"                    "OMITTED"                "ON"
;;      "OPEN"                   "OPTIONAL"               "OR"
;;      "ORDER"                  "ORGANIZATION"           "OTHER"
;;      "OUTPUT"                 "OVERFLOW"               "PACKED-DECIMAL"
;;      "PADDING"                "PAGE"                   "PAGE-COUNTER"
;;      "PERFORM"                "PF"                     "PH"
;;      "PIC"                    "PICTURE"                "PLUS"
;;      "POINTER"                "POSITION"               "POSITIVE"
;;      "PRINTING"               "PROCEDURE"              "PROCEDURES"
;;      "PROCEED"                "PROGRAM"                "PROGRAM-ID"
;;      "PROGRAM-STATUS"         "PROGRAM-STATUS-1"       "PROGRAM-STATUS-2"
;;      "PROMPT"                 "PROTECTED"              "PURGE"
;;      "QUEUE"                  "QUOTE"                  "QUOTES"
;;      "RANDOM"                 "RD"                     "READ"
;;      "RECEIVE"                "RECEIVE-CONTROL"        "RECORD"
;;      "RECORDS"                "REDEFINES"              "REEL"
;;      "REFERENCE"              "REFERENCES"             "RELATIVE"
;;      "RELEASE"                "REMAINDER"              "REMOVAL"
;;      "RENAMES"                "REPLACE"                "REPLACING"
;;      "REPLY"                  "REPORT"                 "REPORTING"
;;      "REPORTS"                "RERUN"                  "RESERVE"
;;      "RESET"                  "RETURN"                 "REVERSED"
;;      "REWIND"                 "REWRITE"                "RF"
;;      "RH"                     "RIGHT"                  "ROUNDED"
;;      "RUN"                    "SAME"                   "SD"
;;      "SEARCH"                 "SECTION"                "SECURITY"
;;      "SEGMENT"                "SEGMENT-LIMIT"          "SELECT"
;;      "SEND"                   "SENTENCE"               "SEPARATE"
;;      "SEQUENCE"               "SEQUENTIAL"             "SET"
;;      "SHARED"                 "SIGN"                   "SIZE"
;;      "SORT"                   "SORT-MERGE"             "SOURCE"
;;      "SOURCE-COMPUTER"        "SPACE"                  "SPACES"
;;      "SPECIAL-NAMES"          "STANDARD"               "STANDARD-1"
;;      "STANDARD-2"             "START"                  "STARTBACKUP"
;;      "STATUS"                 "STOP"                   "STRING"
;;      "SUB-QUEUE-1"            "SUB-QUEUE-2"            "SUB-QUEUE-3"
;;      "SUBTRACT"               "SUM"                    "SUPPRESS"
;;      "SYMBOLIC"               "SYNC"                   "SYNCDEPTH"
;;      "SYNCHRONIZED"           "TABLE"                  "TAL"
;;      "TALLYING"               "TAPE"                   "TERMINAL"
;;      "TERMINATE"              "TEST"                   "TEXT"
;;      "THAN"                   "THEN"                   "THROUGH"
;;      "THRU"                   "TIME"                   "TIMES"
;;      "TO"                     "TOP"                    "TRAILING"
;;      "TRUE"                   "TYPE"                   "UNIT"
;;      "UNLOCK"                 "UNLOCKFILE"             "UNLOCKRECORD"
;;      "UNSTRING"               "UNTIL"                  "UP"
;;      "UPON"                   "USAGE"                  "USE"
;;      "USING"                  "VALUE"                  "VALUES"
;;      "VARYING"                "WHEN"                   "WITH"
;;      "WORDS"                  "WORKING-STORAGE"        "WRITE"
;;      "ZERO"                   "ZEROES"
;;      )
;;   "List of COBOL keywords reserved only in certain language contexts.
;; Used to create the `font-lock-keywords' table.")

;; (defvar cobol-keywords-std-fcns
;;   '( "ACOS"                   "ANNUITY"                "ASIN"
;;      "ATAN"                   "CHAR"                   "COS"
;;      "CURRENT-DATE"           "DATE-OF-INTEGER"        "DAY-OF-INTEGER"
;;      "FACTORIAL"              "INTEGER"                "INTEGER-OF-DATE"
;;      "INTEGER-OF-DAY"         "INTEGER-PART"           "LENGTH"
;;      "LOG"                    "LOG10"                  "LOWER-CASE"
;;      "MAX"                    "MEAN"                   "MEDIAN"
;;      "MIDRANGE"               "MIN"                    "MOD"
;;      "NUMVAL"                 "NUMVAL-C"               "ORD"
;;      "ORD-MAX"                "ORD-MIN"                "PRESENT-VALUE"
;;      "RANDOM"                 "RANGE"                  "REM"
;;      "REVERSE"                "SIN"                    "SQRT"
;;      "STANDARD-DEVIATION"     "SUM"                    "TAN"
;;      "UPPER-CASE"             "VARIANCE"               "WHEN-COMPILED"
;;    )
;;   "List of COBOL standard functions.
;; Used to create the `font-lock-keywords' table.")


;; (defvar cobol-keywords-builtin
;;   '( "#IN"                             "#OUT"
;;      "#TERM"                           "#TEMP"
;;      "#DYNAMIC"                        "COBOL85^ARMTRAP"
;;      "COBOL85^COMPLETION"              "COBOL_COMPLETION_"
;;      "COBOL_CONTROL_"                  "COBOL_GETENV_"
;;      "COBOL_PUTENV_"                   "COBOL85^RETURN^SORT^ERRORS"
;;      "COBOL_RETURN_SORT_ERRORS_"       "COBOL85^REWIND^SEQUENTIAL"
;;      "COBOL_REWIND_SEQUENTIAL_"        "COBOL85^SET^SORT^PARAM^TEXT"
;;      "COBOL_SET_SORT_PARAM_TEXT_"      "COBOL85^SET^SORT^PARAM^VALUE"
;;      "COBOL_SET_SORT_PARAM_VALUE_"     "COBOL_SET_MAX_RECORD_"
;;      "COBOL_SETMODE_"                  "COBOL85^SPECIAL^OPEN"
;;      "COBOL_SPECIAL_OPEN_"             "COBOLASSIGN"
;;      "COBOL_ASSIGN_"                   "COBOLFILEINFO"
;;      "COBOL_FILE_INFO_"                "COBOLSPOOLOPEN"
;;      "CREATEPROCESS"                   "ALTERPARAMTEXT"
;;      "CHECKLOGICALNAME"                "CHECKMESSAGE"
;;      "DELETEASSIGN"                    "DELETEPARAM"
;;      "DELETESTARTUP"                   "GETASSIGNTEXT"
;;      "GETASSIGNVALUE"                  "GETBACKUPCPU"
;;      "GETPARAMTEXT"                    "GETSTARTUPTEXT"
;;      "PUTASSIGNTEXT"                   "PUTASSIGNVALUE"
;;      "PUTPARAMTEXT"                    "PUTSTARTUPTEXT"
;;      )
;;   "List of COBOL privileged builtin functions.
;; Used to create the `font-lock-keywords' table.")





(defvar cobol-free-keyword-fcn-names-regexp
  "^.\\{6\\}\\s-\\{1,4\\}\\(\\w+\\)\\s-*\\."
  "Defines a regexp that finds the names of paragraphs.
Used to create the `font-lock-keywords' table.")
(defvar cobol-free-keyword-section-names-regexp
  "^.\\{6\\}\\s-\\{1,4\\}\\(\\w+\\s-+\\(division\\|section\\)\\)\\."
  "Defines a regexp that finds the names of paragraphs.
Used to create the `font-lock-keywords' table.")


(defun cobol-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\W"))

;; The next 4 def's work tightly together and, as coded, cannot be reused for
;; additional purposes.
(defvar cobol-keyword-on-directive-line-regexp () "Internal use only.")
(defun  cobol-keyword-on-directive-line-regexp ( word-list )
"Returns a function to find WORD-LIST only if line starts with ?"
  (setq cobol-keyword-on-directive-line-regexp
        (concat "\\b"(regexp-opt word-list t)"\\b"))
  'cobol-font-lock-directive-line)



(defvar cobol-free-static-font-lock-keywords
  ;; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ;; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ;; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ;; not used here.  For simplicity, all regexp's were designed so MATCH would
  ;; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `(
    (,(cobol-keyword-on-directive-line-regexp cobol-keywords-directives)
     1 font-lock-builtin-face)
    (,(cobol-keyword-anywhere-regexp cobol-keywords-builtin)
     1 font-lock-builtin-face)
    (,(cobol-keyword-anywhere-regexp (append cobol-keywords-std-fcns
                                           cobol-keywords-reserved))
     1 font-lock-keyword-face)
    (,cobol-free-keyword-section-names-regexp 1 font-lock-type-face)
    (,cobol-free-keyword-fcn-names-regexp 1 font-lock-function-name-face)))


(defvar cobol-free-font-lock-keywords ())


(defun cobol-free-build-font-lock-keywords ()
  "Creates `font-lock-keywords' based on current customize settings."
  ;;注释必须放在前面，否则注释里有关键字时不会被认为是注释。
  (append '(("^[*]+.*$" . font-lock-comment-face) ;;匹配注释。
            ("[']+[[:print:]]*[']+" . font-lock-string-face) ;;匹配单引号字符串。
            ("[\"]+[[:print:]]*[\"]+" . font-lock-string-face)) ;;匹配双引号字符串。
           cobol-free-static-font-lock-keywords))


(defun cobol-free-setup-font-lock ()
  "Sets up the buffer local value for font-lock-defaults and optionally
turns on font-lock-mode"
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.

  (set (make-local-variable 'cobol-free-font-lock-keywords)
       (cobol-free-build-font-lock-keywords))

  (setq font-lock-defaults
       '(cobol-free-font-lock-keywords nil t nil nil)))


;;;###autoload
(defun cobol-free-mode ()
  "A major mode for editing COBOL language program source files."

  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'major-mode) 'cobol-free-mode)
  (set (make-local-variable 'mode-name) "COBOL-free")
;;  (use-local-map cobol-mode-map)
  (set-syntax-table cobol-mode-syntax-table)
  (cobol-free-setup-font-lock)

)


(add-to-list 'magic-mode-alist '("\\(^.*\n\\)*[ ]\\{0,6\\}IDENTIFICATION" . cobol-free-mode))
;; 当IDENTIFICATION前面的空格为0-6(不到7)个时，设置为cobol-free-mode

(provide 'cobol-free-mode)
