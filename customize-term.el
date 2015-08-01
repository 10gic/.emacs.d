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

;; bind key when system is not windows
(when (not (eq system-type 'windows-nt))
  (global-set-key (kbd "<f2>") 'visit-ansi-term))

; term 模式下不绑定M-0, M-1等，它们有其它用处。
(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "M-0") nil)
     (define-key term-raw-map (kbd "M-1") nil)
     (define-key term-raw-map (kbd "M-2") nil)
     (define-key term-raw-map (kbd "M-3") nil)
     (define-key term-raw-map (kbd "M-4") nil)
     (define-key term-raw-map (kbd "M-5") nil)
     (define-key term-raw-map (kbd "M-6") nil)
     (define-key term-raw-map (kbd "M-7") nil)
     (define-key term-raw-map (kbd "M-8") nil)
     (define-key term-raw-map (kbd "M-9") nil)))
