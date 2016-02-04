;; 默认M-x compiler时，make命令会提取下面环境变量。
(setenv "CFLAGS" "-ggdb3 -Wall")
(setenv "CXXFLAGS" "-ggdb3 -Wall")


;; Semantic Refactor is a C/C++ refactoring tool based on Semantic parser framework.
;; https://github.com/tuhdo/semantic-refactor
;; https://github.com/10gic/semantic-refactor
(require 'srefactor)
(semantic-mode 1) ;; this is needed by srefactor
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)


(add-hook 'c-mode-hook 'imenu-add-menubar-index) ;打开c-mode的index菜单
(add-hook 'c++-mode-hook 'imenu-add-menubar-index) ;打开c++-mode的index菜单

(setq-default c-basic-offset 4  ;调整缩进时，默认4个空格
              tab-width 4)      ;显示tab为4个空格
