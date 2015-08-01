;; 默认M-x compiler时，make命令会提取下面环境变量。
(setenv "CFLAGS" "-ggdb3 -Wall")
(setenv "CXXFLAGS" "-ggdb3 -Wall")


(add-hook 'c-mode-hook 'imenu-add-menubar-index) ;打开c-mode的index菜单
(add-hook 'c++-mode-hook 'imenu-add-menubar-index) ;打开c++-mode的index菜单

(setq-default c-basic-offset 4  ;调整缩进时，默认4个空格
              tab-width 4)      ;显示tab为4个空格
