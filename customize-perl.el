(defalias 'perl-mode 'cperl-mode) ;设置默认使用cperl-mode代替perl-mode

(add-hook 'cperl-mode-hook 'imenu-add-menubar-index) ;打开cperl-mode的index菜单

(add-hook 'cperl-mode-hook
          (function (lambda ()
                      ;only work in GUI.
                      (local-set-key [f1] 'cperl-perldoc-at-point))))
