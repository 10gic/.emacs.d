
(setq auto-mode-alist
      (append '(("\\.jj$" . java-mode) ;javacc grammar file
                ("\\.jjt" . java-mode) ;javacc jjtree file
                )
              auto-mode-alist))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2)))



