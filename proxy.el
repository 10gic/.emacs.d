;; Comment out it if you don't need proxy
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "localhost:1087")
     ("https" . "localhost:1087")))
