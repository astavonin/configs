(require 'company)
(require 'semantic)

(use-package semantic
  :ensure t
  :config
  (require 'semantic)
  (require 'semantic/bovine/gcc)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (semantic-mode 1))


(use-package company
  :ensure t
  :config
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-dabbrev-downcase nil)
  (setq company-backends '(company-c-headers company-files
                                             (company-gtags company-dabbrev-code)
                                             (company-dabbrev)
                                             ))
  (global-set-key [C-tab] 'company-complete)
  (global-company-mode))


(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-hook 'c-mode-hook (lambda ()
        (setq company-c-headers-path-system (semantic-gcc-get-include-paths "c"))))
  (add-hook 'c++-mode-hook (lambda ()
        (setq company-c-headers-path-system (semantic-gcc-get-include-paths "c++")))))


(require 'yasnippet)
(yas-global-mode 1)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(require 'helm-company)

(provide 'setup-complete)
