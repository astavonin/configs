
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(setq ac-delay 0.0)

(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-gtags
					ac-source-words-in-all-buffer))))


(require 'yasnippet)
(yas-global-mode 1)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(provide 'setup-complete)
