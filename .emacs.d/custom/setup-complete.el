(require 'auto-complete-config)
(ac-config-default)

(setq-default
 ac-sources '(
              ac-source-words-in-all-buffer
              ac-source-words-in-buffer
              ac-source-files-in-current-dir
              )
 )
(defun auto-complete-mode-maybe ()
  "Overwrite auto-complete-mode-maybe which by defaults turns autocomplete only"
  "on for buffers listed in ac-modes."
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))
(setq  ac-use-fuzzy t)
(setq ac-ignore-case (quote smart))
(global-auto-complete-mode t)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(setq ac-delay 0.0)

(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-gtags
                                        ac-source-words-in-all-buffer))))

(require 'yasnippet)
(yas-global-mode 1)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(provide 'setup-complete)
