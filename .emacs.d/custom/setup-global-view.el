(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(global-linum-mode t)
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 110))
(set-face-attribute 'default nil :height 200)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(setq inhibit-startup-message t)
(global-subword-mode 1)

(require 'whitespace)
(setq whitespace-style '(lines))
(global-whitespace-mode 1)
(setq whitespace-style '(face trailing tab-mark))

(require 'fill-column-indicator)
(setq fci-rule-width 2)
(setq fci-rule-column 80)
(setq fci-rule-color "darkblue")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-montz)

(provide 'setup-global-view)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [M-f1] 'show-file-name)

(setq compilation-scroll-output t)
