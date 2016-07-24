(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(color-theme
		      cmake-mode
		      ggtags
		      helm
		      helm-gtags
		      helm-ag
		      auto-complete
		      yasnippet
		      clang-format
		      fill-column-indicator
		      magit
		      ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-helm)
(require 'setup-global-view)
(require 'setup-complete)
(require 'setup-format)
(require 'setup-helpers)
(require 'setup-vcc)
