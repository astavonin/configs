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
                      projectile
                      helm-projectile
                      comment-dwim-2
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

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'setup-helm)
(require 'setup-project)
(require 'setup-global-view)
(require 'setup-complete)
(require 'setup-format)
(require 'setup-helpers)
(require 'setup-vcc)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "./adlm-make.sh debug build /Users/astavonin/projects/AdLM/develop/global/src" projectile-compilation-cmd-map))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
