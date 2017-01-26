(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(color-theme
                      cmake-mode
                      markdown-mode
                      ggtags
                      helm
                      haskell-mode
                      ghc
                      hindent
                      helm-gtags
                      helm-company
                      helm-ag
                      yasnippet
                      clang-format
                      fill-column-indicator
                      magit
                      projectile
                      helm-projectile
                      comment-dwim-2
                      cedet
                      use-package
                      todotxt
                      company
                      company-go
                      company-ghc
                      exec-path-from-shell
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

(exec-path-from-shell-initialize)
(setenv "GOPATH" (exec-path-from-shell-copy-env "GOPATH"))
(setenv "GOROOT" (exec-path-from-shell-copy-env "GOROOT"))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(require 'setup-helm)
(require 'setup-global-view)
(require 'setup-complete)
(require 'setup-format)
(require 'setup-helpers)
(require 'setup-vcc)
(require 'setup-project)
