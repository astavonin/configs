(require 'clang-format)

(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

(setq bws-c-style
      '((c-basic-offset                 . 4)
        (indent-tabs-mode               . nil)
        (c-tab-always-indent            . t)
        (c-offsets-alist                . (
                                           (innamespace . -)
                                           (access-label . /)
                                           (defun-block-intro . +)
                                           (substatement-open . 0)
                                           (inline-open . 0)
                                           (arglist-cont .(c-lineup-arglist-operators 0))
                                           (arglist-cont-nonempty . c-lineup-argcont)
                                           (arglist-cont-nonempty . (c-lineup-arglist-operators c-lineup-arglist))
                                           (arglist-close . (c-lineup-arglist-close-under-paren))
                                           (comment-intro . +)
                                           (case-label . +)
                                           )
                                        )
        (hs-special-modes-alist         . (

                                           (c++-mode "#if" "#endif" "/[*/]" nil nil)
                                           (c++-mode "{" "}" "/[*/]" nil nil)
                                           )
                                        )
        (c-cleanup-list                 . (
                                           scope-operator
                                           empty-defun-braces
                                           defun-close-semi
                                           list-close-comma
                                           )
                                        )
        )
      )

(defun lconfig-c-mode ()
  (progn 
    (local-set-key (kbd "<f12>") 'clang-format-buffer)
    (c-add-style "My Coding Style" bws-c-style t)))
(add-hook 'c++-mode-hook 'lconfig-c-mode)
(add-hook 'c-mode-hook 'lconfig-c-mode)

(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  (setq tab-width 4)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "<f12>") 'gofmt)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(require 'todotxt)
(add-to-list 'auto-mode-alist '("\\todo.txt\\'" . todotxt-mode))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun my-haskell-mode-hook()
  (ghc-init)
  (local-set-key (kbd "<f12>") 'hindent-reformat-buffer))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(setq cmake-tab-width 4)

(provide 'setup-format)
