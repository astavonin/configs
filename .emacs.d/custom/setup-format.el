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
    (c-add-style "My Coding Style" bws-c-style t)))
(add-hook 'c++-mode-hook 'lconfig-c-mode)
(add-hook 'c-mode-hook 'lconfig-c-mode)

(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(provide 'setup-format)
