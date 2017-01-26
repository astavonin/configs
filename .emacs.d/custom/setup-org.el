(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(setq org-support-shift-select 'always)
(setq org-confirm-babel-evaluate nil)

(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8053/libexec/plantuml.jar")

(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(provide 'setup-org)
