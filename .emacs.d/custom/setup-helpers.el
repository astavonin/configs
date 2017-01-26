(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun search-word-under-cursor (arg)
    (interactive "p")
    (save-excursion
	(setq string
	      (if (and transient-mark-mode mark-active)
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (thing-at-point 'symbol)))
	(occur string)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key [f12] 'indent-buffer)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "M-s") 'search-word-under-cursor)
(global-set-key (kbd "S-M-C-v") 'scroll-other-window-down)
(global-set-key (kbd "C-c d s") 'desktop-save)
(global-set-key (kbd "C-c d l") 'desktop-read)
(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "S-C-o") 'vi-open-line-above)
(global-set-key [f4] 'next-error)
(global-set-key [S-f4] 'previous-error)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(provide 'setup-helpers)
