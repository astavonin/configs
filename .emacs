;;; >>>>>>>>>>> Глобальные настройки >>>>>>>>>>>>>>>

(setq make-backup-files nil)
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

;; <<<<<<<<<<<< Глобальные настройки <<<<<<<<<<<<<<


;; >>>>>>>>>>> Установка пакетов >>>>>>>>>>>>>>>
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defvar my-packages '(color-theme
    cmake-mode
    auto-complete
    auto-indent-mode
    rust-mode
    flycheck
    cider
))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (when (not (package-installed-p p))
      (when (not (package-installed-p p))
	(when (not (package-installed-p p))
	  (package-install p))))))

(when (not package-archive-contents)
  (package-refresh-contents))

;; <<<<<<<<<<<< Установка пакетов <<<<<<<<<<<<<<


;; ===============================================
;; Настройки пакетов
;; ===============================================


;; >>>>>>>>>>> Автодополнение >>>>>>>>>>>>>>>

(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
		       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))
		       ))
(real-global-auto-complete-mode t)

;; <<<<<<<<<<<< Автодополнение <<<<<<<<<<<<<<


;; >>>>>>>>>>> FlyCheck >>>>>>>>>>>>>>>

(require 'flycheck)

;; <<<<<<<<<<<< FlyCheck <<<<<<<<<<<<<<

;; >>>>>>>>>>> IDO >>>>>>>>>>>>>>>

(require 'ido)
(ido-mode t)
(ido-mode 'both) ;; for buffers and files
(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" "\*Completions\*" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "\*GNU Emacs\*" "\*scratch\*"
   "\*Messages\*" "\*buffer-selection\*" "\*Flycheck.*")
 ido-work-directory-list '("~/" "~/Projects")
 ido-case-fold  t                 ; be case-insensitive

 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)

 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-montz)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; <<<<<<<<<<<< IDO <<<<<<<<<<<<<<

(require 'cider)
(add-to-list 'exec-path "/usr/local/bin/")
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; ===============================================
;; Вспомогательные функции и биндинги
;; ===============================================

;; >>>>>>>>>>> Дополнительные функции >>>>>>>>>>>>>>>

(flycheck-define-checker servo-rust
  "A Rust syntax checker using the Rust compiler in Servo."
  :command ("/usr/local/bin/rustc"
            "--parse-only"
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": "
          (one-or-more digit) ":" (one-or-more digit) " error: "
          (message) line-end))
  :modes rust-mode)

(add-hook 'rust-mode-hook (lambda () (flycheck-select-checker 'servo-rust)
			    (flycheck-mode)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (flycheck-mode)))


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

(defun kill-current-line (&optional n)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))

(defun search-word-under-cursor (arg)
    (interactive "p")
    (save-excursion
	(setq string
	      (if (and transient-mark-mode mark-active)
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (thing-at-point 'symbol)))
	(occur string)))


;; <<<<<<<<<<<< Дополнительные функции <<<<<<<<<<<<<<


;; >>>>>>>>>>> Клавиатурные сокращения >>>>>>>>>>>>>>>

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "M-s") 'search-word-under-cursor)
(global-set-key (kbd "S-M-C-v") 'scroll-other-window-down)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c d s") 'desktop-save)
(global-set-key (kbd "C-c d l") 'desktop-read)
(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "S-C-o") 'vi-open-line-above)
(global-set-key (kbd "C-k") 'kill-current-line)

;; <<<<<<<<<<<< Клавиатурные сокращения <<<<<<<<<<<<<<

