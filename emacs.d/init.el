(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;;
;; Packages
;;
(use-package better-defaults
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit
  :ensure t
  :after (magit evil))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package magit
  :ensure t)

(use-package ob-shell
  :after evil)

(use-package org
  :ensure org-plus-contrib
  :config)


(use-package paredit
  :ensure t)

;;
;; Backups
;;
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default nil)

;;
;; Locale
;;
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;
;; Lisp
;;
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;eldoc-mode shows documentation in the minibuffer when writing code http://www.emacswiki.org/emacs/ElDoc

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;
;; GUI
;;
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(set-face-attribute 'default nil :height 140)
(fset 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

;; Save clipboard strings into kill ring before replacing them. When one selects something in another program to paste it into Emacs, but kills something in Emacs before actually pasting it, this selection is gone unless this variable is non-nil
(setq save-interprogram-paste-before-kill t)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
;; (setq ido-use-virtual-buffers t)

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

;; Keys
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun rk-init.el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-plus-contrib evil-magit ido-completing-read+ ido-vertical-mode magit better-defaults evil-surround evil-collection 0blayout intero haskell-mode paredit use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
