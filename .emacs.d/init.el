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
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(package-initialize)

(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/lib/")

(setenv "PATH" (concat (getenv "PATH") ":/home/rpkarlsson/bin"))
(setq exec-path (append exec-path '("/home/rpkarlsson/bin")))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;  (add-to-list 'load-path "<path where use-package is installed>")
;;  (add-to-list 'load-path "~/.emacs.d/elpa")
  (require 'use-package))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package better-defaults
  :ensure t)

(use-package clj-refactor
  :ensure t
  :config
  ;; (define-key input-decode-map [?\C-m] [C-m])
  ;; Do not treat C-m as RET
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package clojure-mode
  :ensure t
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t
  :pin melpa
  ;; :load-path "~/Projects/cider"
  :config
  (setq cider-repl-pop-to-buffer-on-connect t))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package company
  :ensure t
  :custom
  (cider-enhanced-cljs-completion-p nil)
  :config
  (global-company-mode)
  ;; (add-hook 'clojurescript-mode-hook (lambda () (setq company-idle-delay nil)))
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(use-package elfeed)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map  "\M-." 'nil))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package evil-magit
  :ensure t
  :after (magit evil))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Replaced witch Counsel, Ivy and Swiper
(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
;; (setq ido-use-virtual-buffers t)

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

(use-package flycheck-clj-kondo
  :ensure t)

;; (use-package ivy
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   (ivy-count-format "(%d/%d) ")
;;   (ivy-initial-inputs-alist nil)
;;   (ivy-fixed-height-minibuffer nil)
;;   (ivy-display-style 'fancy)
;;   (ivy-re-builders-alist
;;    '((counsel-projectile-ag . ivy--regex-plus))
;;    '((t . ivy--regex-fuzzy)))
;;   (ivy-mode 1))

;; (use-package ivy-posframe
;;   :ensure t
;;   :after ivy
;;   :custom
;;   (ivy-posframe-height-alist
;;    '((swiper . 15)
;;      (swiper-isearch . 15)
;;      (t . 10)))
;;   (ivy-posframe-display-functions-alist
;;    '((complete-symbol . ivy-posframe-display-at-point)
;;      (swiper . nil)
;;      (swiper-isearch . nil)
;;      (t . ivy-posframe-display-at-frame-center)))
;;   :config
;;   (ivy-posframe-mode 1))

;; (use-package counsel
;;   :ensure t
;;   :after ivy
;;   :bind
;;   (("C-s" . swiper)
;;    ("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("<f1> f" . counsel-describe-function)
;;    ("<f1> v" . counsel-describe-variable)
;;    ("<f1> l" . counsel-find-library)
;;    ("<f2> i" . counsel-info-lookup-symbol)
;;    ("<f2> u" . counsel-unicode-char)
;;    ("C-c c" . counsel-compile)
;;    ("C-c g" . counsel-git)
;;    ("C-c j" . counsel-git-grep)
;;    ("C-c k" . counsel-ag)
;;    ("C-x b" . ivy-switch-buffer) ;; counsel-ibuffer
;;    ("C-x l" . counsel-locate)
;;    ("C-S-o" . counsel-rhythmbox)
;;    ("C-c C-r" . ivy-resume)
;;    ("C-x C-d" . counsel-dired))
;;   :config
;;   (counsel-projectile-mode))

;; (use-package counsel-projectile
;;   :ensure t
;;   :after counsel)

;; (use-package prescient
;;   :ensure t
;;   :custom
;;   (prescient-history-length 50)
;;   (prescient-save-file "~/.emacs.d/prescient-items")
;;   (prescient-filter-method '(fuzzy initialism regexp))
;;   :config
;;   (prescient-persist-mode 1))

;; (use-package ivy-prescient
;;   :ensure t
;;   :after (prescient ivy)
;;   :custom
;;   (prescient-filter-method '(fuzzy regexp initialism))
;;   ;; (ivy-prescient-sort-commands
;;   ;;  '(:not swiper ivy-switch-buffer counsel-switch-buffer))
;;   (ivy-prescient-retain-classic-highlighting t)
;;   (ivy-prescient-enable-filtering t)
;;   (ivy-prescient-enable-sorting t)
;;   :config
;; ;; (defun prot/ivy-prescient-filters (str)
;; ;;   "Specify an exception for `prescient-filter-method'.

;; ;; This new rule can be used to tailor the results of individual
;; ;; Ivy-powered commands, using `ivy-prescient-re-builder'."
;; ;;     (let ((prescient-filter-method '(literal regexp)))
;; ;;       (ivy-prescient-re-builder str)))

;; ;;   (setq ivy-re-builders-alist
;; ;;         '((counsel-rg . prot/ivy-prescient-filters)
;; ;;           (counsel-grep . prot/ivy-prescient-filters)
;; ;;           (counsel-yank-pop . prot/ivy-prescient-filters)
;; ;;           (swiper . prot/ivy-prescient-filters)
;; ;;           (swiper-isearch . prot/ivy-prescient-filters)
;; ;;           (swiper-all . prot/ivy-prescient-filters)
;; ;;           (t . ivy-prescient-re-builder)))
;;   (ivy-prescient-mode 1))

(use-package magit
  :ensure t)

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package ob-shell
  :after evil)

(defun org-custom-keys ()
  (local-set-key (kbd "M-n") #'org-metadown)
  (local-set-key (kbd "M-p") #'org-metaup))

(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-duration-format (quote h:mm))
  (with-eval-after-load 'evil-maps
    (define-key org-mode-map "\M-j" #'org-metadown)
    (define-key org-mode-map "\M-k" #'org-metaup)
    (define-key org-mode-map "\M-h" #'org-metaleft)
    (define-key org-mode-map "\M-l" #'org-metaright)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-cc" 'org-capture))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (ruby . t)
     (scheme . t)
     (java . t)
     (clojure . t)
     (python . t)))
  (add-hook 'org-mode-hook (lambda ()
                             (auto-fill-mode 1)
                             (setq org-src-fontify-natively t)
                             (org-custom-keys)))
  (setq org-agenda-files (list "~/Documents/work.org" "~/Documents/home.org"))
  (setq org-capture-templates
        '(("t" "Personal Task" entry (file "~/Documents/home.org")
           "* TODO %?" :emptylines 1)
          ("A" "Appointment -  Home" entry (file+headline "~/Documents/home.org" "Appointments")
           "** %?\n   %T" :empty-lines 1)
          ("p" "Punch In" entry (file+headline "~/Documents/work.org" "log")
           "* %u \n %?\n\n" :clock-in t :clock-keep t :empty-lines 1)
          ("w" "Work-related Task" entry (file "~/Documents/work.org")
           "* TODO %?" :emptylines 1)
          ("a" "Appointment -  work" entry (file+headline "~/Documents/work.org" "Appointments")
           "** %?\n   %T" :empty-lines 1)
          ("b" "Bookmark" entry (file+headline "~/Documents/home.org" "Bookmarks")
           "* %?\n :PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
          ("n" "New note" entry (file+headline "~/Documents/work.org" "Refile")
           "" :empty-lines 1))))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package evil-org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package paredit
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'hybrid) ;; Use .projectile files
  (setq projectile-completion-system 'ivy))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-asdf
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-indentation
                 slime-mdot-fu
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-sbcl-exts
                 slime-scratch
                 slime-xref-browser))
  (slime-autodoc-mode)
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function
        'slime-fuzzy-complete-symbol))

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
;; Calendar
;;
(setq calendar-week-start-day 1)

; Add week numbers
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

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
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;
;; MISC
;;
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

(setq create-lockfiles nil)

(pdf-tools-install)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(setq ag-reuse-window 't
      frame-title-format "emacs – %b" ;; Put buffer name in titlebar
      initial-scratch-message nil)

(push '(" fn ") prettify-symbols-alist)
(global-prettify-symbols-mode +1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq cider-offer-to-open-cljs-app-in-browser nil)

(setq dired-listing-switches "-alh")

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)

;; Focus buffer
(defvar window-split-saved-config nil)
(defun window-split-toggle-one-window ()
  "Make the current window fill the frame.
  If there is only one window try reverting to the most recently saved
  window configuration."
  (interactive)
  (if (and window-split-saved-config (not (window-parent)))
      (set-window-configuration window-split-saved-config)
    (setq window-split-saved-config (current-window-configuration))
    (delete-other-windows)))

 (global-set-key (kbd "C-x 1") 'window-split-toggle-one-window)

;Nov.el
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)

;; Save clipboard strings into kill ring before replacing them. When one selects something in another program to paste it into Emacs, but kills something in Emacs before actually pasting it, this selection is gone unless this variable is non-nil
;; TODO: Do this what i want?
;; (setq save-interprogram-paste-before-kill t)

;; Keys
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun open-init.el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-work.org ()
  (interactive)
  (find-file "~/Documents/work.org"))

(defun rk-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

  (defun rk-create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(cider-enhanced-cljs-completion-p nil)
 '(company-idle-delay 1)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "53760e1863395dedf3823564cbd2356e9345e6c74458dcc8ba171c039c7144ed" "e62b66040cb90a4171aa7368aced4ab9d8663956a62a5590252b0bc19adde6bd" "d97baf5a34c87b05508739505cad03438cde8efa2a0d350c7773f2a8bc26a50d" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" default))
 '(elfeed-feeds
   '("https://feeds.therepl.net/therepl" "feeds.soundcloud.com/users/soundcloud:users:627190089/sounds.rss" "https://lispcast.com/feed/podcast/thoughts-functional-programming/" "https://clojuredesign.club/index.xml" "https://nullprogram.com/feed/" "https://planet.emacslife.com/atom.xml" "http://fetchrss.com/rss/5ce3c0e18a93f86d578b45675ce3c0a78a93f812558b4567.xml" "http://endlessparentheses.com/atom.xml" "http://insideclojure.org/feed.xml"))
 '(evil-collection-outline-bind-tab-p nil)
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#b3c34d" . 20)
     ("#6ccec0" . 30)
     ("#74adf5" . 50)
     ("#e1af4b" . 60)
     ("#fb7640" . 70)
     ("#ff699e" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-display-style 'fancy)
 '(ivy-fixed-height-minibuffer nil)
 '(ivy-initial-inputs-alist nil)
 '(ivy-mode 1)
 '(ivy-posframe-display-functions-alist
   '((complete-symbol . ivy-posframe-display-at-point)
     (swiper)
     (swiper-isearch)
     (t . ivy-posframe-display-at-frame-center)))
 '(ivy-posframe-height-alist '((swiper . 15) (swiper-isearch . 15) (t . 10)))
 '(ivy-prescient-enable-filtering t)
 '(ivy-prescient-enable-sorting t)
 '(ivy-prescient-retain-classic-highlighting t)
 '(ivy-re-builders-alist '((t . ivy--regex-fuzzy)) t)
 '(ivy-use-virtual-buffers t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("~/Documents/work.org" "~/Documents/home.org"))
 '(package-selected-packages
   '(tldr dumb-jump ripgrep centered-window company-lsp lsp-mode prescient ivy-posframe amx swiper-projectile counsel-projectile solarized-theme powerline org-bullets meson-mode counsel flx clojure-snippets company multi-term minions flycheck-clj-kondo emms-player-simple-mpv emms ob-mongo telephone-line project-explorer basic-theme minimal-theme evil-org-agenda org-evil evil-org cargo rust-mode go-mode buttercup pass clj-refactor elfeed ag request pdf-tools ace-window smex clojure-mode-extra-font-locking uniquify cider slime projectile nov org-plus-contrib evil-magit ido-completing-read+ ido-vertical-mode magit better-defaults evil-surround evil-collection 0blayout intero haskell-mode paredit use-package evil))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(prescient-history-length 50)
 '(prescient-save-file "~/.emacs.d/prescient-items")
 '(safe-local-variable-values
   '((projectile-switch-project-action lambda nil
                                       (message "Runs"))
     (projectile-switch-project-action . rk-projectile-find)
     (projectile-switch-project-action quote rk-projectile-find)
     (projectile-switch-project-action lambda nil
                                       (find-file "src/geoserver/repl.clj"))
     (projectile-switch-project-action lambda nil
                                       (projectile-find-file ".gitignore"))
     (projectile-current-project-in-switch lambda nil
                                           (projectile-find-file ".gitignore"))
     (org-static-blog-publish-title . "rpkn.se")
     (cider-default-cljs-repl . "shadow")
     (cider-preferred-build-tool 1)
     (cider-preferred-build-tool quote shadow-cljs)
     (cider-default-cljs-repl . "Shadow")
     (cider-preferred-build-tool shadow-cljs)
     (cider-preferred-build-tool 'shadow-cljs)
     (cider-preferred-build-tool "shadow-cljs")
     (cider-default-cljs-repl "Shadow")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
