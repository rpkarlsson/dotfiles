;; Install packages
(require 'package)
(add-to-list 'package-archives
                          '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ace-window
     arjen-grey-theme
     cider
     clojure-mode
     clojure-mode-extra-font-locking
     editorconfig
     evil
     evil-magit
     evil-surround
     ido-ubiquitous
     ido-vertical-mode
     magit
     org
     paredit
     projectile
     smex
     tagedit))


(if (eq system-type 'darwin)
  (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Load the remaining config specified in org-mode
(require 'org)
(require 'ob-tangle)
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(org-babel-load-file (expand-file-name "loader.org" init-dir))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages
     (quote
       (web-mode js2-mode htmlize ox-reveal less-css-mode flycheck-clojure flycheck ## ag exec-path-from-shell tagedit smex projectile paredit ido-ubiquitous evil-surround evil-magit editorconfig clojure-mode-extra-font-locking cider arjen-grey-theme ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-boot-parameters "repl -s -H :: wait dev")
  '(cider-cljs-boot-repl
     "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))")
  '(cider-cljs-lein-repl
     "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
  '(custom-safe-themes
     (quote
       ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" default)))
  '(package-selected-packages
     (quote
       (less-css-mode inf-clojure evil-visualstar spacemacs-theme solarized-theme slime haskell-mode ag 0blayout 2048-game inf-ruby clj-refactor tagedit smex projectile paredit ido-ubiquitous exec-path-from-shell evil-surround evil-magit editorconfig clojure-mode-extra-font-locking cider arjen-grey-theme ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
