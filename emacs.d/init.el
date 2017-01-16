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
