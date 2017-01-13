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


;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'org)
(require 'ob-tangle)
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(org-babel-load-file (expand-file-name "loader.org" init-dir))
