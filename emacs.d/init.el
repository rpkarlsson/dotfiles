;; Packages

;; emacs -q wont start correctly without:
;; turning this into a real config should make it possible
;; to remove the following.
(package-initialize)
(run-hooks after-init-hook)


(require 'package)

(when (not package-archive-contents)
  (package-refresh-contents))


(defvar my-packages
  '(ace-window
     cider
     clojure-mode
     clojure-mode-extra-font-locking
     editorconfig
     evil
     evil-magit
     evil-surround
     evil-collection
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

(require 'org)
;(require 'org-install)
;(require 'ob-tangle)
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(org-babel-load-file (expand-file-name "loader.org" init-dir))
(put 'dired-find-alternate-file 'disabled nil)
