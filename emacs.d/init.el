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

(load-file "~/.emacs.d/lib/sensible-defaults.el")

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
 '(cider-boot-parameters "repl -s -H :: wait dev")
  '(cider-cljs-boot-repl
     "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))")
  '(cider-cljs-lein-repl
     "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))" t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
  '(custom-safe-themes
     (quote
       ("83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "8e0d1b2e3e46a0d8dc8fca891af8dbe872b73b9f49eff5cbdf11e9b65455d277" "96005f97499f0549f921f81588f190f189b7acb8bbebbcbb9033cdd340118f80" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
  '(highlight-symbol-colors
     (--map
       (solarized-color-blend it "#fdf6e3" 0.25)
       (quote
         ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
  '(highlight-tail-colors
     (quote
       (("#eee8d5" . 0)
         ("#B4C342" . 20)
         ("#69CABF" . 30)
         ("#69B7F0" . 50)
         ("#DEB542" . 60)
         ("#F2804F" . 70)
         ("#F771AC" . 85)
         ("#eee8d5" . 100))))
  '(hl-bg-colors
     (quote
       ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
  '(hl-fg-colors
     (quote
       ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
  '(hl-paren-colors
     (quote
       ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(magit-diff-use-overlays nil)
  '(nrepl-message-colors
     (quote
       ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
  '(package-selected-packages
     (quote
       (powerline rspec-mode pdf-tools org-present epresent less-css-mode inf-clojure evil-visualstar slime haskell-mode ag 0blayout inf-ruby clj-refactor tagedit smex projectile paredit ido-ubiquitous exec-path-from-shell evil-surround evil-magit editorconfig clojure-mode-extra-font-locking cider arjen-grey-theme ace-window)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
  '(vc-annotate-color-map
     (quote
       ((20 . "#dc322f")
         (40 . "#c85d17")
         (60 . "#be730b")
         (80 . "#b58900")
         (100 . "#a58e00")
         (120 . "#9d9100")
         (140 . "#959300")
         (160 . "#8d9600")
         (180 . "#859900")
         (200 . "#669b32")
         (220 . "#579d4c")
         (240 . "#489e65")
         (260 . "#399f7e")
         (280 . "#2aa198")
         (300 . "#2898af")
         (320 . "#2793ba")
         (340 . "#268fc6")
         (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
  '(weechat-color-list
     (quote
       (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
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
