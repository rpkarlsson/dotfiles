;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Robin Karlsson"
      user-mail-address "robinpkarlsson@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; (setq doom-theme 'nord-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(load! "../emacs-backup/lib/mail.el")

(after! org
  (setq org-duration-format (quote h:mm))
  (add-to-list 'org-capture-templates
               '("i" "Punch In" entry (file+headline "~/org/work.org" "log")
                  "* %u \n %?\n\n" :clock-in t :clock-keep t :empty-lines 1)))
(after! elfeed
  (setq elfeed-feeds '("https://clojuredesign.club/index.xml"
                       "https://corfield.org/atom.xml"
                       "https://drewdevault.com/feed.xml"
                       "https://feeds.transistor.fm/thoughts-on-functional-programming-podcast-by-eric-normand"
                       "https://www.michaelnygard.com/atom.xml"
                       "https://insideclojure.org/feed.xml")))

(use-package paredit
  :hook ((clojure-mode . enable-paredit-mode)
         (clojurescript-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)))

(defun jh/hours-a-day (date time)
  (cond
   ((string-equal time "0:01") "")
   ((string-match ".*]!" date) "")
   ((string-match "\\(Fri\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\)]" date) "8:00")
   (t "")))

(defun sort-in-paren ()
  (interactive)
  (when-let (p (show-paren--default))
    (sort-regexp-fields nil "\\(\\sw\\|\\s_\\)+" "\\&" (nth 0 p) (nth 2 p))))


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

(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                          :port 6697
                          :nick "rpkarlsson"
                          :sasl-username ,(+pass-get-user "irc/freenode.net")
                          :sasl-password (lambda (&rest _) (+pass-get-secret "irc/freenode.net"))
                          :channels ("#emacs"))))
