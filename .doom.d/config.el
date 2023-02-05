;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Maciej Bendkowski"
      user-mail-address "maciej.bendkowski@gmail.com")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "Hasklug Nerd Font" :size 20)
      doom-unicode-font doom-font
      doom-font-increment 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq treemacs-follow-mode 't)

;; LSP breakcrumbs
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-headerline-breadcrumb-icons-enable nil)

(setq lsp-signature-auto-activate t
      lsp-signature-doc-lines 1
      lsp-ui-doc-enable 1)

;; LSP with Haskell
(setq lsp-haskell-server-path "haskell-language-server-wrapper"
      lsp-haskell-formatting-provider "fourmolu")

;; wrap lines at 80 characters in all modes.
(setq-default fill-column 80)
(+global-word-wrap-mode +1)

(after! projectile
  (setq projectile-project-root-files-bottom-up (remove ".git"
          projectile-project-root-files-bottom-up))

  (setq projectile-project-search-path
        '( ("~/code/") "~/.config/"))

  (setq projectile-ignored-projects '("~/"))
  (setq projectile-enable-caching t))

(setq auth-sources '("~/.authinfo.gpg"))

(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines))

;; deferr LSP in haskell-mode so to avoid race conditions with direnv.
(use-package! lsp-mode
  :hook (haskell-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "/\\.git$")
  (add-to-list 'lsp-file-watch-ignored-directories "/\\.spago$")
  (add-to-list 'lsp-file-watch-ignored-directories "/frontend$"))

;; convenient navigation and editing
(map! "M-r" #'evil-multiedit-match-all)
(evil-define-key 'normal 'global
  "H" "^"   ;; first character in line
  "L" "g_"  ;; last character in line
  )

;; global beacon minor-mode
(use-package! beacon)
(after! beacon (beacon-mode 1))

;; org-alert
(use-package org-alert
  :ensure t
  :custom (alert-default-style 'notifications)
  :config
  (setq org-alert-interval 300
        org-alert-notification-title "org-alert")
  (org-alert-enable))

;; LaTex C-c C-x C-l
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

;; toogle emphasis markers in org-mode
(setq org-hide-emphasis-markers t)
(defun org-toogle-emphasis ()
  "Toogle showing emphasis markers in org-mode"
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))
(define-key org-mode-map (kbd "C-c e") 'org-toogle-emphasis)

;; hoogle integration
(defun haskell-hoogle-local-lookup ()
  "Query local hoogle server"
  (interactive)
  (if (haskell-hoogle-server-live-p)
      (haskell-hoogle-lookup-from-local)
    (progn
      (haskell-hoogle-start-server)
      (haskell-hoogle-lookup-from-local))))

(map! :leader
      (:prefix ("s" . "search")
       :desc "Query local hoogle server"
       "h" #'haskell-hoogle-local-lookup))

(map! :leader
      :desc "Restart LSP workspace"
      "l r" #'lsp-workspace-restart)

(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_GB")

;; yasnippets
(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook
          (lambda () (yas-activate-extra-mode 'fundamental-mode)))

(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

(defun replace-imports () (interactive)
 "Replace import block with a -ddump-minimal-imports generated one"
 (haskell-navigate-imports)
 (while (not (haskell-navigate-imports-after-imports-p)) (kill-line))
 (open-line 1)
 (insert-file-contents
        (concat (haskell-cabal-find-dir)
                "dist-newstyle/build/x86_64-linux/ghc-9.2.1/"
                (haskell-cabal-get-field "name") "-"
                (haskell-cabal-get-field "version") "/build/"
                (haskell-guess-module-name-from-file-name
                 (buffer-file-name)) ".imports"))
 )

;; KMonad configuration files
(use-package! kbd-mode)

;; Use org-alert
(use-package org-alert
  :ensure t
  :custom (alert-default-style 'notifications)
  :config
  (setq org-alert-interval 300
        org-alert-notification-title "Reminder")
  (org-alert-enable))

;; Wingman actions
(map! :leader
      (:prefix ("l" . "LSP")
       :desc "Lens lookup"
       "l" #'lsp-avy-lens))

;; color code snippets
(use-package! color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

;; parentheses
(map! :leader
      (:prefix-map ("c" . "code")
        (:prefix ("p" . "parentheses")
                :desc "wrap round" "p" #'sp-wrap-round ;; shortcut
                :desc "wrap round" "(" #'sp-wrap-round
                :desc "wrap square" "[" #'sp-wrap-square
                 )))
