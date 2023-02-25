;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Maciej Bendkowski"
      user-mail-address "maciej.bendkowski@gmail.com")

(setq auth-sources '("~/.authinfo.gpg"))

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

;; wrap lines at 100 characters in all modes.
(setq-default fill-column 100)
(+global-word-wrap-mode +1)

(setq doom-font (font-spec :family "Hasklug Nerd Font" :size 18)
      doom-unicode-font doom-font
      doom-font-increment 1)

;; dashboard
(setq fancy-splash-image "~/.config/doom-icons/emacs.svg")
(assoc-delete-all "Open documentation" +doom-dashboard-menu-sections)
(add-to-list '+doom-dashboard-menu-sections
        '("Add journal entry"
          :icon (all-the-icons-octicon "calendar"
                                       :face 'doom-dashboard-menu-title)
          :when (featurep! :lang org +journal)
          :face (:inherit (doom-dashboard-menu-title bold))
          :action org-journal-new-entry))

;; Spell-checking
(setq ispell-program-name "aspell"
      ispell-dictionary "en_GB")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-return-follows-link t
      org-use-speed-commands t
      org-deadline-warning-days 30
      org-agenda-tags-column 75)

(setq org-journal-dir "~/org/journal"
      org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%a, %d-%m-%Y"
      org-journal-time-prefix "* "
      org-journal-file-format "%Y-%m-%d.org")

(setq org-tag-alist
  '(("@home" . ?H)
    ("@work" . ?W)))

(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/todo.org") "* TODO :@work: %?\n %i\n %a")
        ("p" "Personal" entry (file "~/org/personal.org") "* TODO :@home: %?\n %i\n %a")
        ))

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "|" "WAIT(w)" "BACK(b)")))

(setq org-agenda-include-diary t)

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda ""
                ((org-agenda-span 'week)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled tasks")
                 (org-agenda-files '("~/org/todo.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Personal tasks")
                 (org-agenda-files '("~/org/personal.org"))))
           ))))

(map! :n "L" #'evil-end-of-line
      :n "H" #'evil-first-non-blank
      :n "C-a" #'evil-numbers/inc-at-pt
      :n "C-x" #'evil-numbers/dec-at-pt
      :n "RET" #'+fold/toggle)

(map! :leader
      :desc "Flycheck next error" "c n" #'flycheck-next-error
      :desc "Flycheck previous error" "c N" #'flycheck-previous-error
      :desc "Flycheck verify setup" "c v" #'flycheck-verify-setup
      :desc "Flycheck select checker" "c S" #'flycheck-select-checker)

(map! :leader
      :desc "Wrap parens" "c p" #'sp-wrap-round
      :desc "Wrap brackets" "c [" #'sp-wrap-square)

;; evil-multiedit
;; cf. https://github.com/hlissner/evil-multiedit
(map! "M-r" #'evil-multiedit-match-all)

;; Haskell setup
(defun +my/haskell-setup ()
  (setq lsp-haskell-plugin-tactics-config-timeout-duration 2)
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  (setq haskell-mode-stylish-haskell-path "fourmolu")
  (setq lsp-haskell-formatting-provider "fourmolu")
  (setq flycheck-haskell-hlint-executable "hlint")
  (after! flycheck-mode
    (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc))
  (add-hook 'lsp-after-initialize-hook
            (lambda () (flycheck-add-next-checker 'lsp 'haskell-hlint)))
  (set-formatter! 'cabal-fmt "cabal-fmt" :modes '(haskell-cabal-mode))
  (setq flycheck-hlintrc ".hlint.yaml")
  ;; LSP's hlint doesn't respect config file
  (setq lsp-haskell-hlint-on nil))

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
      :desc "Query local hoogle server" "s h" #'haskell-hoogle-local-lookup
      :desc "Restart LSP workspace" "l r" #'lsp-workspace-restart
      :desc "Lens lookup" "l l" #'lsp-avy-lens) ;; lookup wingman actions

(add-hook! '(haskell-mode-hook haskell-literate-mode-hook) #'+my/haskell-setup)

;; NOTE: Is not available outside of LSP.
;; will define elisp functions for the given lsp code actions, prefixing the
;; given function names with "lsp"
(defmacro lsp-make-interactive-code-action (func-name code-action-kind)
  "Define an interactive function FUNC-NAME that attempts to
execute a CODE-ACTION-KIND action."
  `(defun ,(intern (concat "lsp-" (symbol-name func-name))) ()
     ,(format "Perform the %s code action, if available." code-action-kind)
     (interactive)
     ;; Even when `lsp-auto-execute-action' is nil, it still makes sense to
     ;; auto-execute here: the user has specified exactly what they want.
     (let ((lsp-auto-execute-action t))
       (condition-case nil
           (lsp-execute-code-action-by-kind ,code-action-kind)
         (lsp-no-code-actions
          (when (called-interactively-p 'any)
            (lsp--info ,(format "%s action not available" code-action-kind))))))))

(lsp-make-interactive-code-action wingman-fill-hole "refactor.wingman.fillHole")
(lsp-make-interactive-code-action wingman-case-split "refactor.wingman.caseSplit")
(lsp-make-interactive-code-action wingman-refine "refactor.wingman.refine")
(lsp-make-interactive-code-action wingman-split-func-args "refactor.wingman.spltFuncArgs")
(lsp-make-interactive-code-action wingman-use-constructor "refactor.wingman.useConstructor")

;; Agda setup FIXME: Seemingly not working
;; (add-hook 'agda2-mode-hook (lambda ()
;;   (activate-input-method "Agda")))

;; quick jumps
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

;; (ya)snipets
(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda ()
   (yas-activate-extra-mode 'fundamental-mode)))

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

;; projectile settings
(after! projectile

  (setq projectile-project-search-path
        '( ("~/code/") "~/.dotfiles/"))

  (setq projectile-enable-caching t))

;; column indicator
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setq-default
 whitespace-line-column 100
 whitespace-style '(face lines))

;; deferr LSP in haskell-mode so to avoid race conditions with direnv.
(use-package! lsp-mode
  :hook (haskell-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

;; do not watch .git folders in LSP.
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "/\\.git$"))

;; org-agenda notifications
(use-package org-alert
  :ensure t
  :custom (alert-default-style 'notifications)
  :config
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10
        org-alert-notification-title "Reminder")
  (org-alert-enable))

;; LaTex C-c C-x C-l
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

;; KMonad configuration files
(use-package! kbd-mode)

;; open in external applications
(use-package! openwith)
(openwith-mode t)

(setq openwith-associations
      '( ("\\.pdf\\'" "evince" (file))
         ("\\.mp4\\'" "mpv" (file)) )
      )
