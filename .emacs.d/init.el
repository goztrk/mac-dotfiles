;--- Bootstrap Straight.el Package Manager --------------------------------------
; Source: https://github.com/radian-software/straight.el
; Desc:   I think this is better than MELPA because you can pin the commit hash
;         and prevent updating it and causing problems.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; Enable use-package
(straight-use-package 'use-package)
;-------------------------------------------------------------------------------

;--- Global Options ------------------------------------------------------------
(global-display-line-numbers-mode 1)  ; Show line numbers
;-------------------------------------------------------------------------------

;--- Evil Mode -----------------------------------------------------------------
; Source: https://github.com/emacs-evil/evil
; Desc:   I can't live without vim keys. I even use them in VSCode
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)  ; This is optional since default is `t`
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

; Show evil mode state in mode-line
(setq evil-normal-state-tag   (propertize " COMMAND " 'face '((:background "dark khaki" :foreground "black")))
      evil-emacs-state-tag    (propertize "  EMACS  " 'face '((:background "turquoise" :foreground "black")))
      evil-insert-state-tag   (propertize " ------- " 'face '((:background "dark sea green" :foreground "black")))
      evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "dark orange" :foreground "black")))
      evil-motion-state-tag   (propertize "  MOTION " 'face '((:background "khaki" :foreground "black")))
      evil-visual-state-tag   (propertize "  VISUAL " 'face '((:background "light salmon" :foreground "black")))
      evil-operator-state-tag (propertize " OPERATE " 'face '((:background "sandy brown" :foreground "black"))))

; Change cursor shape on VIM modes in terminal mode
; Source: https://github.com/7696122/evil-terminal-cursor-changer
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :straight t
    :config
    (evil-terminal-cursor-changer-activate))
  )
;-------------------------------------------------------------------------------

;--- Visual Theme Packages -----------------------------------------------------
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1))
;-------------------------------------------------------------------------------

;--- Editorconfig --------------------------------------------------------------
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))
;-------------------------------------------------------------------------------

