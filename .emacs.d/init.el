;--- Global Constants ----------------------------------------------------------
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
;-------------------------------------------------------------------------------

;--- Initial Fixes -------------------------------------------------------------
;;; Fix $HOME on Windows
;; $HOME isn't normally defined on Windows, but many unix tools expect it.
(when IS-WINDOWS
  (when-let (realhome
             (and (null (getenv-internal "HOME"))
                  (getenv "USERPROFILE")))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))
;-------------------------------------------------------------------------------

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
  :config (evil-mode 1)
  :demand t
  :preface
  (setq evil-ex-search-vim-style-regexp t
	evil-ex-visual-char-range t  ; custom range for ex commands
	evil-mode-line-format 'nil
	;; More vim like behavior
	evil-symbol-word-search t
	;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
	;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system 'undo-redo))

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

;--- Editor Settings -----------------------------------------------------------
(setq auto-mode-alist
      (append
       '(
	 ("\\.js$" . js-jsx-mode)
	 ("\\.jsx$" . js-jsx-mode)
	 ("\\.ts$" . typescript-mode)
	 ("\\.tsx$" . typescript-mode))
       auto-mode-alist))

(autoload 'typescript-mode "typescript-mode" "TypeScript mode" t)
;-------------------------------------------------------------------------------

;--- Editorconfig --------------------------------------------------------------
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))
;-------------------------------------------------------------------------------

;--- Python --------------------------------------------------------------------
(use-package py-isort
  :straight t
  :hook
  (before-save-hook . py-isort-before-save))

(use-package blacken
  :straight t
  :hook
  (python-mode-hook . blacken-mode))
;-------------------------------------------------------------------------------

;--- JavaScript/TypeScript -----------------------------------------------------
(use-package prettier-js
  :straight t
  :hook
  (js-jsx-mode-hook . prettier-js-mode)
  (js2-mode-hook . prettier-js-mode)
  (typescript-mode-hook . prettier-js-mode)
  (web-mode-hook . prettier-js-mode))

(use-package typescript-mode
  :straight t)
;-------------------------------------------------------------------------------

;--- Tree-Sitter ---------------------------------------------------------------
(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(python "https://github.com/tree-sitter/tree-sitter-python")))
;-------------------------------------------------------------------------------
