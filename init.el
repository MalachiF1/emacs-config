;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-result t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "17:30"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Store auto-save files in the var directory (default is directory of visited file)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes. Measured in bytes.
;; If you experience freezing, decrease this.
;; If you experience stuttering, increase this."
(setq gc-cons-threshold (* 134217728)) ; 128mb

(use-package gcmh
  :init (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 'auto ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024))) ; 16mb

;; Profile emacs startup
;(add-hook 'emacs-startup-hook
;          (lambda ()
;            (message "*** Eamcs loaded in %s with %d gargabe collections."
;                     (format "%.2f seconds"
;                             (float-time
;                              (time-subtract after-init-time before-init-time)))
;                     gcs-done)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  ;; split windows
  (define-key evil-normal-state-map (kbd "C-S-v") 'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-split)
  ;; move across splits
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)

  (setq evil-goggles-blocking-duration 0.050)
  (setq evil-goggles-async-duration 0.200)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
  (general-create-definer malachi/leader-keys
                          :keymaps '(normal visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC")
  (malachi/leader-keys
   "q" '(:ignore t :which-key "quit")
   "qq" '(kill-buffer-and-window :which-key "kill window & buffer")
   "qw" '(delete-window :which-key "delete window")
   "qb" '(kill-this-buffer :which-key "kill buffer")
   "w" '(save-buffer :which-key "save")
   "e" '(treemacs :which-key "treemacs")
   "t" '(:ignore t :which-key "toggle")))

(general-define-key "C-M-j" 'counsel-switch-buffer)

(use-package key-chord
  :init
  (setq key-chord-two-keys-delay 0.1)
  (setq key-chord-one-key-delay 0.2)
  :config
  (key-chord-mode 1))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-delay 0.5
        which-key-add-column-padding 1
        which-key-separator "  "
        which-key-prefix-prefix "+"
        which-key-unicode-correction 3
        which-key-show-prefix 'left))

(use-package hydra
  :defer t)

(setq inhibit-startup-screen t) ; Disable default emacs startup screen

(scroll-bar-mode -1)         ; Disable visible scrollbar
(tool-bar-mode -1)           ; Disable the toolbar
(tooltip-mode -1)            ; Disable tooltips
(set-fringe-mode 10)         ; Give some breathig room

(menu-bar-mode -1)           ; Disable the menue bar

(setq visible-bell t) ;; Set up the visible bell

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                treemacs-mode-hook
                neotree-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                dired-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(setq-default truncate-lines t) ; Disable line wraping

(setq bookmark-set-fringe-mark nil)
;; TODO: Change to this symbol - 

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)

(setq-default
  cua-mode t ; Hide the cursor in inactive windows.
  cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
  default-directory "~/"
  tab-width 4
  indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces.
  indent-line-function 'insert-tab)

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

;; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(global-font-lock-mode 1)             ; Use font-lock everywhere.
(setq font-lock-maximum-decoration t) ; We have CPU to spare; highlight all syntax categories.

;; Font size
(defvar malachi/default-font-size 100)
(defvar malachi/default-variable-font-size 120)

;; Doom Emacs Code

(defvar +bidi-mode-map (make-sparse-keymap)
  "Keymap for `+bidi-mode'.")

(defvar +bidi-hebrew-font (font-spec :family "Open Sans" :weight 'semi-light)
  "Overriding font for hebrew script.
   Must be a `font-spec', see `doom-font' for examples.
   WARNING: if you specify a size for this font it will hard-lock any usage of this
   font to that size. It's rarely a good idea to do so!")

(defface +bidi-hebrew-face `((t :font ,+bidi-hebrew-font)) "")

(defcustom +bidi-want-smart-fontify t
  "Use bidi override fonts on surrounding space and punctuation as well.
   Add `+bidi-smart-fontify-keywords' to `font-lock-keywords' on editable buffers
   when `+bidi-mode' is on."
  :type 'boolean)

(defvar +bidi-smart-fontify-keywords
  `((,(rx (any (#x0590 . #x05FF))       ; Hebrew
          (group (one-or-more (any " " punctuation))))
     (1 '+bidi-hebrew-face t)))

  "`font-lock' keywords matching spaces and punctuation after RTL characters.
   See the variable `font-lock-keywords' for information on the format.")

(defcustom +bidi-paragraph-direction nil
  "The value of `bidi-paragragh-direction' when `+bidi-mode' is on.
   See the `bidi-paragraph-direction' for more info.
   Warning: do not change this if you are using `+bidi-global-mode'.'"
  :type '(choice
          (const :tag "Left to Right" left-to-right)
          (const :tag "Right to Left" right-to-left)
          (const :tag "Dynamic, according to paragraph text" nil)))

   ;;;###autoload
(define-minor-mode +bidi-mode
  "Minor mode for using bidirectional text in a buffer.
   Note that the whole buffer doesn't have to contain any
   bidirectional text at all, this mode just makes bidi editing
   easier."
  :keymap +bidi-mode-map
  (if +bidi-mode
      (progn
        (setq bidi-paragraph-direction +bidi-paragraph-direction   ; Better paragraph alignment
              bidi-paragraph-separate-re "^" ; No need for empty lines to switch alignment
              bidi-paragraph-start-re "^"    ; ^
              bidi-inhibit-bpa nil)          ; Better bidi paren logic
        (when (and +bidi-want-smart-fontify
                   (not buffer-read-only))
          (font-lock-add-keywords
           nil
           +bidi-smart-fontify-keywords
           'append)
          (font-lock-flush)))
    (setq bidi-paragraph-direction 'left-to-right
          bidi-paragraph-separate-re nil
          bidi-paragraph-start-re nil
          bidi-inhibit-bpa t)
    (when (and +bidi-want-smart-fontify
               (not buffer-read-only))
      (font-lock-remove-keywords
       nil
       +bidi-smart-fontify-keywords)
      (font-lock-flush))))

(define-globalized-minor-mode +bidi-global-mode +bidi-mode +bidi-mode)

(defun +bidi-set-fonts-h ()
  (set-fontset-font t 'hebrew +bidi-hebrew-font)
  (set-face-font '+bidi-hebrew-face +bidi-hebrew-font))

(add-hook 'after-setting-font-hook
  (+bidi-set-fonts-h))

;; My Configuration Choice
(set-input-method 'rfc1345) ; Default
(+bidi-global-mode 1)
;(+bidi-set-fonts-h)

(defun malachi/change-language-to-hebrew ()
  (interactive)
  (set-input-method 'hebrew-new)
  (ispell-change-dictionary "hebrew"))

(defun malachi/change-language-to-english ()
  (interactive)
  (set-input-method 'rfc1345)
  (ispell-change-dictionary "american"))

(defhydra hydra-toggle-language (:timeout 4)
  "toggle input language"
  ("h" malachi/change-language-to-hebrew "Hebrew" :exit t)
  ("e" malachi/change-language-to-english "English" :exit t))

(malachi/leader-keys
  "tl" '(hydra-toggle-language/body :which-key "language"))

(defun malachi/set-font-faces ()
  (set-face-attribute 'default nil :font "FiraCode NF" :height malachi/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "FiraCode NF" :height malachi/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height malachi/default-variable-font-size :weight 'regular)

  (+bidi-set-fonts-h))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (setq doom-modeline-icon t)
                  (with-selected-frame frame
                    (malachi/set-font-faces))))
      (malachi/set-font-faces))

(use-package ligature
  :config
  ;; Enable www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the `variable-pitch` face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures '(prog-mode org-mode LaTeX-mode)
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enables ligature checks globally in all buffers.
  ;; You can aslo do per mode with `ligature-mode1
  (global-ligature-mode 't))

(defun malachi/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
    (lambda (block-name)
      (malachi/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
    '("Dingbats"
      "Emoticons"
      "Miscellaneous Symbols and Pictographs"
      "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(malachi/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands '(counsel-find-file
					  counsel-file-jump
					  counsel-recentf
					  counsel-projectile-find-file
					  counsel-projectile-find-dir)))

(global-hl-line-mode t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	  doom-themes-enable-italic t)

  ;; (load-theme 'doom-ayu-dark t)
  ;; Correct line number colors for ayu-dark
  ;; (set-face-foreground 'line-number "#1e222a")
  ;; (set-face-foreground 'line-number-current-line "#e6b673")

  (load-theme 'doom-one t)
  (set-face-foreground 'line-number-current-line "#908ac0")

  ;; (load-theme 'doom-tomorrow-night t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :defer 0.1
  :custom (solaire-mode-remap-fringe t)
  :config (solaire-global-mode +1))

(malachi/leader-keys
 "tt" '(counsel-load-theme :which-key "theme"))

(setq display-time-format "%k:%M %a %d/%m/%y"
      display-time-default-load-average nil)

(use-package diminish)

(use-package delight
  :config
  (delight '((c++-mode "" :major)
             (lua-mode "󰢱" :major)
             (org-mode "" :major)
             (elisp-mode "" :major)
             (vterm-mode "" :major)
             (dashboard-mode "󰟒" :major)
             (pdf-view-mode "" :major))))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  ;; :after eshell
  ;; :hook (after-init-hook . doom-modeline-init)
  ;; Set default mode-line
  :config
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-height 20)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp nil)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-time nil)
  (doom-modeline-env-version nil))

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

  Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' ired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode)))
     "Emacs")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((derived-mode-p '(eshell-mode
                       term-mode
                       shell-mode
                       vterm-mode))
     "Term")
    ((memq major-mode '(helpful-mode
                        help-mode))
     "Help")
    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "*vterm*" name)
     (string-prefix-p "*terminal*" name)
     (string-prefix-p "*eshell*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))
  
(use-package centaur-tabs
  :demand
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "●"
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t)
  (centaur-tabs-mode t)
  :bind
  ("C-M-h" . centaur-tabs-backward)
  ("C-M-l" . centaur-tabs-forward)
  ("s-S-l" . centaur-tabs-move-current-tab-to-left)
  ("s-S-h" . centaur-tabs-move-current-tab-to-right)
  (:map evil-normal-state-map
    ("C-M-h" . centaur-tabs-backward)
    ("C-M-l" . centaur-tabs-forward)
    ("s-S-l" . centaur-tabs-move-current-tab-to-left)
    ("s-S-h" . centaur-tabs-move-current-tab-to-right)
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward)))

(use-package dashboard
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "With Great Power Comes Great Responsibility!\n\n\n\n"
        dashboard-center-content t
        dashboard-set-footer nil
        dashboard-startup-banner "~/.emacs.d/banner.txt"
        dashboard-show-shortcuts nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'projectile
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name

        dashboard-set-navigator t

        dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))

  (dashboard-setup-startup-hook))

;; For frames created by emacsclient -c
(setq initial-buffer-choice (lambda ()
                              (dashboard-refresh-buffer)))
                              ;;(get-buffer-create "*dashboard*")))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
         :config
         (setq ivy-extra-directories nil)
         (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(use-package ivy-prescient ;; Remember history
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package ivy-posframe
  :after ivy
  :custom
  (ivy-posframe-border-width 6)
  ;(ivy-posframe-width      200)
  (ivy-posframe-min-width  115)
  ;(ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (counsel-M-x . ivy-posframe-display-at-frame-top-center)
          (t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode t))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; (use-package svg-tag-mode
;;   :defer t
;;   :commands (svg-tag-mode global-svg-tag-mode)
;;   :hook (org-mode . svg-tag-mode)
;;   :config
;;   (plist-put svg-lib-style-default :font-family "FiraCode NF")
;;   (plist-put svg-lib-style-default :font-size 11.5)
;;   ;; (plist-put svg-lib-style-default :alignment 0)
;;   ;; (plist-put svg-lib-style-default :ascent 'bottom)

;;   (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
;;   (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
;;   (defconst day-re "[A-Za-z]\\{3\\}")
;;   (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

;;   (defun svg-progress-percent (value)
;;     (svg-image (svg-lib-concat
;;                 (svg-lib-progress-bar (/ (string-to-number value) 100.0) nil
;;                                       :height 0.8 :background (doom-color 'bg)
;;                                       :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                 (svg-lib-tag (concat value "%") nil
;;                              :height 0.8 :background (doom-color 'bg)
;;                              :stroke 0 :margin 0)) :stroke 0 :margin 0 :ascent 'center))

;;   (defun svg-progress-count (value)
;;     (let* ((seq (mapcar #'string-to-number (split-string value "/")))
;;            (count (float (car seq)))
;;            (total (float (cadr seq))))
;;     (svg-image (svg-lib-concat
;;                 (svg-lib-progress-bar (/ count total) nil
;;                                       :backgroud (doom-color 'bg) :height 0.8
;;                                       :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                 (svg-lib-tag value nil
;;                              :backgroud (doom-color 'bg) :height 0.8
;;                              :stroke 0 :margin 0)) :ascent 'center)))

;;   ;;(set-face-attribute 'svg-tag-default-face nil :family "FiraCode NF")
;;   (setq svg-tag-tags
;;         `(

;;           ;; Progress
;;           ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;;                                               (svg-progress-percent (substring tag 1 -2)))))
;;           ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;;                                               (svg-progress-count (substring tag 1 -1)))))

;;           ;; Task priority e.g. [#A], [#B], or [#C]
;;           ("\\[#A\\]" . ((lambda (tag) (svg-tag-make tag :face 'error :inverse t :height .85
;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))
;;           ("\\[#B\\]" . ((lambda (tag) (svg-tag-make tag :face 'warning :inverse t :height .85
;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))
;;           ("\\[#C\\]" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :height .85
;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))

;;           ;; Org tags
;;           (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
;;           (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))


;;           ;; TODO / DONE
;;           ("TODO" . ((lambda (tag) (svg-tag-make tag :inverse t :height 1.0 :face 'org-todo :margin 0))))
;;           ("DONE" . ((lambda (tag) (svg-tag-make tag :inverse t :height 1.0 :face 'org-done :margin 0))))


;;           ;; Citation of the form [cite:@Knuth:1984]
;;           ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
;;                                             (svg-tag-make tag
;;                                                           :inverse t
;;                                                           :height 0.8
;;                                                           :face 'org-cite
;;                                                           :beg 7 :end -1
;;                                                           :crop-right t))))
;;           ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
;;                                                   (svg-tag-make tag
;;                                                                 :face 'org-cite
;;                                                                 :end -1
;;                                                                 :crop-left t))))


;;           ;;Active date (with or without day name, with or without time)
;;           (,(format "\\(<%s>\\)" date-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :end -1 :margin 0 :height 0.8 :face 'org-date))))
;;           (,(format "\\(<%s \\)%s>" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :height 0.8 :face 'org-date))))
;;           (,(format "<%s \\(%s>\\)" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :height 0.8 :face 'org-date))))

;;           ;; Inactive date  (with or without day name, with or without time)
;;           (,(format "\\(\\[%s\\]\\)" date-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :end -1 :margin 0 :height 0.8))))
;;           (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :height 0.8))))
;;           (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :height 0.8)))))))


;; To do:         TODO DONE

;; Tags:          :TAG1:TAG2:TAG3:

;; Priorities:    [#A] [#B] [#C]

;; Progress:      [1/3]
;;                [42%]

;; Active date:   <2021-12-24>
;;                <2021-12-24 Fri>
;;                <2021-12-24 14:00>
;;                <2021-12-24 Fri 14:00>

;; Inactive date: [2021-12-24]
;;                [2021-12-24 Fri]
;;                [2021-12-24 14:00]
;;                [2021-12-24 Fri 14:00]

;; Citation:      [cite:@Knuth:1984]

(defun malachi/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (prettify-symbols-mode)
  (setq-default prettify-symbols-unprettify-at-point 'right-edge)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . malachi/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-return-follows-links t
        org-deadline-warning-days 30
        ;org-agenda-tags-column 75
        org-capture-bookmark nil
        org-highlight-latex-and-related '(native)
        org-start-with-inline-images t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-weekend-days '(5 6))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "BACKLOG(b)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAITING(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(w@)")))

  (setq org-agenda-files '("~/.emacs.d/orgfiles/inbox.org"
                           "~/.emacs.d/orgfiles/projects.org"
                           "~/.emacs.d/orgfiles/repeaters.org"))

  (setq org-capture-templates '(("t" "TODO" entry
                                     (file+headline "~/.emacs.d/orgfiles/inbox.org" "Tasks")
                                     "* TODO %?\n  %i\n  %a")))
  (setq org-agenda-custom-commands
  '((" " "Agenda"
     ((agenda ""
              ((org-agenda-span 'week)))
      (todo "TODO"
             ((org-agenda-overriding-header "Unscheduled tasks")
              (org-agenda-files '("~/.emacs.d/orgfiles/inbox.org"))
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
      (todo "TODO"
             ((org-agenda-overriding-header "Unscheduled project tasks")
              (org-agenda-files '("~/.emacs.d/orgfiles/projects.org"))
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))))

  ;; Save all org buffers when a deadline/schedule/node/todo is changed.
  (defmacro func-ignore (fnc)
    "Return function that ignores its arguments and invokes FNC"
    '(lambda (&rest _rest)
       (funcall , fnc)))

  (advice-add 'org-deadline       :after (func-ignore #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (func-ignore #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (func-ignore #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (func-ignore #'org-save-all-org-buffers)))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  ;(org-superstar-special-todo-items 'hide)
  (org-superstar-special-todo-items '(("TODO" . 9744)     ; ☐
                                      ("DONE" . 9745)))   ; ☑
  (org-superstar-item-bullet-alist '((42 . 10032)  ; -    ; ▸
                                     (43 . 8226)   ; +    ; •
                                     (45 . 9656))) ; *    ; ✰
  (org-superstar-headline-bullets-list '("◉" "○" "●" "✦" "✸" "▷" "✿")))

(with-eval-after-load 'org
  ;;;; For doom-ayu-dark theme
  ;; (set-face-attribute 'org-document-title nil :font "Fira Sans" :weight 'bold :height 1.4  :foreground "#73b8ff" :underline '(:color "#d2a6ff" :style line))

  ;; (set-face-attribute 'org-level-1 nil :font "Fira Sans" :weight 'medium :height 1.25 :foreground "#59c2ff")
  ;; (set-face-attribute 'org-level-2 nil :font "Fira Sans" :weight 'medium :height 1.15 :foreground "#d2a6ff")
  ;; (set-face-attribute 'org-level-3 nil :font "Fira Sans" :weight 'medium :height 1.1 :foreground "#ffb454")
  ;; (set-face-attribute 'org-level-4 nil :font "Fira Sans" :weight 'medium :height 1.05 :foreground "#aad94c")
  ;; (set-face-attribute 'org-level-5 nil :font "Fira Sans" :weight 'medium :height 1.05 :foreground "#f26d78")
  ;; (set-face-attribute 'org-level-6 nil :font "Fira Sans" :weight 'medium :height 1.05 :foreground "#e6b673")
  ;; (set-face-attribute 'org-level-7 nil :font "Fira Sans" :weight 'medium :height 1.05 :foreground "#95e6cb")
  ;; (set-face-attribute 'org-level-8 nil :font "Fira Sans" :weight 'medium :height 1.05 :foreground "#d95757")

  ;;;; For doom-one theme
  (set-face-attribute 'org-document-title nil :font "Fira Sans" :weight 'bold :height 1.4  :foreground "#4ea3de" :underline '(:color "#c276d8" :style line))
  (set-face-attribute 'org-level-1 nil :font "Fira Sans" :weight 'medium :height 1.25)
  (set-face-attribute 'org-level-2 nil :font "Fira Sans" :weight 'medium :height 1.15)
  (set-face-attribute 'org-level-3 nil :font "Fira Sans" :weight 'medium :height 1.1)
  (set-face-attribute 'org-level-4 nil :font "Fira Sans" :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-5 nil :font "Fira Sans" :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-6 nil :font "Fira Sans" :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-7 nil :font "Fira Sans" :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-8 nil :font "Fira Sans" :weight 'medium :height 1.05)

  (set-face-attribute 'org-latex-and-related nil :family "FiraCode NF" :weight 'normal :height 0.8 :foreground "#ff8f40")

  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(defun malachi/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
 :hook (org-mode . malachi/org-mode-visual-fill))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-roam
  :defer t
  :custom
  (org-roam-directory "~/.emacs.d/orgfiles/roam")
  (org-roam-completions-everywhere t)
  (org-roam-capture-templates
    '(("d" "default" plain
       "%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
       :unnarrowed t)
      ("p" "project" plain
       "\n* Goals\n\n%?\n\n* Tasks\n\n++ TODO Add initial tasks\n\n* Dates\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: Project")
       :unnarrowed t)
      ("r" "bibliography reference" plain
       "%?"
       :if-new (file+head "references/${citekey}.org" "#+title: ${title}\n")
       :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode t)
  (org-roam-setup))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package org-ref
  :after org
  :config
  (setq
   bibtex-completion-bibliography '("~/.emacs.d/orgfiles/roam/bibtex.bib")
   bibtex-completion-notes-path "~/.emacs.d/orgfiles/roam/references"
   bibtex-completion-pdf-field "file"
   bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process "open" nil 0 nil fpath))))

(use-package ivy-bibtex
  :after org-ref)

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  ;; (use-package org-fragtog
  ;;   :hook
  ;;   (org-mode . org-fragtog-mode)
  ;;   :config
  ;;   (setq org-startup-with-latex-preview t)
  ;;   (plist-put org-format-latex-options :scale 1.5))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
  (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(malachi/leader-keys
  "o" '(:ignore t :which-key "org mode")
  "oi" '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")
  "oa" '(org-agenda :which-key "agenda")
  "ot" '(org-todo-list :which-key "todos")
  "oc" '(org-capture :which-key "capture")
  "ox" '(org-export-dispatch :which-key "export")
  "or" '(:ignore t :which-key "roam")
  "ort" '(org-roam-buffer-toggle :which-key "toggle buffer")
  "orf" '(org-roam-node-find :which-key "find")
  "ori" '(org-roam-node-insert :which-key "insert node")
  "orb" '(orb-insert-link :which-key "insert bibtex link")
  "oru" '(org-roam-ui-open :which-key "ui")
  "orc" '(org-roam-capture :which-key "capture")
  "orp" '(completion-at-point :which-key "completion"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (lua . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("lua" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src c++"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our config.org config file when we save it
(defun malachi/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda ()
                        (add-hook 'after-save-hook #'malachi/org-babel-tangle-config)))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(malachi/leader-keys
 "g" '(:ignore t :which-key "git")
 "gs" '(magit-status :which-key "status")
 "gd" '(magit-diff-unstaged :which-key "unstaged-diff")
 "gc" '(magit-branch-or-checkout :which-key "checkout")
 "gl" '(:ignore t :which-key "log")
 "glc" '(magit-log-current :which-key "current")
 "glf" '(magit-log-buffer-file :which-key "file")
 "gb" '(magit-branch :which-key "branch")
 "gP" '(magit-push-current :which-key "push")
 "gp" '(magit-pull-branch :which-key "pull")
 "gf" '(magit-fetch :which-key "fetch")
 "gF" '(magit-fetch-all :which-key "fetch all")
 "gr" '(magit-rebase :which-key "rebase"))

(use-package magit-todos
  :defer t)

(use-package forge
  :after magit)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t)
  (malachi/leader-keys
    "gL"  '(git-link :which-key "link")))

(use-package git-gutter
  :diminish
  :hook ((prog-mode . git-gutter-mode)
         ;(org-mode . git-gutter-mode)
         (LaTeX-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (require 'git-gutter-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

(use-package git-gutter-fringe
  :after git-gutter)

(use-package git-timemachine
  :commands (git-timemachine))
  :config
  (malachi/leader-keys
    "gt"  '(git-link :which-key "time-machine"))

(defun malachi/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "/mnt/c/Users/malach/My\ Stuff/Programming/My\ Projects")
    (setq projectile-project-search-path '("/mnt/c/Users/malach/My\ Stuff/Programming/My\ Projects")))
    (setq projectile-project-search-action #'projectile-dired)
    (setq projectile-switch-project-action #'malachi/switch-project-action))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(malachi/leader-keys
 "p" '(:ignore t :which-key "project")
 "pf" '(projectile-find-file :which-key "find file")
 "pF" '(counsel-projectile-rg :which-key "grep")
 "ps" '(projectile-switch-project :which-key "switch project")
 "pc" '(projectile-compile-project :which-key "compile project")
 "pd" '(projectile-dired :which-key "projectile-dired"))

(defun malachi/lsp-mode-setup ()
  ;; (setq lsp-headerline-breadcrum-segments '(path-to-project file symbols))
  ;; (lsp-headerline-breadcrumb-mode t)
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred);
  :init
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;(evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
  :hook (lsp-mode . malachi/lsp-mode-setup))
         ;(lsp-mode . lsp-enable-which-key-integration))

(add-hook 'prog-mode-hook #'lsp)

(malachi/leader-keys
 "l" '(:ignore t :which-key "lsp")
 "ld" '(xref-find-definitions :which-key "find definition")
 "lr" '(xref-find-references :which-key "find refrences")
 "ln" '(lsp-ui-find-next-reference :which-key "next reference")
 "lp" '(lsp-ui-find-prev-reference :which-key "previous reference")
 "lj" '(counsel-imenu :which-key "jump")
 "le" '(lsp-ui-flycheck-list :which-key "flycheck list")
 "ls" '(lsp-ui-sideline-mode :which-key "sideline mode")
 "lx" '(lsp-execute-code-action :which-key "execute action"))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package lsp-ivy
    :after lsp)

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Set up cpp debugging
  ;; (require 'dap-lldb)

  ;; Bind `SPC l d` to `dap-hydra` for easy access
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode nil))

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)

(use-package typescript-mode
  :mode ("\\.\\(ts\\|tsx\\)\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package json-mode
  :mode "\\.json\\'"
  :hook (before-save . malachi/json-mode-before-save-hook)
  :preface
  (defun malachi/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))

  (defun malachi/json-array-of-numbers-on-one-line (encode array)
    "Print the arrays of numbers in one line"
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separtor (if json-encoding-pretty-print "," ", ")))
           (funcall encode array)))
    :config (advice-add 'json-encode-array :around #'malachi/json-array-of-numbers-on-one-line))

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")

  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp-deferred))

(use-package auctex
  :defer t
  :hook
  (TeX-mode-hook . prettify-symbols-mode)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t
        prettify-symbols-unprettify-at-point 'right-edge
        TeX-source-correlate-mode t
        TeX-parse-self t
        setq TeX-auto-save t)
  (setq-default TeX-master nil))

(with-eval-after-load 'auctex
  ;; Increase the size of various headings
  (set-face-attribute 'font-latex-slide-title-face nil :font "Fira Sans" :weight 'bold :height 1.4)

  (set-face-attribute 'font-latex-sectioning-0-face nil :weight 'medium :height 1.3)
  (set-face-attribute 'font-latex-sectioning-1-face nil :weight 'medium :height 1.3)
  (set-face-attribute 'font-latex-sectioning-2-face nil :weight 'medium :height 1.3)
  (set-face-attribute 'font-latex-sectioning-3-face nil :weight 'medium :height 1.2)
  (set-face-attribute 'font-latex-sectioning-4-face nil :weight 'medium :height 1.1)
  (set-face-attribute 'font-latex-sectioning-5-face nil :weight 'medium :height 1.1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :init
  (setq company-clang-executable "/usr/lib/clang")
  :bind (:map company-active-map
        ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil

        ;; These are the Doom Emacs defaults (icon colors)
        company-box-icons-all-the-icons
       `((Unknown . ,(all-the-icons-material "find_in_page" :face 'all-the-icons-purple))
        (Text . ,(all-the-icons-material "text_fields" :face 'all-the-icons-green))
        (Method . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
        (Function . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
        (Constructor . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
        (Field . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
        (Variable . ,(all-the-icons-material "adjust" :face 'all-the-icons-blue))
        (Class . ,(all-the-icons-material "class" :face 'all-the-icons-red))
        (Interface . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
        (Module . ,(all-the-icons-material "view_module" :face 'all-the-icons-red))
        (Property . ,(all-the-icons-material "settings" :face 'all-the-icons-red))
        (Unit . ,(all-the-icons-material "straighten" :face 'all-the-icons-red))
        (Value . ,(all-the-icons-material "filter_1" :face 'all-the-icons-red))
        (Enum . ,(all-the-icons-material "plus_one" :face 'all-the-icons-red))
        (Keyword . ,(all-the-icons-material "filter_center_focus" :face 'all-the-icons-red))
        (Snippet . ,(all-the-icons-material "short_text" :face 'all-the-icons-red))
        (Color . ,(all-the-icons-material "color_lens" :face 'all-the-icons-red))
        (File . ,(all-the-icons-material "insert_drive_file" :face 'all-the-icons-red))
        (Reference . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
        (Folder . ,(all-the-icons-material "folder" :face 'all-the-icons-red))
        (EnumMember . ,(all-the-icons-material "people" :face 'all-the-icons-red))
        (Constant . ,(all-the-icons-material "pause_circle_filled" :face 'all-the-icons-red))
        (Struct . ,(all-the-icons-material "streetview" :face 'all-the-icons-red))
        (Event . ,(all-the-icons-material "event" :face 'all-the-icons-red))
        (Operator . ,(all-the-icons-material "control_point" :face 'all-the-icons-red))
        (TypeParameter ,(all-the-icons-material "class" :face 'all-the-icons-red))
        (Template . ,(all-the-icons-material "short_text" :face 'all-the-icons-green)))))

(use-package yasnippet-snippets)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;;; electric-pair
(use-package elec-pair
  :hook ((prog-mode org-mode LaTeX-mode) . electric-pair-mode)
  :config
  (setq electric-pair-preserve-balance nil ; for LaTeX
        electric-pair-skip-whitespace nil
        electric-pair-delete-adjacent-pairs t
        electric-pair-open-newline-between-pairs nil
        electric-pair-skip-whitespace-chars '(9 10 32)
        electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
                              (?\{ . ?\})
                              (?\[ . ?\])
                              )))

;; Disable electric-pair-mode in minibuffer during Macro definition
(defvar malachi/electic-pair-modes '(c-mode c++-mode lisp-mode emacs-lisp-mode org-mode LaTeX-mode))

(defun malachi/inhibit-electric-pair-mode (char)
  (not (member major-mode malachi/electic-pair-modes)))

(setq electric-pair-inhibit-predicate #'malachi/inhibit-electric-pair-mode)

(add-hook 'org-mode-hook '+electric-inhibit-<)
(defun +electric-inhibit-< ()
  "Disable auto pairing of  `<>'."
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<) t
                   (,electric-pair-inhibit-predicate c)))))

(defun malachi/set-whitespace-mode-font-faces (&rest _)
  (set-face-attribute 'whitespace-space nil :family "FiraCode NF" :background nil :foreground "#565b66" :height 0.8)
  (set-face-attribute 'whitespace-newline nil :family "FiraCode NF" :background nil :foreground "#565b66" :height 1.1)
  (set-face-attribute 'whitespace-tab nil :family "FiraCode NF" :background nil :foreground "#565b66" :height 1.0))

(advice-add 'whitespace-mode :before #'malachi/set-whitespace-mode-font-faces)

(setq-default whitespace-display-mappings
              '((space-mark 32
                           [183]
                           [46])
               (newline-mark 10
                             [8201 9166 10]
                             [36 10])
               (tab-mark 9
                         [9655 9] ; 9141 9251
                         [187 9]
                         [92 9])))

(malachi/leader-keys
 "tw" '(whitespace-mode :which-key "whitespace mode"))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)
         (org-mode . ws-butler-mode)))

(use-package aggressive-indent
  :defer t
  ;; :hook ((prog-mode org-mode) . aggressive-indent-mode)
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode))
  ;; (add-to-list 'aggressive-indent-excluded-modes 'snippet-mode)
  (add-hook 'snippet-mode-hook (lambda () (aggressive-indent-mode -1)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line 2 10)
  ;(highlight-indent-guides-character ?\|) ;; Indent character samples: | ┆ ┊
  :commands highlight-indent-guides-mode
  :hook (prog-mode  . highlight-indent-guides-mode))

(use-package origami
  :hook ((yaml-mode . origami-mode)
         (c-mode . origami-mode)
         (c++-mode . origami-mode)
         (javascript-mode . origami-mode)
         (typescript-mode . origami-mode)
         (elisp-mode . origami-mode)
         (python-mode . origami-mode)
         (LaTeX-mode . origami-mode)))

(use-package rainbow-delimiters
  :hook ((org-mode prog-mode LaTex-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook ((prog-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         (org-mode . rainbow-mode)
         (css-mode . rainbow-mode))
  :config
  ;; Changed this function so rainbow mode colorizes the foreground and not the background
  (defun rainbow-colorize-match (color &optional match)
    "Return a matched string propertized with a face whose
  foreground is COLOR. The background is computed using
  `rainbow-color-luminance'."
    (let ((match (or match 0)))
      (put-text-property
       (match-beginning match) (match-end match)
       'face `(;(:background ,(if (> 0.5 (rainbow-x-color-luminance color))
               ;                  "white" "black"))
               (:foreground ,color))))))

(use-package tree-sitter-langs)

(use-package tree-sitter
  :after tree-sitter-langs
  :hook (global-tree-sitter-mode . tree-sitter-hl-mode)
  :custom
  ((global-tree-sitter-mode t))
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package darkroom
  :commands (darkroom-mode darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 0)
  (darkroom-tentative-mode 0))

(defun malachi/enter-focus-mode ()
  (interactive)
  (darkroom-tentative-mode 1)
  (centaur-tabs-mode 0)
  (display-line-numbers-mode 0))

(defun malachi/leave-focus-mode ()
  (interactive)
  (darkroom-tentative-mode 0)
  (centaur-tabs-mode 1)

  (if (eq major-mode 'org-mode)
      (display-line-numbers-mode 0)
    (display-line-numbers-mode 1)))

(defun malachi/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-tentative-mode)
      (malachi/leave-focus-mode)
    (malachi/enter-focus-mode)))

(malachi/leader-keys
  "tf" '(malachi/toggle-focus-mode :which-key "focus mode"))

(use-package pdf-tools
  ;; :defer t
  :config
  (pdf-loader-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; fix blurry text
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick t))

(add-hook 'pdf-view-mode-hook
          (lambda ()
        (centaur-tabs-local-mode)
        (set (make-local-variable 'evil-normal-state-cursor) (list nil))
        (internal-show-cursor nil nil)))

(setq ispell-dictionary "american")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)

(dolist (hook '(text-mode-hook org-mode-hook TeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regex "^[#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :after evil-collection
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-hide-method 'delete-window)
  ;; show vterm buffer as side window at the bottom
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                    (let ((buffer (get-buffer buffer-or-name)))
                      (with-current-buffer buffer
                        (or (equal major-mode 'vterm-mode)
                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  ;; work with centaur-tabs
  ;; (Defined shell, term, shell and vterm to same group in centaur-tabs)
  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p))

(defun vmacs-term-mode-p(&optional args)
  (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))

(malachi/leader-keys
 "tv" '(vterm-toggle :which-key "vterm"))

(defun malachi/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for preformance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  ;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

 (setq eshell-history-size 10000
       eshell-buffer-maximum-lines 10000
       eshell-hist-ignoreedups t
       eshell-scroll-to-bottom-on-input t))

(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell
  :hook (eshell-first-time-mode . malachi/configure-eshell))
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "vim"))

    (eshell-git-prompt-use-theme 'powerline))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package dired
  :straight nil
  :hook (dired-mode . dired-hide-details-mode)
  :commands (dired dired-jump)
  :custom
  ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(malachi/leader-keys
  "d" '(dired-jump :which-key "Dired")
  "f" '(find-file :which-key "find file"))

(use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#e6b450" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-single
  :defer t)

(use-package dired-ranger
  :defer t
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(use-package dired-collapse
  :defer t)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package mu4e
  :straight nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  ;:defer 20
  :config
  ;; Run mu4e in the background to sync mail periodically
  (mu4e t)

  ;; tell emacs where the mu binary is
  (setq mu4e-mu-binary "/usr/local/bin/mu")

  ;; this is set to 't' to avoid email syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/Mail")

  (setq mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-sent-folder "/[Gmail]/Sent Mail"
        mu4e-refile-folder "/[Gmail]/All Mail"
        mu4e-trash-folder "/[Gmail]/Trash")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox" :key ?i)
          (:maildir "/[Gmail]/Sent Mail" :key ?s)
          (:maildir "/[Gmail]/Trash" :key ?t)
          (:maildir "/[Gmail]/Drafts" :key ?d)
          (:maildir "/[Gmail]/All Mail" :key ?a)))

  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (setq shr-use-colors t)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-headers-date-format "%d/%m/%y"))
