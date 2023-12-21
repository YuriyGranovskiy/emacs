(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-h") 'backward-char)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(timu-spacegrey))
 '(custom-safe-themes
   '("3074fda75f35f990d112fb75681729a74b6c7f15d3e5dfcf80313abb4cd39ed8" "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" default))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(zweilight-theme color-theme-sanityinc-solarized timu-spacegrey-theme sublime-themes atom-one-dark-theme vscode-dark-plus-theme zenburn-theme spacemacs-theme vs-dark-theme dockerfile-mode all-the-icons-nerd-fonts visual-fill-column visual-fill org-bullets all-the-icons neotree neo-tree dap-mode yasnippet company command-log-mode lsp-mode omnisharp ssh-agency ssh evil-magit magit projectile counsel ivy-rich which-key rainbow-delimiters doom-themes use-package doom-modeline mode csharp-mode go-mode yaml-mode solarized-theme ##))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 130)

;; Install and configure packages
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; Maximize the frame on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
; Make emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(98 . 50))
(add-to-list 'default-frame-alist '(alpha . (98 . 50)))

(column-number-mode)
(global-display-line-numbers-mode t)

;;
;; Org mode 
(use-package org
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (setq org-agenda-files
	'("~/OrgFiles/Tasks.org")))

;; C-j is used for org-return-and-maybe-indent by default
(defun unset-c-j ()
  (local-unset-key (kbd "C-j")))
(add-hook 'org-mode-hook 'unset-c-j)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 120
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))


;;
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		neotree-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-f" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'timu-spacegrey t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "d:/Source")
    (setq projectile-project-search-path '("d:/Source")))
  (when (file-directory-p "d:/code")
    (setq projectile-project-search-path '("d:/code")))
  (setq projectile-switch-project-action #'projectile-dired)) 

(with-eval-after-load 'projectile
  (projectile-register-project-type 'dotnet #'projectile-dotnet-project-p
                                  :project-file '("?*.csproj" "?*.fsproj")
                                  :compile "dotnet build"
                                  :run "dotnet run"
                                  :test "dotnet test"))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map [C-f5] 'projectile-compile-project)
  (define-key projectile-mode-map [f5] 'projectile-run-project))
; (define-key projectile-mode-map (kbd "M-,") 'projectile-command-map)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package omnisharp)

(defun my-split-window-below (&optional arg)
  "Splits window to the bottom with given transparency"
  (interactive "P")
  (let ((proportion (* (or arg 5) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

(defun my-split-window-right (&optional arg)
  "Splits window to the right with given transparency"
  (interactive "P")
  (let ((proportion (* (or arg 5) 0.1)))
    (split-window-right (round (* proportion (window-height))))))

; (my-split-window-below 8)
; (my-split-window-right 8)

(prefer-coding-system 'utf-8)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook (csharp-mode . lsp))

(defun my-visual-studio()
  (omnisharp-mode)
  (company-mode))
(use-package neotree)


(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 50)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

 ;(use-package dap-mode
;  :ensure t
;  :config
;  (dap-mode t)
;  (dap-ui-mode t))

(add-hook 'csharp-mode-hook 'my-visual-studio)
(add-hook 'csharp-mode-hook #'company-mode)
(use-package command-log-mode)
(use-package yasnippet)
;; Add debugging configuration for .NET Core projects
;(require 'dap-netcore)
;(dap-netcore-setup)
