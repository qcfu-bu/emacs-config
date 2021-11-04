(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Straight
(setq straight-check-for-modifications '(check-on-save)
      straight-cache-autoloads t)
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
(straight-use-package 'use-package)

;; Defaults
(save-place-mode 1)
(global-auto-revert-mode t)

;; Diagnostics
(use-package esup
  :straight t
  :commands (esup)
  :init
  (setq esup-depth 1))

;; Packages
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(use-package ivy-prescient
  :straight t
  :defer t)

(use-package counsel
  :straight t
  :config
  (counsel-mode)
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer))

(use-package company-prescient
  :straight t
  :defer t)

(use-package company
  :straight t
  :defer t
  :hook ((prog-mode . company-mode)
	 (text-mode . company-mode))
  :init
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

(use-package flycheck
  :straight t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(coq)))

(use-package flyspell
  :straight t
  :defer t
  :hook (text-mode . flyspell-mode))

(use-package magit
  :straight t
  :defer t)

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode +1))

(use-package treemacs
  :straight t
  :defer t)

(use-package smartparens
  :straight t
  :defer t
  :hook ((prog-mode . smartparens-mode)
	 (text-mode . smartparens-mode)
	 (prog-mode . show-smartparens-mode)
	 (text-mode . show-smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
	 (text-mode . rainbow-delimiters-mode)))

;; Appearance
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons-dired
  :straight t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(set-face-attribute 'default nil :font "Hack-14")
(set-face-attribute 'fixed-pitch nil :family "Hack")
(set-face-attribute 'variable-pitch nil :family "DejaVu Serif")

;; Languages
(use-package tex-site
  :straight auctex
  :defer t
  :mode (("\\.tex\\'" . LaTeX-mode)
	 ("\\.bib\\'" . LaTeX-mode)))

(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)))

(use-package org
  :straight t
  :defer t
  :mode (("\\.org\\'" . org-mode))
  :hook ((org-mode . org-indent-mode)
	 (org-mode . variable-pitch-mode))
  :config
  (setq org-format-latex-options
	(plist-put org-format-latex-options :scale 1.5)))

(use-package org-superstar
  :straight t
  :defer t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-hide-leading-stars nil
	org-superstar-leading-bullet ?\s
	org-indent-mode-turns-on-hiding-stars nil))

(use-package flycheck-ocaml
  :straight t
  :defer t)

(use-package merlin
  :straight t
  :defer t
  :init
  (setq merlin-error-after-save nil)
  :config
  (flycheck-ocaml-setup))

(use-package utop
  :straight t
  :defer t
  :hook (utop-mode . company-mode)
  :init
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package tuareg
  :straight t
  :defer t
  :mode (("\\.ml[lipy]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :hook ((tuareg-mode . merlin-mode)
         (tuareg-mode . utop-minor-mode)))

(use-package company-coq
  :straight t
  :defer t)
 
(use-package proof-general
  :straight t
  :defer t
  :after company-coq
  :mode (("\\.v\\'" . coq-mode))
  :hook (coq-mode . comany-coq-mode))
