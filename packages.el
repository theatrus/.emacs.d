;;; load-path.el
;;; External Packages
(require 'package)
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.org/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))


;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
    (message "Refreshing ELPA package archives...")
    (package-refresh-contents))

;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0)

(use-package whitespace-cleanup-mode
  :ensure t)

(use-package ag
  :ensure t
  :defer t
  :config
  (progn
    (setq ag-highlight-search t)
    (bind-key "n" 'compilation-next-error ag-mode-map)
    (bind-key "p" 'compilation-previous-error ag-mode-map)
    (bind-key "N" 'compilation-next-file ag-mode-map)
    (bind-key "P" 'compilation-previous-file ag-mode-map)))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :config
  (progn
    (browse-kill-ring-default-keybindings)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)
;;(use-package zenburn-theme
;;  :ensure t)

(use-package flycheck
  :ensure t)

(use-package projectile
  :ensure t)

(use-package auto-complete-clang
  :ensure t)

(use-package dtrt-indent
  :ensure t)

(use-package smooth-scrolling
  :ensure t)

(use-package diminish
  :ensure t)

(use-package edit-server
  :ensure t
  :if window-system
  :init
  (add-hook 'after-init-hook 'edit-server-start)
  :config
  (progn
    (bind-key "C-c C-k" 'edit-server-abort edit-server-edit-mode-map)
    (add-hook 'edit-server-start-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fill-column-indicator
  :ensure t
  :defer t)

(use-package flx-ido
  :ensure t
  :defer t)

(use-package flx
  :ensure t
  :defer t
  :init
  (progn
    (flx-ido-mode 1))
  :config
  (progn
    (setq ido-use-faces nil)
    (setq gc-cons-threshold 20000000)))

(use-package gist
  :ensure t
  :bind ("C-c g p" . gist-region-or-buffer-private))

;(use-package git-commit-mode
;  :ensure t
                                        ;  :defer t)

(use-package php-mode
  :ensure t
  :defer t)

(use-package grep-a-lot
  :ensure t
  :defer t)

(use-package thrift
  :ensure t
  :defer t)


(use-package log4j-mode
  :ensure t
  :defer t)



(use-package go-mode
  :ensure t
  :defer t
  :init
  (progn
    (defun go-capitalize-previous-word ()
      (interactive)
      (backward-word)
      (capitalize-word 1)))

  :config
  (progn
    (bind-key "C-c C-c" 'go-capitalize-previous-word go-mode-map)
    (bind-key "C-c f" 'gofmt go-mode-map)
    (bind-key "C-c d" 'godoc go-mode-map)))

(use-package google-c-style
  :ensure t
  :defer t)

(use-package ido-completing-read+
  :ensure t
  :init
  ;; Fix ido-ubiquitous for newer packages
;;  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;    `(eval-after-load ,package
;;       '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;          (let ((ido-ubiquitous-enable-compatibility nil))
;;            ad-do-it))))
;;  :config
;;  (progn
;;    (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
  ;;    (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)))
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80)))))

(use-package handlebars-mode
  :ensure t
  )

(use-package rust-mode
  :ensure t
  )

(use-package haskell-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))

    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-prvious-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (progn
    (setq mc/list-file (emacs-d "var/multiple-cursors-all-or-once.el"))))

(use-package mustache-mode
  :ensure t
  :defer t)


(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package puppet-mode
  :ensure t
  :mode ("\\.pp$" . puppet-mode))

(use-package regex-tool
  :ensure t
  :defer t)

(use-package ruby-tools
  :ensure t
  :defer t)


(use-package smex
  :ensure t
  :defer t
  :bind ("M-x" . smex))

(use-package yaml-mode
  :ensure t
  :defer t)

;(use-package yasnippet
;  :ensure t
;  :diminish yas-minor-mode
;  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
;  :init
;  (progn
;    (setq yas-verbosity 3)
;    (yas-global-mode 1)))

(use-package js2-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)
