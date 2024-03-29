(package-initialize)

(setq inhibit-default-init 1
      inhibit-startup-message 1)


(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

;;; External Packages
(load (emacs-d "packages"))
;;; Personal Elisp functions
(load (emacs-d "bdd-defuns"))
;;; Twitter
(load (emacs-d "twitter") 'missing-ok)
;;;; CEDET
(load (emacs-d "cedet"))
;;; Theme
;;; (load-theme 'zenburn t)
(load-theme 'sanityinc-tomorrow-eighties t)


;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Users/yramin/repos/goroot/bin")

(load (emacs-d "go-autocomplete"))
(require 'go-autocomplete)
(require 'auto-complete-config)


;;; Registers
(set-register ?i
              (cons 'file (emacs-d "init.el")))
(set-register ?p
              (cons 'file (emacs-d "packages.el")))

;;; no backup files, no auto-saving
;;;--- TODO: Consolidate save files to a common directory.
(setq make-backup-files nil)
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;;;; UI
(if window-system
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      ;; 4px left, and no right right fringe
      (set-fringe-style '(4 . 0)))
  ;; No menu bar when running from a terminal.
  (menu-bar-mode 0))


;;;; Mode Line
(setq size-indication-mode t
      line-number-mode t
      column-number-mode t)

;;;; Ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-use-virtual-buffers t
      recentf-save-file (emacs-d "var/recentf")
      ido-save-directory-list-file (emacs-d "var/ido-last.el"))


;; Display completions vertically
(setq ido-decorations (quote ("\n> " "" "\n  " "\n  ..." "[" "]"
                         " [No Match]" " [Matched]" " [Not Readable]"
                         " [Too Big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
 (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
 (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
 (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


;;;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        delete-by-moving-to-trash t
        trash-directory (expand-file-name ".Trash" (getenv "HOME"))))


;;; TAB behavior
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)   ; never use tabs to indent.

;;;; Electric
(electric-pair-mode)   ; paranthesis, braces, quotation marks.
(electric-indent-mode) ; on-the-fly reindentation.
;; Use auto indentation only in programming modes.
(hook-into-modes '(lambda ()
                    (local-set-key (kbd "RET") 'newline-and-indent))
                 '(prog-mode-hook))

;;;; Whitespace
(setq-default indicate-empty-lines t) ; in the left fringe
(setq require-final-newline t)
(set-face-attribute 'whitespace-trailing nil
                    :background "#eeee22"
                    :weight 'bold)
(setq whitespace-style (quote (face trailing empty)))
(hook-into-modes 'whitespace-mode '(prog-mode-hook))
(hook-into-modes 'whitespace-cleanup-mode '(prog-mode-hook))

;;;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

;;;; Annoyances
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p) ; brevity
(setq ring-bell-function 'ignore) ; hush...
;;; Disable commonly unintended key presses.
;;; (global-unset-key (kbd "C-z")) ; suspend-frame
(global-unset-key (kbd "s-p")) ; ns-print-buffer
(global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel


;;;; Disabled commands
(dolist (cmd
         '(erase-buffer
           upcase-on
           downcase-region
           dired-find-alternate-file
           narrow-to-region))
  (put cmd 'disabled nil))


;;;; Misc
(show-paren-mode)
(global-auto-revert-mode)
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))
(hook-into-modes 'hl-line-mode '(prog-mode-hook))


;;;; Internal Packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;(when window-system
(add-hook 'after-init-hook 'server-start t)
;;;)



(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Initialization complete. (%.3fs)\n%s"
                        elapsed
                        (make-string 80 ?\-))))
          t)

(require 'smooth-scrolling)

(defun my-c-mode-hook ()
  (setq indent-tabs-mode nil))

(require 'cc-mode)
;;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(setq c-default-style "linux" c-basic-offset 4)


(add-hook 'ruby-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            ))

(add-hook 'sh-mode-hook
          '(lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq sh-basic-offset 2)
            ))

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        ))

(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))


(global-set-key [s-up] 'windmove-up)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-down] 'windmove-down)
(global-set-key [s-left] 'windmove-left)

(add-to-list 'default-frame-alist
             '(font . "Go Mono-12"))
;;(set-face-font 'default "GoMono-13")
(setq-default line-spacing 2)


(defun donuts ()
  (interactive)
  (print "Mmmm, donuts."))

(put 'set-goal-column 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Cursor blinks always
(setq blink-cursor-blinks -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(package-selected-packages
   (quote
    (yasnippet whitespace-cleanup-mode thrift smooth-scrolling smex salt-mode rust-mode ruby-tools regex-tool puppet-mode protobuf-mode projectile php-mode mustache-mode multiple-cursors markdown-mode magit log4j-mode latex-pretty-symbols latex-extra ido-ubiquitous haskell-mode handlebars-mode grep-a-lot google-c-style go-mode gitignore-mode gitconfig-mode gist flycheck flx-ido fill-column-indicator expand-region edit-server dtrt-indent color-theme-sanityinc-tomorrow browse-kill-ring auto-complete-clang ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
