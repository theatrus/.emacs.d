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
;;; Theme
;;; (load-theme 'zenburn t)
(load-theme 'sanityinc-tomorrow-eighties t)


;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

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
(setq whitespace-style '(face trailing tab-mark))
(hook-into-modes 'whitespace-mode '(prog-mode-hook))


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
(global-unset-key (kbd "C-z")) ; suspend-frame
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

(when window-system
  (add-hook 'after-init-hook 'server-start t))


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

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 2)
        (setq python-indent 2)))

(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))


(global-set-key [s-up] 'windmove-up)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-down] 'windmove-down)
(global-set-key [s-left] 'windmove-left)

(set-face-font 'default "Source Code Pro")


(defun donuts ()
  (interactive)
  (print "Mmmm, donuts."))

(put 'set-goal-column 'disabled nil)

(setq ispell-program-name "/usr/local/bin/ispell")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
