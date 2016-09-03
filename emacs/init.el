(let ((gc-cons-threshold most-positive-fixnum))

;;; Package init
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)

;;; Custom.el
    (setq custom-file "~/.emacs.d/custom.el")
    (load custom-file)

;;; Plugins
  (autoload 'yasnippet "yasnippets" "yet another snippet plugin" t)
  (yas-global-mode 1)

  (ivy-mode 1)
  
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace")
  (save-place-mode 1)

;; Evil mode
   (autoload 'evil "evil" "extensive vi layer" t)
   (evil-mode 1)
   (global-evil-leader-mode 1)
   (evil-leader/set-leader ",")

   (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
   
   (evil-leader/set-key
    "m" 'recompile-quietly
    "r" 'shell-command
    "w" 'evil-write
    "l" 'switch-to-compilation-buffer
    "t" 'switch-to-term-or-back
    "b" 'ivy-switch-buffer
    "e" 'counsel-find-file
    "s" 'swiper
    "c" 'comment-or-uncomment-region
   )
   (define-key evil-normal-state-map (kbd ";") 'evil-ex)
   
    (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
    ;; In Delete Selection mode, if the mark is active, just deactivate it;
    ;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (global-set-key [escape] 'evil-exit-emacs-state)
    
;;; General settings
    (setq evil-mode-line-format '(before . mode-line-front-space)) 

    ;(load-theme 'smyx)
    (load-theme 'mustang)

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (show-paren-mode t)
    (xclip-mode t)
    (xterm-mouse-mode t)

    (setq make-backup-files nil)
    (setq auto-save-default nil)
    (setq-default indent-tabs-mode nil)
    (setq tab-width 4)
    (setq c-basic-offset 4)

    (setq compilation-always-kill t)
    (setq shell-file-name "zsh")
    (setq shell-command-switch "-ic")

    (setq inhibit-startup-screen +1)
    (setq initial-major-mode 'org-mode)
    (setq initial-scratch-message nil)

    (require 'smartparens-config)


;;; Mode hooks
(add-hook 'rust-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (let ((file (file-name-nondirectory buffer-file-name)))
                   (format "cargo run")))))

(add-hook 'c-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "clang -o %s %s -Weverything -std=c99 && ./%s"
                             (file-name-sans-extension file)
                             file
                             (file-name-sans-extension file)))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "clang++ -o %s %s -Weverything -std=c++14 -fno-exceptions && ./%s"
                             (file-name-sans-extension file)
                             file
                             (file-name-sans-extension file)))))))


;; GO mode
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/Users/andy_normann/Git/Code/go/")

(setq exec-path (cons "/usr/local/Cellar/go/1.6.2/bin" exec-path))
(add-to-list 'exec-path "/Users/andy_normann/Git/Code/go/bin")

(defun my-go-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (format "go run %s"
                     file)))))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Compilation mode
(add-hook 'compilation-mode-hook '(lambda ()
                                    (local-unset-key "g")
                                    (local-unset-key "h")
                                    (evil-define-key 'motion compilation-mode-map "r" 'recompile)
                                    (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)
                                    (evil-define-key 'motion compilation-mode-map "k" 'kill-compilation)
                                    (evil-define-key 'motion compilation-mode-map "l" 'switch-to-compilation-buffer)))

;;; Helper functions
(defun recompile-quietly ()
  (interactive)
  (save-window-excursion
    (recompile)))

(defun switch-to-compilation-buffer ()
  (interactive)
  (if (string-match "compilation" (buffer-name))
      (switch-to-prev-buffer)
    (switch-to-buffer "*compilation*")))

(defun switch-to-term-or-back ()
  (interactive)
  (if (string-match "*term*" (buffer-name))
      (switch-to-prev-buffer)
    (if (get-buffer "*term*" )
        (switch-to-buffer "*term*")
      (term "/bin/zsh"))))

(defun init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


)
