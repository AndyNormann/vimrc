(let ((gc-cons-threshold most-positive-fixnum))

;;; Package init
  (require 'package)
  (setq package-enable-at-startup nil)

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

  (package-initialize)

;;; Custom.el
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

;;; Plugins

  ; Make sure we have use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)

  (use-package yasnippet :ensure t
    :config
    (yas-global-mode 1))

  (use-package saveplace :ensure t
    :config
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saveplace")
    (save-place-mode 1))

  (use-package ivy :ensure t
    :config
    (ivy-mode 1))

  (use-package evil :ensure t
    :config
    (evil-mode 1)
    (global-evil-leader-mode 1)
    (evil-leader/set-leader "SPC")
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
    (define-key evil-normal-state-map (kbd ";") 'evil-ex))


;;; General settings

    (setq evil-mode-line-format '(before . mode-line-front-space)) 

    (load-theme 'sanityinc-tomorrow-bright)

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
    (setq
     hscroll-step 1
     scroll-conservatively 1000)
    (setq coding-system-for-read 'utf-8 ) 
    (setq coding-system-for-write 'utf-8 )

    (setq compilation-always-kill t)
    (setq shell-file-name "zsh")
    (setq shell-command-switch "-ic")

    (setq inhibit-startup-screen +1)
    (setq initial-major-mode 'org-mode)
    (setq initial-scratch-message nil)


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

(add-hook 'go-mode-hook '(lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                    (let ((file (file-name-nondirectory buffer-file-name)))
                        (format "go run %s"
                                 file))))))

;; Compilation mode
(add-hook 'compilation-mode-hook '(lambda ()
                                    (local-unset-key "g")
                                    (local-unset-key "h")
                                    (evil-define-key 'motion compilation-mode-map "r" 'recompile)
                                    (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)
                                    (evil-define-key 'motion compilation-mode-map "e" 'kill-compilation)
                                    ))

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
