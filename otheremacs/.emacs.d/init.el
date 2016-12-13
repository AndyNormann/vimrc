(let ((gc-cons-threshold most-positive-fixnum))

;;; Package init
  (require 'package)
  (setq package-enable-at-startup nil)

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

  (package-initialize)

;;; Custom.el
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (add-to-list 'load-path "~/.emacs.d/lisp/")

  (setq ad-redefinition-action 'accept)

;;; Plugins

  ; Make sure we have use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)

  ;; (if window-system
  ;;     (setq sml/theme 'light)
  ;;   )
  ;; (if window-system
  ;;     (sml/setup)
  ;;   )
  (if (not window-system)
      (use-package custom-powerline-theme
        :init
        (setq powerline-evil-tag-style (quote verbose))
        :config
        (custom-powerline-evil-theme)))

  (if (not window-system)
      (custom-set-faces
       '(mode-line ((t (:background "#010101" :foreground "#eaeaea" :box nil :weight normal))))
       )
    )

;(setq gruvbox-contrast 'hard)
;(load-theme 'gruvbox)
;(load-theme 'sanityinc-tomorrow-day)
(load-theme 'base16-bright)

(if window-system
    (load-theme 'sanityinc-solarized-light))

(use-package evil :ensure t
  :config
  (evil-mode 1)
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "SPC")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    "m" 'recompile-quietly
    "M" 'compile
    "r" 'shell-command
    "w" 'evil-write
    "l" 'switch-to-compilation-buffer
    "t" 'switch-to-term-or-back
    "b" 'ivy-switch-buffer
    "e" 'counsel-find-file
    "s" 'swiper
    "p" 'goto-plan
    "c" 'comment-or-uncomment-region)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-lines-up)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-lines-down)
  (define-key evil-insert-state-map (kbd "C-n") 'company-simple-complete-next)
  (define-key evil-insert-state-map (kbd "C-p") 'company-simple-complete-previous)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex))

(use-package org-evil :ensure t)

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode 1))

(defun evil-scroll-lines-up ()
  (interactive)
  (evil-scroll-up 5)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun evil-scroll-lines-down ()
  (interactive)
  (evil-scroll-down 5)
  (evil-scroll-line-to-center (line-number-at-pos)))

(use-package yasnippet 
  :init
  (setq yas-verbosity 0)
  (yas-global-mode 1))

(use-package all-the-icons :ensure t)


(use-package saveplace :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace")
  (save-place-mode 1))

(use-package ivy :ensure t
  :config
  (ivy-mode 1))

(use-package company :ensure t
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase 0)
  (setq company-dabbrev-ignore-case nil)
  (require 'company-simple-complete))

(require 'ox-reveal)
(setq org-reveal-root "file:///Users/andy_normann/.emacs.d/reveal.js-master/")

;;; General settings

(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode t)
(xclip-mode t)
(xterm-mouse-mode t)
(blink-cursor-mode 0)
(setq-default cursor-type 'box) 

(add-to-list 'default-frame-alist '(font . "Iosevka term 18" ))
(set-face-attribute 'default t :font "Iosevka term 18")

(when (display-graphic-p)
  (scroll-bar-mode -1)
  (fringe-mode -1)
  (toggle-frame-fullscreen))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq make-backup-files nil
      auto-save-default nil
      hscroll-step 1
      scroll-conservatively 1000
      global-auto-revert-mode 1
      tab-width 4
      c-basic-offset 4
      coding-system-for-read 'utf-8 
      coding-system-for-write 'utf-8 
      compilation-always-kill t
      shell-file-name "zsh"
      shell-command-switch "-ic"
      inhibit-startup-screen +1
      initial-major-mode 'org-mode
      initial-scratch-message nil
      ring-bell-function 'ignore 
      show-paren-delay 0
      gnuplot-program "/usr/local/Cellar/gnuplot/5.0.4/bin/gnuplot")

(setq-default indent-tabs-mode nil)


;;; Mode hooks
(add-hook 'term-mode-hook
          (lambda()
            (company-mode -1)
            (yas-minor-mode -1)))
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
                     (format "clang++ -o %s %s -Weverything -std=c++14 && ./%s"
                             (file-name-sans-extension file)
                             file
                             (file-name-sans-extension file)))))))

(add-hook 'go-mode-hook '(lambda ()
                           (setq tab-width 4)
                           (setq indent-tabs-mode nil)
                           (set (make-local-variable 'company-backends '(company-go)))
                           (require 'company-go)
                           (gofmt-before-save 1)
                           (if (not (string-match "go" compile-command))
                               (set (make-local-variable 'compile-command)
                                    (let ((file (file-name-nondirectory buffer-file-name)))
                                      (format "go run %s"
                                              file))))))
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(add-hook 'java-mode-hook
          (lambda ()
            (unless (file-exists-p "build.xml")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "javac *.java && java %s"
                             (file-name-sans-extension file)))))))

(add-hook 'org-mode-hook
          (lambda ()
            (company-mode -1)
              (define-key evil-normal-state-map (kbd "C-m") 'latex-export)
              (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
              (set (make-local-variable 'compile-command)
                   "pdflatexc ")))

(defun latex-export ()
  (interactive)
  (org-latex-export-to-pdf)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes)))))))

;; Compilation mode
(add-hook 'compilation-mode-hook '(lambda ()
                                    (local-unset-key "g")
                                    (local-unset-key "h")
                                    (evil-define-key 'motion compilation-mode-map "r" 'recompile)
                                    (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)
                                    (evil-define-key 'motion compilation-mode-map "e" 'kill-compilation)
                                    ))

(defun recompile-quietly ()
  (interactive)
  (save-window-excursion
    (recompile)))

(defun goto-plan ()
  (interactive)
  (if (string-match "plan.org" (buffer-name))
      (switch-to-prev-buffer)
    (find-file "~/plan.org")))

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
