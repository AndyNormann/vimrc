(let ((gc-cons-threshold most-positive-fixnum))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'evil)
(evil-mode 1)

(global-evil-leader-mode 1)
(evil-leader/set-leader ",")

(require 'evil-setup)

;; (require 'helm-config)
;; (helm-mode 1)

(show-paren-mode t)

(highlight-numbers-mode 1)

(when (not (window-system))
  (xterm-mouse-mode +1))

(require 'yasnippet)
(yas-global-mode 1)

(require 'use-package)

(require 'company-simple-complete)

(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)

(set-variable 'ycmd-server-command '("python" "/Users/andy_normann/.emacs.d/ycmd/ycmd/ycmd/__main__.py"))
(set-variable 'ycmd-global-config "~/.vim/.ycm_extra_conf.py")

(require 'company-ycmd)
(company-ycmd-setup)

(use-package company
  :defer t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    (setq company-idle-delay 0
          company-minimum-prefix-length 0
          company-selection-wrap-around t
          company-show-numbers nil
          company-global-modes t
          company-dabbrev-downcase nil
          )))
(with-eval-after-load 'company (define-key company-active-map [tab] nil))
(with-eval-after-load 'company (define-key company-active-map (kbd "TAB") nil))
(with-eval-after-load 'company (define-key company-active-map (kbd "RET") nil))

(require 'mode-hooks)

(require 'elm-mode)


(if (display-graphic-p)
    (scroll-bar-mode -1)
  )

(if (display-graphic-p)
    (fringe-mode 0)
  )

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(size-indication-mode 0)
(setq backup-directory-alist `(("~/.emacs-backup")))
(setq make-backup-files nil)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace")
(xclip-mode t)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

(add-to-list 'default-frame-alist '(font . "PragmataPro for Powerline 16" ))
(set-face-attribute 'default t :font "PragmataPro for Powerline 16" )



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

(setq compilation-always-kill t)
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")


(setq inhibit-startup-screen +1)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

)

(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file)


;; Set theme
(load-theme 'smyx)
(setq sml/theme 'dark)
(if (display-graphic-p)
    (setq sml/theme 'dark)
  )

(sml/setup)




