(let ((gc-cons-threshold most-positive-fixnum))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)

(global-evil-leader-mode 1)
(evil-leader/set-leader ",")

(require 'helm-config)
(helm-mode 1)

(show-paren-mode t)

(highlight-numbers-mode 1)

(when (not (window-system))
  (xterm-mouse-mode +1))


(evil-leader/set-key
  "m" 'recompile-quietly
  "r" 'shell-command
  "w" 'evil-write
  )

(define-key evil-insert-state-map (kbd "C-j") 'company-select-next)
(define-key evil-insert-state-map (kbd "C-k") 'company-complete-selection)

(define-key evil-visual-state-map (kbd "C-k") 'comment-or-uncomment-region)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-e") 'helm-find-files)
(define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)

(define-key evil-normal-state-map (kbd "C-m") 'switch-to-compilation-buffer)
(define-key compilation-mode-map (kbd "C-m") 'switch-to-compilation-buffer)
(define-key evil-normal-state-map (kbd "C-t") 'switch-to-term-or-back)
(define-key evil-insert-state-map (kbd "C-t") 'switch-to-term-or-back)

(define-key evil-normal-state-map (kbd "J") (lambda ()
                                              (interactive)
                                              (evil-next-visual-line nil)
                                              (evil-next-visual-line nil)
                                              (evil-next-visual-line nil)
                                              (evil-next-visual-line nil)
                                              (evil-next-visual-line nil)))

(define-key evil-normal-state-map (kbd "K") (lambda ()
                                              (interactive)
                                              (evil-previous-visual-line nil)
                                              (evil-previous-visual-line nil)
                                              (evil-previous-visual-line nil)
                                              (evil-previous-visual-line nil)
                                              (evil-previous-visual-line nil)))


(require 'yasnippet)
(yas-global-mode 1)

(require 'use-package)

(use-package company
  :defer t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 0
          company-selection-wrap-around t
          company-show-numbers nil
          company-global-modes t
          company-dabbrev-downcase nil
          )
    (bind-keys :map company-active-map
               ("C-j" . company-select-next)
               ("C-k" . company-complete))))

(with-eval-after-load 'company (define-key company-active-map [tab] nil))
(with-eval-after-load 'company (define-key company-active-map (kbd "TAB") nil))
(with-eval-after-load 'company (define-key company-active-map (kbd "RET") nil))


(use-package company-irony
             :defer t
             :init (with-eval-after-load 'company(add-to-list 'company-backends 'company-irony)))
(use-package company-irony-c-headers
             :defer t
             :init (with-eval-after-load 'company (add-to-list 'company-backends 'company-irony-c-headers)))
(use-package company-go
             :defer t
             :init (with-eval-after-load 'company (add-to-list 'company-backends 'company-go)))
(use-package company-racer
             :defer t
             :init (with-eval-after-load 'company (add-to-list 'company-backends 'company-racer))
             :config
             (progn
               (setq racer-cm "~/.cargo/bin/racer")
               (setq racer-rust-src-path "/Users/andy_normann/.scripts/rust-master/src")))


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(company-idle-delay t)
 ;; '(company-minimum-prefix-length 0)
 '(custom-safe-themes
   (quote
    ("4de3565231fbe2d69afbd64af55b4e1827c3cda72c637249c081441d34fcc24b" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "a164837cd2821475e1099911f356ed0d7bd730f13fa36907895f96a719e5ac3e" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "81c42af8c9099159081eb475d8c94a2fa3df561cd2cdc41b0487b42f11f52372" "48996f13002fa5a50ae5f3c5317081c95543675b368f81a500542c00dc57a821" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default)))
 ;; '(global-company-mode t)
 '(irony-cmake-executable "/usr/local/Cellar/cmake/3.5.2/bin/cmake")
 '(sml/mode-width nil)
 '(sml/position-percentage-format "")
 '(sml/size-indication-format ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/col-number ((t (:inherit sml/global))))
 '(sml/modes ((t (:inherit sml/filename :weight normal))))
 '(sml/position-percentage ((t (:inherit sml/prefix :weight normal)))))



(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
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
                     (format "clang -o %s %s -Weverything && ./%s"
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


;; GO STUFF
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

(add-hook 'compilation-mode-hook '(lambda ()
                                    (local-unset-key "g")
                                    (local-unset-key "h")
                                    (local-unset-key "C-m")
                                    (evil-define-key 'motion compilation-mode-map "r" 'recompile)
                                    (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)
                                    ))

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

(setq compilation-always-kill t)
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")


  ;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
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

  (evil-define-key 'insert org-mode-map (kbd "RET") #'org-return)


  (setq inhibit-startup-screen +1)
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message nil)

  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("c697b65591ba1fdda42fae093563867a95046466285459bd4e686dc95a819310" "1012cf33e0152751078e9529a915da52ec742dabf22143530e86451ae8378c1a" "a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "737d9d0e0f6c4279e80f7479ec5138af6e4908a2d052126f254e1e6d1a0d0188" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4de3565231fbe2d69afbd64af55b4e1827c3cda72c637249c081441d34fcc24b" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "a164837cd2821475e1099911f356ed0d7bd730f13fa36907895f96a719e5ac3e" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "81c42af8c9099159081eb475d8c94a2fa3df561cd2cdc41b0487b42f11f52372" "48996f13002fa5a50ae5f3c5317081c95543675b368f81a500542c00dc57a821" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default)))
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-smart eshell-term eshell-unix)))
 '(fci-rule-color "#eee8d5")
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*compilation" "\\*Messages" "\\*clang*" "\\*Customize*")))
 '(helm-buffers-fuzzy-matching t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(irony-cmake-executable "/usr/local/Cellar/cmake/3.5.2/bin/cmake")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (highlight-numbers white-sand-theme shell-toggle color-theme-approximate smyx-theme ample-theme hydandata-light-theme badwolf-theme espresso-theme solarized-theme xclip workgroups volatile-highlights use-package smart-mode-line rainbow-delimiters racer powerline ox-twbs highlight-symbol helm gruvbox-theme go flycheck-irony exec-path-from-shell evil-leader elm-yasnippets elm-mode company-racer company-irony-c-headers company-irony company-go clojure-mode autopair)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/mode-width nil)
 '(sml/position-percentage-format "")
 '(sml/size-indication-format "")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c37300")
     (60 . "#b97d00")
     (80 . "#b58900")
     (100 . "#a18700")
     (120 . "#9b8700")
     (140 . "#948700")
     (160 . "#8d8700")
     (180 . "#859900")
     (200 . "#5a942c")
     (220 . "#439b43")
     (240 . "#2da159")
     (260 . "#16a870")
     (280 . "#2aa198")
     (300 . "#009fa7")
     (320 . "#0097b7")
     (340 . "#008fc7")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/col-number ((t (:inherit sml/global))))
 '(sml/modes ((t (:inherit sml/filename :weight normal))))
 '(sml/position-percentage ((t (:inherit sml/prefix :weight normal))))
 '(term ((t (:inherit default :background "#242424" :foreground "brightwhite"))))
 '(term-color-black ((t (:background "#202020" :foreground "#282828"))))
 '(term-color-blue ((t (:background "blue" :foreground "blue"))))
 '(term-color-green ((t (:background "#9fc59f" :foreground "#9fc59f"))))
 '(term-color-yellow ((t (:background "#F6DC69" :foreground "#F6DC69")))))

(load-theme 'white-sand)

(setq sml/theme 'respectful)
(if (display-graphic-p)
    (setq sml/theme 'respectful)
  )

(sml/setup)

(defun init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


