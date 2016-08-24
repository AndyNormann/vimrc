;; (add-hook 'rust-mode-hook 'racer-mode)
;; (add-hook 'racer-mode-hook 'eldoc-mode)
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
                                    (evil-define-key 'motion compilation-mode-map "µ" 'switch-to-compilation-buffer)
                                    (evil-define-key 'motion compilation-mode-map "m" 'switch-to-compilation-buffer)
                                    ))


(add-hook 'elm-mode-hook
          (lambda()
            (interactive)
            (elm-indent-mode)))

(provide 'mode-hooks)
