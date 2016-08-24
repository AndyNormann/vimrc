(evil-leader/set-key
  "m" 'recompile-quietly
  "r" 'shell-command
  "w" 'evil-write
  )


(define-key evil-normal-state-map (kbd "RET") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "C-k") 'comment-or-uncomment-region)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-e") 'helm-find-files)
(define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)


(if (display-graphic-p)
    (define-key evil-normal-state-map (kbd "M-m") 'switch-to-compilation-buffer)
  (define-key evil-normal-state-map (kbd "µ") 'switch-to-compilation-buffer))
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


(provide 'evil-setup)
