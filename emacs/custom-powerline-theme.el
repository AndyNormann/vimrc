(defun custom-powerline-evil-theme ()
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (quote utf-8)
                                                          (car powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (concat " " (powerline-evil-tag) " ") evil-face)))
                                     (funcall separator-left (powerline-evil-face) mode-line)
                                     (powerline-buffer-id `(mode-line-buffer-id ,mode-line) mode-line)
                                     (powerline-raw " %*" nil mode-line)
                                     (when buffer-read-only
                                       (powerline-raw " RO " mode-line))
                                     (powerline-raw " %l " mode-line mode-line)
                                     )))
                     (powerline-render lhs))))))

(provide 'custom-powerline-theme)
