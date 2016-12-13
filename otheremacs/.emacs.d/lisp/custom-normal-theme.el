;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face nil
        'help-echo (buffer-file-name)))

    ;; line and column
    "%02l" 

    ;; the current major mode for the buffer.

    " "


    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat " "  (propertize "+"
                             'face nil
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat " "  (propertize "RO"
                             'face nil
                             'help-echo "Buffer is read-only"))))
    ))

(provide 'custom-normal-theme)
