;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("v" ":= $2" "v" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/v" nil nil)
                       ("make" "${1:name} := make([]${2:type}, 0)$0" "make" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/make" nil nil)
                       ("main" "package main\n\nimport(\n    \"fmt\"\n)\n\nfunc main() {\n    $0\n}" "main" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/main" nil nil)
                       ("fori" "for ${1:i} := 0; $1 < $2; $1++ {\n  $0\n}\n" "for i" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/fori" nil nil)
                       ("fore" "for ${1:it}, ${2:elem} := range($3) {\n  $0\n}\n" "for range" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/fore" nil nil)
                       ("app" "${1:name} = append($1, ${2:value})$0\n" "append slice" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/append" nil nil)))


;;; Do not edit! File generated at Fri Oct 21 12:21:30 2016
