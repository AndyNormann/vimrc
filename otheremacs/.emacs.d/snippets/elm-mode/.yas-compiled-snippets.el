;;; Compiled snippets and support files for `elm-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'elm-mode
                     '(("alias" "type alias $1 =\n  { $0\n  }\n" "type-alias" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/andy_normann/.emacs.d/snippets/elm-mode/type-alias" nil nil)
                       ("module" "module $1 ($2) where\n\n$0" "module" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/andy_normann/.emacs.d/snippets/elm-mode/module" nil nil)
                       ("import" "import $1 exposing ($0)" "import" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/andy_normann/.emacs.d/snippets/elm-mode/import" nil nil)
                       ("function" "$1 : $2\n$1 $3 =\n  $0" "function" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/andy_normann/.emacs.d/snippets/elm-mode/function" nil nil)
                       ("div" "${1:div} [] [$0]" "div" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/andy_normann/.emacs.d/snippets/elm-mode/div" nil nil)
                       ("docstring" "{-| $0\n-}" "docstring" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/andy_normann/.emacs.d/snippets/elm-mode/comment" nil nil)))


;;; Do not edit! File generated at Fri Oct 21 12:21:30 2016
