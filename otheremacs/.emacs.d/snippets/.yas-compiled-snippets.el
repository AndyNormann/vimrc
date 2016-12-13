;;; Compiled snippets and support files for `snippets'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("type" "typedef struct ${1:}\n{\n        ${0:}\n} $1;" "typedef struct name {};" nil nil nil "/Users/andy_normann/.emacs.d/snippets/c-mode/typedef" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
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


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("v" ":= $2" "v" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/v" nil nil)
                       ("make" "${1:name} := make([]${2:type}, 0)$0" "make" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/make" nil nil)
                       ("main" "package main\n\nimport(\n    \"fmt\"\n)\n\nfunc main() {\n    $0\n}" "main" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/main" nil nil)
                       ("fori" "for ${1:i} := 0; $1 < $2; $1++ {\n  $0\n}\n" "for i" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/fori" nil nil)
                       ("fore" "for ${1:it}, ${2:elem} := range($3) {\n  $0\n}\n" "for range" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/fore" nil nil)
                       ("app" "${1:name} = append($1, ${2:value})$0\n" "append slice" nil nil nil "/Users/andy_normann/.emacs.d/snippets/go-mode/append" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while {$1:cond} {\n    {$2:()}\n}$0" "while cond { … }" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/while" nil nil)
                       ("warn" "#[warn(${1:lint_check})]$0" "#[warn(lint_check)]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/warn" nil nil)
                       ("use" "use name;" "use name;" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/use" nil nil)
                       ("unsafe" "unsafe {\n    ${1:()}\n}$0" "unsafe { … }" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/unsafe" nil nil)
                       ("type" "type ${1:Name} = ${2:Type};$0" "type Name = Type;" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/type" nil nil)
                       ("test" "#[test]$0\n" "#[test]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/test" nil nil)
                       ("st" "struct ${1:name} {\n   ${2:}\n}" "struct" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/struct" nil nil)
                       ("static" "static ${1:NAME}: ${2:Type} = ${3:expr};$0" "static NAME: Type = expr;" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/static" nil nil)
                       ("pr" "println!(\"${1:}\"${2:});${0:}" "println!()" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/pr" nil nil)
                       ("mod" "mod ${1:name} {\n    ${2:()}\n}$0" "mod name { … }" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/mod" nil nil)
                       ("main" "pub fn main() {\n    ${0:}\n}" "fn main() { … }" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/main" nil nil)
                       ("loop" "loop {\n    ${1:()}\n}$0" "loop { … }" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/loop" nil nil)
                       ("lettm" "let mut ${1:name}: ${2:type} = ${3:};" "let mut : type =" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/lettm" nil nil)
                       ("lett" "let ${1:name}: ${2:type} = ${3:};" "let : type =" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/lett" nil nil)
                       ("letm" "let mut ${1:name} = ${2:};" "let mut" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/letm" nil nil)
                       ("let" "let ${1:name} = ${2:};" "let" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/let" nil nil)
                       ("forbid" "#[forbid(${1:lint_check})]$0" "#[forbid(lint_check)]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/forbid" nil nil)
                       ("for" "for ${1:name} in ${2:iterable} {\n    ${3:()}\n}$0\n" "for name in iterable { … }" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/for" nil nil)
                       ("deriving" "#[deriving(${1:Trait})]$0" "#[deriving(Trait)]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/deriving" nil nil)
                       ("deny" "#[deny(${1:lint_check})]$0" "#[deny(lint_check)]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/deny" nil nil)
                       ("const" "const ${1:NAME}: ${2:Type} = ${3:expr};$0" "const NAME: Type = expr;" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/const" nil nil)
                       ("cfg=" "#[cfg(${1:option} = \"${2:value}\")]$0" "#[cfg(option = \"value\")]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/cfg=" nil nil)
                       ("cfg" "#[cfg(${1:option})]$0" "#[cfg(option)]" nil nil nil "/Users/andy_normann/.emacs.d/snippets/rust-mode/cfg" nil nil)))


;;; Do not edit! File generated at Fri Oct 21 12:21:40 2016
