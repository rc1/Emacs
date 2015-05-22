;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("#" "function ($1) { $0 }" "anonymous function using #" nil nil nil nil nil nil)
                       ("f" "function ($1) { $0 }" "anonymous function" nil nil nil nil nil nil)
                       ("fn" "function $1 ($2) { $3 }" "named function" nil nil nil nil nil nil)
                       ("h" "/* jshint unused: false, -W117 */\n\"use strict\";" "jshint options" nil nil nil nil nil nil)
                       ("if" "if ( ${1} ) { $0 }" "if statement" nil nil nil nil nil nil)
                       ("l" "console.log( $0 );" "console log" nil nil nil nil nil nil)
                       ("p" "return W.promise( function ( resolve, reject ) {\n    $0\n});\n" "promise" nil nil nil nil nil nil)
                       ("q" "'$0'" "quote" nil nil nil nil nil nil)
                       ("r" "return$0;" "return" nil nil nil nil nil nil)
                       ("try" "try {\n    $0\n} catch ( e ) {\n    $1\n}" "try catch" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu May 14 17:26:28 2015
