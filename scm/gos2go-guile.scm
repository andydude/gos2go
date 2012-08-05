(load "base-guile.scm")
(load "emit.scm")
(load "rules.scm")

(define *input-filename*
  (make-parameter "/dev/null"))

(define (main-guile)
  (begin
    (*input-filename* (list-ref (command-line) 1))
    ;(*package-name* (basename (*input-filename*)))
    ;(*package-path* (string-replace (*package-name*) "_" "/"))
    (display (emit-expr (call-with-input-file (*input-filename*) read)))
    (newline)))

(main-guile)
