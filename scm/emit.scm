; -*- mode: scheme -*-
(use-modules (ice-9  match))
(load "base-guile.scm")

(define *top-context* (make-parameter #t))
(define *type-context* (make-parameter #f))

;; General functions

(define (void)
  (if #f #f))

(define (void? obj)
  (unspecified? obj))

(define (char-mangle c)
  (case c
    ((#\!) "ZA")
    ((#\#) "ZC")
    ((#\$) "ZD")
    ((#\%) "ZE")
    ((#\&) "ZF")
    ((#\*) "ZH")
    ((#\+) "ZI")
    ((#\,) "ZJ")
    ((#\-) "ZK")
    ;((#\.) "ZL")
    ((#\/) "ZM")
    ((#\:) "ZN")
    ((#\;) "ZO")
    ((#\<) "ZP")
    ((#\=) "ZQ")
    ((#\>) "ZR")
    ((#\?) "ZS")
    ((#\@) "ZT")
    ((#\^) "ZV")
    ((#\`) "ZW")
    ((#\|) "ZX")
    ((#\~) "ZY")
    ((#\Z) "ZZ")
    (else (string c))))

(define (char-unmangle c)
  (case c
    ((#\A) "!")
    ((#\C) "#")
    ((#\D) "$")
    ((#\E) "%")
    ((#\F) "&")
    ((#\H) "*")
    ((#\I) "+")
    ((#\J) ",")
    ((#\K) "-")
    ;((#\L) ".")
    ((#\M) "/")
    ((#\N) ":")
    ((#\O) ";")
    ((#\P) "<")
    ((#\Q) "=")
    ((#\R) ">")
    ((#\S) "?")
    ((#\T) "@")
    ((#\V) "^")
    ((#\W) "`")
    ((#\X) "|")
    ((#\Y) "~")
    ((#\Z) "Z")
    (else (string c))))

(define (string->mangle str)
  (string-join (map char-mangle (string->list str)) ""))

(define (symbol->mangle sym)
  (string->mangle (symbol->string sym)))

(define (string-escape s)
  s)

(define (go-encoded? name)
  (if (>= (string-length name) 2)
      (let ((c0 (string-ref name 0))
            (c1 (string-ref name 1)))
        (and (eqv? c0 #\Z) (not (eqv? c1 #\Z))))
      #f))

(define (go-public? name)
  (ds-function-name-public? name))

(define (go-keyword? kw)
  (define *table* '(
    append
    bool
    break
    byte
    cap
    case
    chan
    close
    complex
    complex128
    complex64
    const
    continue
    copy
    default
    defer
    delete
    else
    error
    fallthrough
    false
    float32
    float64
    for
    func
    go
    goto
    if
    imag
    import
    int
    int16
    int32
    int64
    int8
    interface
    iota
    len
    make
    map
    new
    nil
    package
    panic
    print
    println
    range
    real
    recover
    return
    rune
    select
    string
    struct
    switch
    true
    type
    uint
    uint16
    uint32
    uint64
    uint8
    uintptr
    var))
  (memv (string->symbol kw) *table*))

;; Emit functions

(define (emit-char ch)
  (string-append "'" (string-escape (string ch)) "'"))

(define (emit-string st)
  (string-append "\"" (string-escape st) "\""))

;(define (emit-symbol id)
;  (symbol->string id))
(define (emit-symbol id)
  (define *table* '(
          ; types
          ("&bool" . "bool")
          ("&byte" . "byte")
          ("&complex64" . "complex64")
          ("&complex128" . "complex128")
          ("&error" . "error")
          ("&float32" . "float32")
          ("&float64" . "float64")
          ("&imm-string" . "string")
          ("&int" . "int")
          ("&int8" . "int8")
          ("&int16" . "int16")
          ("&int32" . "int32")
          ("&int64" . "int64")
          ("&rune" . "rune")
          ("&uint" . "uint")
          ("&uint8" . "uint8")
          ("&uint16" . "uint16")
          ("&uint32" . "uint32")
          ("&uint64" . "uint64")
          ("&uintptr" . "uintptr")
          ("&void" . "")
          ; objects
          ("%nil" . "nil")))
  (if (symbol? id)
      (let ((s (symbol->string id)))
        (if (and (or (eqv? (string-ref s 0) #\%)
                     (eqv? (string-ref s 0) #\&))
                 (assoc s *table*))
            (cdr (assoc s *table*))
            (symbol->mangle id)))
            ;(let ((out (symbol->string id)))
            ;  (if (or (go-keyword? out)
            ;          (go-encoded? out))
            ;      (string-append "__" out)
            ;      out))))
      (error id)))

(define (emit-literal vec)
  (let ((l (vector->list vec)))
    (string-append 
     (emit-expr (car l)) "{" 
     (emit-exprs (cdr l)) "}")))

(define (emit-params fields)
  (parameterize ((*type-context* #t))
   (string-join (map emit-field fields) ", ")))

(define (emit-fields fields)
  (string-join (map emit-field fields) "\n"))

(define (emit-field field)
  (if (vector? field)
      (let ((ms (reverse (cdr (reverse (vector->list field)))))
            (ls (vector-last field)))
        (string-append (emit-exprs ms) " " (emit-expr ls)))
      (emit-expr field)))

(define (emit-field... field)
  (if (vector? field)
      (let ((ms (reverse (cdr (reverse (vector->list field)))))
            (ls (vector-last field)))
        (string-append (emit-exprs ms) " ..." (emit-expr ls)))
      (string-append " ..." (emit-expr field))))

(define (emit-short op vars . vals)
  (emit-assign op vars vals))

(define (emit-assign op vars vals)
  ;(write (list 'emit-assign op vars vals))
  (string-append
    (cond
      ;; string means cached output
      ((string? vars) vars)
      ((symbol? vars) (emit-symbol vars))
      ((vector? vars) (emit-fields vars))
      ((list? vars) (emit-exprs vars))
      (else (error "assign-stmt expected list or symbol" vars)))
    " "
    (cond
     ((string? op) op)
     ((symbol? op) (symbol->string op))
     (else (error "assign-stmt expected string or symbol" op)))
    " "
    (emit-exprs vals)))

(define (emit-binary op vals)
  (string-join (map emit-expr vals)
    (string-append " " op " ")))

(define (emit-parens kw proc rest)
  (if (= (length rest) 1)
      (string-append kw " " (proc rest) "\n")
      (string-append kw " (\n\t" (proc rest) "\n)\n")))

(define (emit-imports specs)
  (define emit-spec emit-string)
  (define (emit spec)
    (cond
     ((string? spec)
      (emit-spec spec))
     ((list? spec)
      (case (car spec)
        (('as) (string-append (emit-symbol (cadr spec)) " " (emit-spec (caddr spec))))
        (('dot) (string-append ". " (emit-spec (cadr spec))))
        (else (error "import expected list or string"))))))
  (string-join (map emit specs) "\n"))

(define (emit-type-parens kw proc rest)
  (if (= (length rest) 2)
      (string-append kw " " (proc rest) "\n")
      (string-append kw " (\n\t" (proc rest) "\n)\n")))

(define (emit-types specs)
  (define emit1 emit-expr)
  (define (emit2 name spec . specs)
    (let ((s (string-append (emit1 name) " " (emit1 spec))))
      (if (null? specs)
          (list s)
          (cons s (apply emit2 specs)))))
  (string-join (apply emit2 specs) "\n"))

(define (emit-values specs)
  (define (emit spec)
    (if (and (list? spec) (eqv? (car spec) '=))
        (apply emit-short spec)
        (emit-field spec)))
  (string-join (map emit specs) "\n"))

(define (emit-cases clauses)
  (define (emit-case rest)
    (let ((exprs (car rest))
          (stmts (cdr rest)))
      (if (eqv? exprs 'else)
          (string-append "default:\n" (emit-stmts stmts))
          (string-append "case " (emit-exprs exprs) ":\n" (emit-stmts stmts)))))
  (string-append "{\n" (string-join (map emit-case clauses) "\n") "\n}\n"))

(define (emit-conds clauses)
  (define (emit-cond rest)
    (let ((expr (car rest))
          (stmts (cdr rest)))
      (if (eqv? expr 'else)
          (string-append "default:\n" (emit-stmts stmts))
          (string-append "case " (emit-expr expr) ":\n" (emit-stmts stmts)))))
  (string-append "{\n" (string-join (map emit-cond clauses) "\n") "\n}\n"))

(define (emit-sig ins ret)
  (string-append "(" 
    (emit-params ins) ")" 
    (emit-expr ret)))

(define (emit-sig... ins ret)
  (string-append "(" 
    (string-join (append 
     (emit-params (most ins)) 
     (list (emit-field... (last ins)))) ", ")
    ")"
    (emit-expr ret)))

(define (emit-as a b . rest)
  (if (null? rest)
      (string-append (emit a) ".(" (emit-type-name b) ")")
      (apply emit-dot (list 'as a b) rest)))

(define (emit-else-block stmts)
  (if (null? stmts)
      "{}"
      (let ((ms (most stmts))
            (ls (last stmts)))
        (if (eqv? (car ls) 'else)
            (string-append "{\n\t" 
             (emit-stmts ms) "\n} else {\n" 
             (emit-stmts (cdr ls)) "\n}\n")
            (emit-block stmts)))))

(define (emit-block stmts)
  (if (null? stmts)
      "{}"
      (string-append "{\n\t" (emit-stmts stmts) "\n}\n")))

(define (emit-decls decls)
  (define (emit decl)
    (emit-expr decl))
  (string-join (map emit decls) "\n\n"))

(define (emit-stmts stmts)
  (define (emit stmt)
    (emit-expr stmt))
  (string-join (map emit stmts) "\n"))

(define (emit-exprs exprs)
  (string-join (map emit-expr exprs) ", "))

(define (emit-expr expr)
  ;(when (string? expr)
  ;      (display (string-append "emit-str " expr "\n")))
  ;(when (symbol? expr)
  ;      (display (string-append "emit-sym " (symbol->string expr) "\n")))
  ;(when (pair? expr)
  ;      (display (string-append "emit-lst (" (symbol->string (car expr)) " ...)\n")))
  (cond
    ((pair? expr) (apply apply-go expr))
    ((char? expr) (emit-char expr))
    ((boolean? expr) (if expr "true" "false"))
    ((number? expr) (number->string expr))
    ((string? expr) (emit-string expr))
    ((symbol? expr) (emit-symbol expr))
    ((vector? expr) (emit-literal expr))
    ((null? expr) "_null()")
    (else (error "emit unrecognized type"))))

(define (map-go sy xs)
  (define (fn x) (apply apply-go sy x))
  (map fn xs))
  
(define (join-go . rest)
  (cond
   ((null? rest) "")
   ((pair? rest)
    (let* ((expr (car rest))
           (exprs (cdr rest)))
      (string-append
       (cond
        ((string? expr) expr)
        ;((and (list? expr)
        ;      (eqv? (car expr) 'map))
        ; (let ((sy (cadr expr))
        ;       (xs (caddr expr)))
        ;   (string-join (map-go sy xs) " ")))
        (else (emit-expr expr)))
       " "
       (apply join-go exprs))))
   (else (error "unexpected"))))

(define (apply-go . expr)
  (define (do-match)
    (let ((m (apply *rules* expr)))
      (cond ((list? m) (apply join-go m))
            ((string? m) m)
            (else #f)))) ;(error "apply-go unexpected" expr m)))))
  (define (yes-match key . args) #f);(error "yes-match" args))
  (define (no-match key . args) #f);(error "no-match" args))
  (let ((t (catch 'misc-error 
                  (lambda () (catch 'match-error do-match no-match))
                  yes-match)))
    (if t t (apply apply-go 'apply expr))))
