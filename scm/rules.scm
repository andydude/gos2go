; -*- mode: scheme -*-
(use-modules (ice-9  match))

(define (*rules* . expr)
  (match expr

    ;; binary operators
    (('!= . vals)      (apply emit-binary expr))
    (('% . vals)       (apply emit-binary expr))
    (('%= vars . vals) (apply emit-assign expr))
    (('* . vals)       (apply emit-binary expr))
    (('*= vars . vals) (apply emit-assign expr))
    (('+ . vals)       (apply emit-binary expr))
    (('+= vars . vals) (apply emit-assign expr))
    (('- . vals)       (apply emit-binary expr))
    (('-= vars . vals) (apply emit-assign expr))
    (('/ . vals)       (apply emit-binary expr))
    (('/= vars . vals) (apply emit-assign expr))
    ((':= vars . vals) (apply emit-assign expr))
    (('< . vals)       (apply emit-binary expr))
    (('<< . vals)      (apply emit-binary expr))
    (('<<= vars . vals)(apply emit-assign expr))
    (('<= . vals)      (apply emit-binary expr))
    (('= vars . vals)  (apply emit-assign expr))
    (('== . vals)      (apply emit-binary expr))
    (('> . vals)       (apply emit-binary expr))
    (('>= . vals)      (apply emit-binary expr))
    (('>> . vals)      (apply emit-binary expr))
    (('>>= vars . vals)(apply emit-assign expr))
    (('and . vals)     (apply emit-binary expr))
    (('dot . vals)     (apply emit-binary expr))
    (('or . vals)      (apply emit-binary expr))

    ;; bitwise operators
    (('bitwise-and . vals)       (apply emit-binary expr))
    (('bitwise-and= vars . vals) (apply emit-assign expr))
    (('bitwise-but . vals)       (apply emit-binary expr))
    (('bitwise-but= vars . vals) (apply emit-assign expr))
    (('bitwise-or . vals)        (apply emit-binary expr))
    (('bitwise-or= vars . vals)  (apply emit-assign expr))
    (('bitwise-xor . vals)       (apply emit-binary expr))
    (('bitwise-xor= vars . vals) (apply emit-assign expr))

    ;; other operators
    (('++ expr) `(,expr "++"))
    (('-- expr) `(,expr "--"))
    ((': key value) `(,key ": " ,value))
    (('<- chan) `("<-" ,chan))
    (('<-! chan expr) `(,chan "<-" ,expr))
    (('adr expr) `("&" ,expr))
    (('as . body) (apply emit-as body))
    (('not expr) `("!" ,expr))
    (('ptr expr) `("*" ,expr))

    ;; keywords
    (('apply fn . args)
     `(,fn "(" ,(emit-exprs args) ")"))
    (('apply... fn . args)
     `(,fn "(" ,(emit-exprs args) "...)"))
    (('array length type)
     `("[" ,length "]" ,type))
    (('values . types)
     `("(" ,(emit-params types) ")"))
    (('array... type)
     `("[...]" ,type))
    (('slice type)
     `("[]" ,type))
    (('struct . fields)
     `("struct" "{\n" ,(emit-fields fields) "\n}\n"))
    (('map-type key-type type)
     `("map" "[" ,key-type "]" ,type))
    (('chan type) `("chan" ,type))
    (('chan<- type) `("<-chan" ,type))
    (('chan<-! type) `("chan<-" ,type))
    (('interface . methods)
     `("interface" "{\n" ,(emit-fields methods) "\n}\n"))

    (('break . rest) (emit-branch 'break rest))
    (('continue . rest) (emit-branch 'continue rest))
    (('fallthrough . rest) (emit-branch 'fallthrough rest))
    (('goto . rest) (emit-branch 'goto rest))

    (('for a b c . body)
     `("for" ,a ";" ,b ";" ,c ,(emit-block body)))
    (('while c . body)
     `("for" ,c ,(emit-block body)))
    (('range a . body)
     `("for" ,(emit-range a) ,(emit-block body)))

    (('branch-stmt kw . label)
     `(kw ,@label))
    (('package name . decls)
     `("package" ,name "\n" ,(emit-decls decls)))
    (('return . expr)
     `("return" ,(emit-exprs expr)))
    (('defer expr)
     `("defer" ,expr))

    ;; The "if/else" keywords
    (('when expr . body)
     `("if" ,expr ,(emit-else-block body)))
    (('when* stmt expr . body)
     `("if" ,stmt ";" ,expr ,(emit-else-block body)))
    (('unless expr . body)
     `("if !" ,expr ,(emit-else-block body)))
    (('unless* stmt expr . body)
     `("if" ,stmt "; !" ,expr ,(emit-else-block body)))


    ;; The "switch/select" keywords
    (('case! expr . body)
     `("switch" ,expr ,(emit-cases body)))
    (('case!* stmt expr . body)
     `("switch" ,stmt ";" ,expr ,(emit-cases body)))
    (('comm! . body)
     `("select" ,(emit-conds body)))
    (('cond! . body)
     `("switch" ,(emit-conds body)))
    (('cond!* stmt . body)
     `("switch" stmt ";" ,(emit-conds body)))
    (('type! expr . body)
     `("switch" ,expr ,(emit-cases body)))
    (('type!* stmt expr . body)
     `("switch" ,stmt ";" ,expr ,(emit-cases body)))

    (('index expr j . ks)
     (let ((offset (lambda (k) (if (not k) "" (emit-expr k)))))
       (if (pair? ks)
           `(,expr "[" ,(offset j) ":" ,(offset (car ks)) "]")
           `(,expr "[" ,(offset j) "]"))))

    (('import . specs)
     (emit-parens "import" emit-imports specs))
    (('type . specs)
     (emit-type-parens "type" emit-types specs))
    (('const . specs)
     (emit-parens "const" emit-values specs))
    (('var . specs)
     (emit-parens "var" emit-values specs))

    (('define-func name sig ret . body)
     (if (*top-context*)
         `("func" ,name ,(emit-sig sig ret) ,(emit-block body)) ; FuncDecl
         `("var" ,name " = func" ,(emit-sig sig ret) ,(emit-block body)))) ; FuncStmt

    (('define-func... name sig ret . body)
     (if (*top-context*)
         `("func" ,name ,(emit-sig... sig ret) ,(emit-block body)) ; FuncDecl
         `("var" ,name " = func" ,(emit-sig... sig ret) ,(emit-block body)))) ; FuncStmt

    (('define-method-func rec name sig ret . body)
     `("func (" ,(emit-field rec) ")" ,name 
                ,(emit-sig sig ret) 
                ,(emit-block body)))

    (('define-method-func... rec name sig ret . body)
     `("func (" ,(emit-field rec) ")" ,name 
                ,(emit-sig... sig ret) 
                ,(emit-block body)))

    (('lambda-func sig ret . body)
     (if (*type-context*)
         `("func" ,(emit-sig sig ret)) ; FuncType
         `("func" ,(emit-sig sig ret) 
                  ,(emit-block body)))) ; FuncExpr

    (('lambda-func... sig ret . body)
     (if (*type-context*)
         `("func" ,(emit-sig... sig ret)) ; FuncType
         `("func" ,(emit-sig... sig ret) 
                  ,(emit-block body)))) ; FuncExpr

    ;; The glorious "func" type-switch!
    (('func . rest)
     (cond
      ((vector? (car rest)) `((define-method-func ,@rest)))
      ((symbol? (car rest)) `((define-func ,@rest)))
      ((list? (car rest)) `((lambda-func ,@rest)))
      (else (error "func expected symbol, vector, or list"))))

    (('func... . rest)
     (cond
      ((vector? (car rest)) `((define-method-func... ,@rest)))
      ((symbol? (car rest)) `((define-func... ,@rest)))
      ((list? (car rest)) `((lambda-func... ,@rest)))
      (else (error "func... expected symbol, vector, or list"))))

  );match
);define
