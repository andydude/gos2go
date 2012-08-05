; -*- mode: scheme -*-
(use-modules (ice-9  match))

(define (*rules* . expr)
  (match expr
    ;; binary operators
    (('!= . vals) (emit-binary "!=" vals))
    (('% . vals) (emit-binary "%" vals))
    (('%= vars . vals) (emit-assign "%=" vars vals))
    (('* . vals) (emit-binary "*" vals))
    (('*= vars . vals) (emit-assign "*=" vars vals))
    (('+ . vals) (emit-binary "+" vals))
    (('++ expr) `(,expr "++"))
    (('+= vars . vals) (emit-assign "+=" vars vals))
    (('- . vals) (emit-binary "-" vals))
    (('-- expr) `(,expr "--"))
    (('-= vars . vals) (emit-assign "-=" vars vals))
    (('/ . vals) (emit-binary "/" vals))
    (('/= vars . vals) (emit-assign "/=" vars vals))
    ((':= vars . vals) (emit-assign ":=" vars vals))
    (('< . vals) (emit-binary "<" vals))
    (('<- chan) `("<-" ,chan))
    (('<-! chan expr) `(,chan "<-" ,expr))
    (('<< . vals) (emit-binary "<<" vals))
    (('<<= vars . vals) (emit-assign "<<=" vars vals))
    (('<= . vals) (emit-binary "<=" vals))
    (('= vars . vals) (emit-assign "=" vars vals))
    (('== . vals) (emit-binary "==" vals))
    (('> . vals) (emit-binary ">" vals))
    (('>= . vals) (emit-binary ">=" vals))
    (('>> . vals) (emit-binary ">>" vals))
    (('>>= vars . vals) (emit-assign ">>=" vars vals))
    (('and . vals) (emit-binary "&&" vals))
    (('not expr) `("!" ,expr))
    (('or . vals) (emit-binary "||" vals))
    (('dot . vals) (emit-binary "." vals))
    (('adr expr) `("&" ,expr))
    (('ptr expr) `("*" ,expr))
    ((': key value) `(,key ": " ,value))

    ;; bitwise operators
    (('bitwise-and . vals) (emit-binary "&" vals))
    (('bitwise-and= vars . vals) (emit-assign "&=" vars vals))
    (('bitwise-but . vals) (emit-binary "&^" vals))
    (('bitwise-but= vars . vals) (emit-assign "&^=" vars vals))
    (('bitwise-or . vals)  (emit-binary "|" vals))
    (('bitwise-or= vars . vals) (emit-assign "|=" vars vals))
    (('bitwise-xor . vals) (emit-binary "^" vals))
    (('bitwise-xor= vars . vals) (emit-assign "^=" vars vals))

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

    (('break . rest) `((branch-stmt ,@rest)))
    (('continue . rest) `((branch-stmt ,@rest)))
    (('fallthrough . rest) `((branch-stmt ,@rest)))
    (('goto . rest) `((branch-stmt ,@rest)))

    (('branch-stmt kw . label)
     `(kw ,@label))
    (('package name . decls)
     `("package" ,name "\n" ,(emit-decls decls)))
    (('return . expr)
     `("return" ,(emit-exprs expr)))

    ;; The "if/else" keywords
    (('when expr . body)
     `("if" ,expr ,(emit-else-block body)))
    (('when* stmt expr . body)
     `("if" ,stmt ";" ,expr ,(emit-else-block body)))
    (('unless expr . body)
     `("if !" ,expr ,(emit-else-block body)))
    (('unless* stmt expr . body)
     `("if" ,stmt "; !" ,expr ,(emit-else-block body)))

    (('as . body)
     (apply emit-as body))

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
