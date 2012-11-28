#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt" 
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string))))



(define (desugar-boolop [op : symbol] [values : (listof PyExpr)]
                        [global? : boolean]
                        [env : IdEnv]) : CExpr
  (local [(define first-val (rec-desugar (first values) global? env))]
         (if (> (length values) 1)
          (case op
            ['And (CIf first-val
                       (desugar-boolop op (rest values) global? env)
                       first-val)]
            ['Or (CIf first-val
                      first-val
                      (desugar-boolop op (rest values) global? env))])

          (rec-desugar (first values) global? env))))

(define (desugar-compop [l : PyExpr] 
                        [ops : (listof symbol)] 
                        [comparators : (listof PyExpr)]
                        [global? : boolean]
                        [env : IdEnv]) : CExpr
  (local [(define first-right (rec-desugar (first comparators) global? env))
          (define l-expr (rec-desugar l global? env))
          (define first-comp (rec-desugar (PyBinOp l (first ops) (first
                                                                   comparators))
                                          global? env))]
         (if (> (length comparators) 1) 
           (CIf first-comp
                (desugar-compop (first comparators) (rest ops) (rest
                                                                 comparators)
                                global? env)
                first-comp)
           first-comp)))

;; look through a  and find a list of all names from assignments and definitions
;;(define (get-names [expr : PyExpr]) : (listof symbol)
;;  (type-case PyExpr expr
;;   [PySeq (es) (map (

(define (desugar [expr : PyExpr]) : CExpr
  (type-case DesugarResult (rec-desugar expr true (hash empty))
    [DResult (expr env) env]))

(define (rec-desugar [expr : PyExpr] [global? : boolean] [env : IdEnv]) : DResult 
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) 
                         (CSeq e2 (rec-desugar e1 global? env))))
                       (rec-desugar (first es) global? env) (rest es)]
    [PyAssign (targets value) (foldl (lambda (t asgns)
                                         (CSeq asgns 
                                               (CAssign 
                                                 (rec-desugar t global? env))
                                                 (rec-desugar value global? env))))
                                       (CAssign (rec-desugar (first targets) global? env))
                                                (rec-desugar value global? env)
                                       (rest targets)]

    [PyNum (n) (DResult (make-builtin-num n) env)]
    [PyBool (b) (DResult (if b (CTrue) (CFalse)) env)]
    [PyStr (s) (DResult (make-builtin-str s) env)]
    [PyId (x ctx) (DResult (CId x (LocalId)) env)]
    [PyGlobal (ids) (DResult (CNone) (map (lambda (id) (add-id id (GlobalId) env)) ids))]
    [PyNonlocal (ids) (DResult (CNone) (map (lambda (id) (add-id id (NonlocalId) env)) ids))]

    ; for now just desugar raise as error
    ; TODO: implement real exceptions
    [PyRaise (expr) (local [(define exn (rec-desugar expr global? env))]
                            (DResult
                              (CRaise 
                                (if (PyPass? expr)
                                    (none)
                                    (some (DResult-expr exn))))
                              (DResult-env exn)))]

    ; PyPass is an empty lambda
    [PyPass () (DResult (CApp (CFunc empty (none) (CNone)) empty (none)) env)] 

    [PyIf (test body orelse)
          (local [(define rtest (rec-desugar test global? env))
                  (define rbody (rec-desugar body global? (DResult-env rtest)))
                  (define rorelse (rec-desugar orelse global? (DResult-env rbody)))]
          (DResult 
            (CIf (DResult-expr rtest)
                 (DResult-expr rbody)
                 (DResult-expr rorelse))
            (DResult-env rorelse)))]

    [PyBinOp (left op right)
             (DResult
               (local [(define left-r (rec-desugar left global? env))
                       (define left-c (DResult-expr left-r))
                       (define right-r (rec-desugar right global? (DResult-env left-c)))
                       (define right-c (DResult-expr left-r))
                       (define renv (DResult-expr right-r))] 
               (case op 
                 ['Add (CApp (CGetField left-c '__add__) 
                             (list left-c right-c)
                             (none))]
                 ['Sub (CApp (CGetField left-c '__sub__) 
                             (list left-c right-c)
                             (none))]
                 ['Mult (CApp (CGetField left-c '__mult__)
                              (list left-c right-c)
                              (none))]
                 ['Div (CApp (CGetField left-c '__div__)
                              (list left-c right-c)
                              (none))]
                 ['FloorDiv (CApp (CGetField left-c '__floordiv__)
                              (list left-c right-c)
                              (none))]
                 ['Mod (CApp (CGetField left-c '__mod__)
                              (list left-c right-c)
                              (none))]
                 ['Eq (CApp (CGetField left-c '__eq__)
                            (list left-c right-c)
                            (none))]
                 ['Gt (CApp (CGetField left-c '__gt__)
                            (list left-c right-c)
                            (none))]
                 ['Lt (CApp (CGetField left-c '__lt__)
                            (list left-c right-c)
                            (none))]
                 ['LtE (CApp (CGetField left-c '__lte__)
                            (list left-c right-c)
                            (none))]
                 ['GtE (CApp (CGetField left-c '__gte__)
                            (list left-c right-c)
                            (none))]
                 ['NotEq (local [(define rnot 
                                   (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right))
                                                global? env))]
                                (begin
                                  (set! renv (DResult-env rnot)) 
                                  (DResult-expr rnot)))]

                 ['In (CApp (CFunc (list 'self 'test) (none)
                                   (CSeq
                                     (CAssign (CId '__infunc__ (LocalId))
                                              (CGetField (CId 'self (LocalId))
                                                         '__in__))
                                     (CIf (CId '__infunc__ (LocalId))
                                          (CReturn
                                            (CApp
                                              (CId '__infunc__ (LocalId))
                                              (list (CId 'self (LocalId))
                                                    (CId 'test (LocalId)))
                                              (none)))
                                          (CApp (CId 'TypeError (LocalId))
                                                (list (CObject
                                                        'str
                                                        (some (MetaStr 
                                                                (string-append
                                                                  "argument of type '___'" 
                                                                  "is not iterable")))))
                                                (none)))))
                            (list right-c left-c)
                            (none))]

                 ['NotIn (local [(define rnot
                                   (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'In right))
                                                global? env))]
                                (begin
                                  (set! renv (DResult-env rnot))
                                  (DResult-expr rnot)))]

                 [else (CPrim2 op left-c right-c)]))
               renv)]

    [PyUnaryOp (op operand)
               (case op
                 ['USub (rec-desugar (PyBinOp (PyNum 0) 'Sub operand) global? env)]
                 ['UAdd (rec-desugar (PyBinOp (PyNum 0) 'Add operand) global? env)]
                 ['Invert (local [(define roperand (rec-desugar operand global? env))]
                            (DResult 
                              (CApp (CGetField (DResult-expr roperand) '__invrt__)
                                (list (DResult-expr roperand))
                                (none))
                              (DResult-env roperand)))]
                 [else (local [(define roperand (rec-desugar operand global? env))]
                              (DResult (CPrim1 op 
                                               (DResult-expr roperand)) 
                                       (DResult-env roperand)))])]

    [PyBoolOp (op values) (desugar-boolop op values global? env)]
              
    [PyCompOp (l op rights) (desugar-compop l op rights global? env)]

    [PyLam (args body)
           (local [(define rbody (rec-desugar body global? env))]
                  (DResult 
                    (CFunc args (none)
                         (CReturn                   
                           (DResult-expr rbody)))
                    (DResult-env rbody)))]
    
    [PyFunc (name args body)
            (local [(define rbody (rec-desugar body false env))]
            (DResult
              (CLet name (CNone)
                    (CAssign (CId name (LocalId))
                             (CFunc args (none) rbody))))
              (merge-globals env (DResult-env rbody)))]

    [PyFuncVarArg (name args sarg body)
                  (local [(define rbody (rec-desugar body false env))]
                  (DResult
                    (CLet name (CNone)
                          (CAssign (CId name (LocalId))
                                   (CFunc args (some sarg) rbody))))
                    (merge-globals env (DResult-env rbody)))]
    
    [PyReturn (value)
              (local [(define rvalue (rec-desugar value global? env))]
                     (DResult (CReturn rvalue)) (DResult-env rvalue))]

    [PyDict (keys values)
            (local [(define rec
            (CDict (lists->hash (map (lambda(k) (rec-desugar k global? env)) keys)
                                (map (lambda(v) (rec-desugar v global? env)) values)))]

    [PyList (values)
            (CList (map (lambda(v) (rec-desugar v global? env)) values))]

    [PySubscript (left ctx slice)
                 (if (symbol=? ctx 'Load)
                   (let ([left-id (new-id)])
                     (CLet left-id 
                           (rec-desugar left global? env)
                           (CApp (CGetField (CId left-id (LocalId))
                                            '__attr__)
                                 (list (CId left-id (LocalId)) (rec-desugar slice global? env))
                                 (none))))
                   (CNone))]

    [PyTuple (values)
            (CTuple (map (lambda(v) (rec-desugar v global? env)) values))]

    [PyApp (fun args)
           (let ([f (rec-desugar fun global? env)])
             (if (CGetField? f)
               (let ([o (CGetField-value f)])
                 (CApp f
                       (cons o (map (lambda(a) (rec-desugar a global? env)) args))
                       (none)))
               (CApp
                 (rec-desugar fun global? env)
                 (map (lambda (a) (rec-desugar a global? env)) args)
                 (none))))]

    [PyAppStarArg (fun args sarg)
           (let ([f (rec-desugar fun global? env)])
             (if (CGetField? f)
               (let ([o (CGetField-value f)])
                 (CApp f
                       (cons o (map (lambda(a) (rec-desugar a global? env)) args))
                       (none)))
               (CApp
                 (rec-desugar fun global? env)
                 (map (lambda(a) (rec-desugar a global? env)) args)
                 (some (rec-desugar sarg global? env)))))]
    
    [PyClass (name bases body)
             (CSeq 
               (CAssign
                 (CId name (LocalId))
                 (CUndefined))
               (CAssign (CId name (LocalId))
                        (CClass name
                                (if (empty? bases) 
                                  'object
                                  (first bases))  
                                (rec-desugar body false (remember-globals env)))))]
    
    [PyDotField (value attr)
                (CGetField (rec-desugar value global? env)
                           attr)]
    
    [PyTryExceptElseFinally (try excepts orelse finally)
                (CTryExceptElseFinally
                  (rec-desugar try global? env)
                  (map (lambda (e) (rec-desugar e global? env)) excepts)
                  (rec-desugar orelse global? env)
                  (rec-desugar finally global? env))]
    
    [PyExcept (types body)
              (CExcept (map (lambda(t) (rec-desugar t global? env)) types)
                       (none)
                       (rec-desugar body global? env))]

    [PyExceptAs (types name body)
                (CExcept (map (lambda(t) (rec-desugar t global? env)) types)
                         (some name)
                         (rec-desugar body global? env))]

    [PyAugAssign (op target value)
                 (CAssign (rec-desugar target global? env)
                          (rec-desugar (PyBinOp target op value) global? env))]
    ; XXX: target is interpreted twice, independently.
    ; Is there any case where this might cause problems?

))

(define-type DesugarResult
   [DResult (e : CExpr) (env : env)]

(define (remember-globals [env : IdEnv]) env)

(define (merge-globals [complete-env : IdEnv] [only-globals : IdEnv]) env)

(define (add-id [id : symbol] [type : IdType] [env : IdEnv]) env)
