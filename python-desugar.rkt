#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt" 
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string))))



(define (desugar-boolop [op : symbol] [values : (listof PyExpr)]
                        [global? : boolean]) : CExpr
  (local [(define first-val (rec-desugar (first values) global?))]
         (if (> (length values) 1)
          (case op
            ['And (CIf first-val
                       (desugar-boolop op (rest values) global?)
                       first-val)]
            ['Or (CIf first-val
                      first-val
                      (desugar-boolop op (rest values) global?))])

          (rec-desugar (first values) global?))))

(define (desugar-compop [l : PyExpr] 
                        [ops : (listof symbol)] 
                        [comparators : (listof PyExpr)]
                        [global? : boolean]) : CExpr
  (local [(define first-right (rec-desugar (first comparators) global?))
          (define l-expr (rec-desugar l global?))
          (define first-comp (rec-desugar (PyBinOp l (first ops) (first
                                                                   comparators))
                                          global?))]
         (if (> (length comparators) 1) 
           (CIf first-comp
                (desugar-compop (first comparators) (rest ops) (rest
                                                                 comparators)
                                global?)
                first-comp)
           first-comp)))

;; look through a  and find a list of all names from assignments and definitions
;;(define (get-names [expr : PyExpr]) : (listof symbol)
;;  (type-case PyExpr expr
;;   [PySeq (es) (map (

(define (desugar [expr : PyExpr]) : CExpr
  (rec-desugar expr true))

(define (rec-desugar [expr : PyExpr] [global? : boolean]) : CExpr
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (rec-desugar e1 global?)))
                       (rec-desugar (first es) global?) (rest es))]
    [PyAssign (targets value) (foldl (lambda (t asgns)
                                       (CSeq asgns (CAssign (rec-desugar t
                                                                         global?)
                                                            (rec-desugar value
                                                                         global?))))
                                       (CAssign (rec-desugar (first targets)
                                                             global?)
                                                (rec-desugar value global?))
                                       (rest targets))]
    [PyNum (n) (make-builtin-num n)]
    [PyBool (b) (if b (CTrue) (CFalse))]
    [PyStr (s) (make-builtin-str s)]
    [PyId (x ctx) (CId x)]

    ; for now just desugar raise as error
    ; TODO: implement real exceptions
    [PyRaise (expr) (CRaise (if (PyPass? expr)
                                (none)
                                (some (rec-desugar expr global?))))]


    [PyPass () (CApp (CFunc empty (none) (CNone)) empty (none))] ;PyPass is an empty lambda

    [PyIf (test body orelse)
          (CIf (rec-desugar test global?) (rec-desugar body global?)
               (rec-desugar orelse global?))]

    [PyBinOp (left op right)
             (let ([left-c (rec-desugar left global?)]
                   [right-c (rec-desugar right global?)]) 
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
                 ['NotEq (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right))
                                      global?)]

                 ['In (CApp (CFunc (list 'self 'test) (none)
                                   (CSeq
                                     (CAssign (CId '__infunc__)
                                              (CGetField (CId 'self)
                                                         '__in__))
                                     (CIf (CId '__infunc__)
                                          (CReturn
                                            (CApp
                                              (CId '__infunc__)
                                              (list (CId 'self)
                                                    (CId 'test))
                                              (none)))
                                          (CApp (CId 'TypeError)
                                                (list (CObject
                                                        'str
                                                        (some (MetaStr 
                                                                (string-append
                                                                  "argument of type '___'" 
                                                                  "is not iterable")))))
                                                (none)))))
                            (list right-c left-c)
                            (none))]

                 ['NotIn (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'In right))
                                      global?)]

                 [else (CPrim2 op (rec-desugar left global?) (rec-desugar right
                                                                          global?))]))]

    [PyUnaryOp (op operand)
               (case op
                 ['USub (rec-desugar (PyBinOp (PyNum 0) 'Sub operand) global?)]
                 ['UAdd (rec-desugar (PyBinOp (PyNum 0) 'Add operand) global?)]
                 ['Invert (CApp (CGetField (rec-desugar operand global?) '__invrt__)
                                (list (rec-desugar operand global?))
                                (none))]
                 [else (CPrim1 op (rec-desugar operand global?))])]
    [PyBoolOp (op values) (desugar-boolop op values global?)]
              
    [PyCompOp (l op rights) (desugar-compop l op rights global?)]

    [PyLam (args body)
           (CFunc args (none)
                  (CReturn                   
                   (rec-desugar body global?)))]
    
    [PyFunc (name args body)
            (CLet name (CNone)
                (CAssign (CId name)
                         (CFunc args (none)
                                (rec-desugar body false))))]

    [PyFuncVarArg (name args sarg body)
            (CLet name (CNone)
                (CAssign (CId name)
                         (CFunc args (some sarg)
                                (rec-desugar body global?))))]
    
    [PyReturn (value)
              (CReturn (rec-desugar value global?))]

    [PyDict (keys values)
            (CDict (lists->hash (map (lambda(k) (rec-desugar k global?)) keys)
                                (map (lambda(v) (rec-desugar v global?)) values)))]

    [PySet (elts)
            (CSet (map desugar elts))]

    [PyList (values)
            (CList (map (lambda(v) (rec-desugar v global?)) values))]

    [PySubscript (left ctx slice)
                 (if (symbol=? ctx 'Load)
                   (let ([left-id (new-id)])
                     (CLet left-id 
                           (rec-desugar left global?)
                           (CApp (CGetField (CId left-id)
                                            '__attr__)
                                 (list (CId left-id) (rec-desugar slice global?))
                                 (none))))
                   (CNone))]

    [PyTuple (values)
            (CTuple (map (lambda(v) (rec-desugar v global?)) values))]

    [PyApp (fun args)
           (let ([f (rec-desugar fun global?)])
             (if (CGetField? f)
               (let ([o (CGetField-value f)])
                 (CApp f
                       (cons o (map (lambda(a) (rec-desugar a global?)) args))
                       (none)))
               (CApp
                 (rec-desugar fun global?)
                 (map (lambda (a) (rec-desugar a global?)) args)
                 (none))))]

    [PyAppStarArg (fun args sarg)
           (let ([f (rec-desugar fun global?)])
             (if (CGetField? f)
               (let ([o (CGetField-value f)])
                 (CApp f
                       (cons o (map (lambda(a) (rec-desugar a global?)) args))
                       (none)))
               (CApp
                 (rec-desugar fun global?)
                 (map (lambda(a) (rec-desugar a global?)) args)
                 (some (rec-desugar sarg global?)))))]
    
    [PyClass (name bases body)
             (CSeq 
               (CAssign
                 (CId name)
                 (CUndefined))
               (CAssign (CId name)
                        (CClass name
                                (if (empty? bases) 
                                  'object
                                  (first bases))  
                                (rec-desugar body false))))]
    
    [PyDotField (value attr)
                (CGetField (rec-desugar value global?)
                           attr)]
    
    [PyTryExceptElseFinally (try excepts orelse finally)
                (CTryExceptElseFinally
                  (rec-desugar try global?)
                  (map (lambda (e) (rec-desugar e global?)) excepts)
                  (rec-desugar orelse global?)
                  (rec-desugar finally global?))]
    
    [PyExcept (types body)
              (CExcept (map (lambda(t) (rec-desugar t global?)) types)
                       (none)
                       (rec-desugar body global?))]

    [PyExceptAs (types name body)
                (CExcept (map (lambda(t) (rec-desugar t global?)) types)
                         (some name)
                         (rec-desugar body global?))]

    [PyAugAssign (op target value)
                 (CAssign (rec-desugar target global?)
                          (rec-desugar (PyBinOp target op value) global?))]
    ; XXX: target is interpreted twice, independently.
    ; Is there any case where this might cause problems?

))
