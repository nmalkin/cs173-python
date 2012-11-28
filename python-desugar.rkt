#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt" 
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string))))



(define (desugar-boolop [op : symbol] [values : (listof PyExpr)]) : CExpr
  (local [(define first-val (desugar (first values)))]
         (if (> (length values) 1)
          (case op
            ['And (CIf first-val
                       (desugar-boolop op (rest values))
                       first-val)]
            ['Or (CIf first-val
                      first-val
                      (desugar-boolop op (rest values)))])

          (desugar (first values)))))

(define (desugar-compop [l : PyExpr] 
                        [ops : (listof symbol)] 
                        [comparators : (listof PyExpr)]) : CExpr
  (local [(define first-right (desugar (first comparators)))
          (define l-expr (desugar l))
          (define first-comp (desugar (PyBinOp l (first ops) (first comparators))))]
         (if (> (length comparators) 1) 
           (CIf first-comp
                (desugar-compop (first comparators) (rest ops) (rest comparators))
                first-comp)
           first-comp)))


(define (desugar [expr : PyExpr]) : CExpr
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyAssign (targets value) (foldl (lambda (t asgns)
                                       (CSeq asgns (CAssign (desugar t)
                                                            (desugar value))))
                                       (CAssign (desugar (first targets))
                                                (desugar value))
                                       (rest targets))]
    [PyNum (n) (make-builtin-num n)]
    [PyBool (b) (if b (CTrue) (CFalse))]
    [PyStr (s) (make-builtin-str s)]
    [PyId (x ctx) (CId x)]

    ; for now just desugar raise as error
    ; TODO: implement real exceptions
    [PyRaise (expr) (CRaise (if (PyPass? expr)
                                (none)
                                (some (desugar expr))))]


    [PyPass () (CApp (CFunc empty (none) (CNone)) empty (none))] ;PyPass is an empty lambda

    [PyIf (test body orelse)
          (CIf (desugar test) (desugar body) (desugar orelse))]

    [PyBinOp (left op right)
             (let ([left-c (desugar left)]
                   [right-c (desugar right)]) 
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
                 ['NotEq (desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right)))]

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

                 ['NotIn (desugar (PyUnaryOp 'Not (PyBinOp left 'In right)))]

                 [else (CPrim2 op (desugar left) (desugar right))]))]

    [PyUnaryOp (op operand)
               (case op
                 ['USub (desugar (PyBinOp (PyNum 0) 'Sub operand))]
                 ['UAdd (desugar (PyBinOp (PyNum 0) 'Add operand))]
                 ['Invert (CApp (CGetField (desugar operand) '__invrt__)
                                (list (desugar operand))
                                (none))]
                 [else (CPrim1 op (desugar operand))])]
    [PyBoolOp (op values) (desugar-boolop op values)]
              
    [PyCompOp (l op rights) (desugar-compop l op rights)]

    [PyLam (args body)
           (CFunc args (none)
                  (CReturn                   
                   (desugar body)))]
    
    [PyFunc (name args body)
            (CLet name (CNone)
                (CAssign (CId name)
                         (CFunc args (none)
                                (desugar body))))]

    [PyFuncVarArg (name args sarg body)
            (CLet name (CNone)
                (CAssign (CId name)
                         (CFunc args (some sarg)
                                (desugar body))))]
    
    [PyReturn (value)
              (CReturn (desugar value))]

    [PyDict (keys values)
            (CDict (lists->hash (map desugar keys)
                                (map desugar values)))]

    [PyList (values)
            (CList (map desugar values))]

    [PySubscript (left ctx slice)
                 (if (symbol=? ctx 'Load)
                   (let ([left-id (new-id)])
                     (CLet left-id 
                           (desugar left)
                           (CApp (CGetField (CId left-id)
                                            '__attr__)
                                 (list (CId left-id) (desugar slice))
                                 (none))))
                   (CNone))]

    [PyTuple (values)
            (CTuple (map desugar values))]

    [PyApp (fun args)
           (let ([f (desugar fun)])
             (if (CGetField? f)
               (let ([o (CGetField-value f)])
                 (CApp f
                       (cons o (map desugar args))
                       (none)))
               (CApp
                 (desugar fun)
                 (map desugar args)
                 (none))))]

    [PyAppStarArg (fun args sarg)
           (let ([f (desugar fun)])
             (if (CGetField? f)
               (let ([o (CGetField-value f)])
                 (CApp f
                       (cons o (map desugar args))
                       (none)))
               (CApp
                 (desugar fun)
                 (map desugar args)
                 (some (desugar sarg)))))]
    
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
                                (desugar body))))]
    
    [PyDotField (value attr)
                (CGetField (desugar value)
                           attr)]
    
    [PyTryExceptElseFinally (try excepts orelse finally)
                (CTryExceptElseFinally
                  (desugar try)
                  (map desugar excepts)
                  (desugar orelse)
                  (desugar finally))]
    
    [PyExcept (types body)
              (CExcept (map desugar types)
                       (none)
                       (desugar body))]

    [PyExceptAs (types name body)
                (CExcept (map desugar types)
                         (some name)
                         (desugar body))]

    [PyAugAssign (op target value)
                 (CAssign (desugar target)
                          (desugar (PyBinOp target op value)))]
    ; XXX: target is interpreted twice, independently.
    ; Is there any case where this might cause problems?

))
