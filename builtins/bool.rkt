#lang plai-typed 

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "str.rkt"
         "object.rkt")

(define bool-class 
  (CClass 
    'bool
    'num
    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'arg) (none)
                           (CReturn (CBuiltinPrim 'bool-init
                                                  (list
                                                   (CId 'self)
                                                   (CId 'arg))))))

               (def '__str__
                    (CFunc (list 'self) (none)
                           (CIf (CApp (CGetField (CId 'self) '__eq__)
                                      (list (CId 'self) (make-builtin-num 1))
                                      (none))
                                (CReturn (make-builtin-str "True"))
                                (CReturn (make-builtin-str "False")))))
               (def '__int__
                    (CFunc (list 'self) (none)
                           (CReturn (CApp (CGetField (CId 'self) '__add__) 
                                          (list (CId 'self) 
                                                (make-builtin-num 0))
                                          (none)))))

               (def '__float__
                    (CFunc (list 'self) (none)
                           (CReturn (CApp (CGetField (CId 'self) '__add__) 
                                          (list (CId 'self) 
                                                (make-builtin-num 0.0))
                                          (none)))))))))

(define (make-builtin-bool [b : boolean]) : CExpr
  (CObject 
    'bool
    (some 
      (if b 
        (MetaNum 1)
        (MetaNum 0)))))

(define (bool-init [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (let ([other (second args)]) ; FIXME: what if (second args) DNE?
  (type-case CVal other
    [VStr (s) (if (string=? "" s)
              (some false-val)
              (some true-val))]
    [VClosure (e a s b) (some true-val)]
    [VObject (a mval d) (if (truthy-object? (VObject a mval d))
                          (some true-val)
                          (some false-val))])))
