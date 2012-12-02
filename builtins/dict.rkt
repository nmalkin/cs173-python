#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "none.rkt")
(require
  (typed-in racket/base (andmap : (('a -> boolean) (listof 'a) -> 'b)))
  (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
  (typed-in racket/base (car : (('a * 'b)  -> 'b)))
  (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
  (typed-in racket/base (hash-has-key? : ((hashof 'a 'b) 'a -> boolean)))
  (typed-in racket/base (hash-values : ((hashof 'a 'b) -> (listof 'b))))
)

(define dict-class : CExpr
  (CClass
   'dict
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'dict-len
                                                  (list
                                                   (CId 'self (LocalId)))))))
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-str
                                                     (list (CId 'self (LocalId)))))))

              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-clear
                                                     (list (CId 'self (LocalId)))))))

              (def 'update
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'dict-update
                                                     (list (CId 'self (LocalId))
                                                           (CId 'other (LocalId)))))))
              (def 'get
                   (CFunc (list 'self 'key) (some 'default)
                          (CReturn (CBuiltinPrim 'dict-get 
                                        (list (CId 'self (LocalId)) 
                                              (CId 'key (LocalId)) 
                                              (CId 'default (LocalId)))))))
              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'dict-in
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId))
                                               )))))

              (def '__eq__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'dict-eq
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId))
                                               )))))

              (def 'keys
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-keys
                                                     (list (CId 'self (LocalId)))))))

              (def 'values
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-values
                                                     (list (CId 'self (LocalId)))))))
              
              (def 'items
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-items
                                                     (list (CId 'self (LocalId)))))))

              (def '__attr__
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'dict-attr
                                                     (list (CId 'self (LocalId))
                                                           (CId 'other (LocalId)))))))

              (def '__setattr__
                   (CFunc (list 'self 'target 'value) (none)
                          (CReturn (CBuiltinPrim 'dict-setattr
                                                     (list (CId 'self (LocalId))
                                                           (CId 'target (LocalId))
                                                           (CId 'value (LocalId)))))))

              (def '__delitem__
                   (CFunc (list 'self 'slice) (none)
                          (CReturn (CBuiltinPrim 'dict-delitem
                                                     (list (CId 'self (LocalId))
                                                           (CId 'slice (LocalId)))))))

))))


(define (dict-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (some (VObject 'num
                              (some (MetaNum (length (hash-keys (MetaDict-contents mval1)))))
                              (hash empty)))))

(define (dict-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))

(define (dict-clear (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                 (begin
                   ; remove all key-value pairs from hash
                   (map (lambda (key) (hash-remove! contents key))
                        (hash-keys contents))
                   (some vnone)))))

(define (dict-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                 (if (hash-has-key? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))
(define (dict-get [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
      (local [(define d (first args))
              (define meta-d (MetaDict-contents (some-v (VObject-mval d))))
              (define key (second args))
              (define startuple (third args))
              (define meta-startuple (MetaTuple-v (some-v (VObject-mval
                                                            startuple))))
              (define mayb-val (hash-ref meta-d key))]
             (if (some? mayb-val)
               mayb-val
               (if (not (= 0 (length meta-startuple)))
                 (some (first meta-startuple))
                 (some vnone))))))

(define (dict-update (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict 'dict
               (let ([target (MetaDict-contents mval1)]
                     [extras (MetaDict-contents mval2)])
                 (begin
                   (map (lambda (pair)
                          (hash-set! target (car pair) (cdr pair)))
                        (hash->list extras))
                   (some vnone)))))

(define (dict-eq (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict 'dict
               (let ([self (MetaDict-contents mval1)]
                     [other (MetaDict-contents mval2)]
                     [compare (lambda (me them) ; check that they have all my values
                                (andmap
                                  (lambda (pair)
                                    ; get their value for the current key
                                    (let ([their-value (hash-ref them (car pair))])
                                      (if (some? their-value) ; if it exists,
                                        (equal? (some-v their-value) ; compare it
                                                (cdr pair)) ; with my value
                                        #f))) ; if their value DNE, they're not the same
                                  (hash->list me)))])
                 (begin
                   (if (and (compare self other)
                            (compare other self))
                     (some true-val)
                     (some false-val))))))

(define (dict-keys (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set (hash-keys contents))))
                               (make-hash empty))))))

(define (dict-values (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set (hash-values contents))))
                               (make-hash empty))))))

(define (dict-items (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [items (map (lambda (pair) ; create a tuple for each (key, value)
                                            (VObject 'tuple
                                                     (some (MetaTuple (list (car pair) (cdr pair))))
                                                     (make-hash empty)))
                                    (hash->list contents))])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set items)))
                               (make-hash empty))))))


(define (dict-attr [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [mayb-val (hash-ref contents target)])
                 (if (some? mayb-val)
                   mayb-val
                   (some vnone)))))

(define (dict-setattr [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [value (third args)])
                 (begin
                   (hash-set! contents target value)
                   (some vnone)))))

(define (dict-delitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)])
                 (begin
                   (hash-remove! contents target)
                   (some vnone)))))
