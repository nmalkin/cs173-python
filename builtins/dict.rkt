#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require
  (typed-in racket/base (andmap : (('a -> boolean) (listof 'a) -> 'b)))
  (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
  (typed-in racket/base (car : (('a * 'b)  -> 'b)))
  (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
  (typed-in racket/base (hash-has-key? : ((hashof 'a 'b) 'a -> boolean))))

(define dict-class : CExpr
  (CClass
   'dict
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'dict-len
                                                  (list
                                                   (CId 'self))))))
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-str
                                                     (list (CId 'self))))))

              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-clear
                                                     (list (CId 'self))))))

              (def 'update
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'dict-update
                                                     (list (CId 'self)
                                                           (CId 'other))))))
              (def 'get
                   (CFunc (list 'self 'key) (some 'default)
                          (CReturn (CBuiltinPrim 'dict-get 
                                        (list (CId 'self) 
                                              (CId 'key) 
                                              (CId 'default))))))
              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'dict-in
                                              (list
                                               (CId 'self)
                                               (CId 'other)
                                               )))))

              (def '__eq__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'dict-eq
                                              (list
                                               (CId 'self)
                                               (CId 'other)
                                               )))))
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
                   (some (VNone))))))

(define (dict-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                 (if (hash-has-key? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))
(define (dict-get [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
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
             (some (VNone))))))

(define (dict-update (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict 'dict
               (let ([target (MetaDict-contents mval1)]
                     [extras (MetaDict-contents mval2)])
                 (begin
                   (map (lambda (pair)
                          (hash-set! target (car pair) (cdr pair)))
                        (hash->list extras))
                   (some (VNone))))))

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

