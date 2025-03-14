#lang racket

(provide (all-defined-out))

(define id (lambda (x) x))
(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

;;; Q1.a
; Signature: compose(f g)
; Type: [T1 -> T2] * [T2 -> T3]  -> [T1->T3]
; Purpose: given two unary functions return their composition, in the same order left to right
; test: ((compose - sqrt) 16) ==> -4
;       ((compose not not) true)==> true
(define compose
  (lambda (f g)
    (lambda (x)
       (g (f x)))))

(define compose$ 
  (lambda (f g cont)
    (cont(lambda (x cont)
      (f x
         (lambda (res)
           (g res cont)))))))

; Signature: pipe(lst-fun)
; Type: [[T1 -> T2],[T2 -> T3]...[Tn-1 -> Tn]]  -> [T1->Tn]
; Purpose: Returns the composition of a given list of unary functions. For (pipe (list f1 f2 ... fn)), returns the composition fn(....(f1(x)))
; test: ((pipe (list sqrt - - number?)) 16) ==> true
;       ((pipe (list sqrt - - number? not)) 16) ==> false
;       ((pipe (list sqrt add1 - )) 100) ==> -11
(define pipe
  (lambda (fs)  
    (if (empty? (cdr fs))
        (car fs)
        (compose (car fs) (pipe (cdr fs))))))

; Signature: pipe$(lst-fun,cont)
;         [T1 * [T2->T3] ] -> T3,
;         [T3 * [T4 -> T5] ] -> T5,
;         ...,
;         [T2n-1 * [T2n -> T2n+1]] -> T2n+1
;        *
;       [[T1 * [T2n -> T2n+1]] -> T2n+1] -> 
;              [[T1 * [T2n+1 -> T2n+2]] -> T2n+2]
;      -> [T1 * [T2n+1 -> T2n+2]] -> T2n+2
; Purpose: Returns the composition of a given list of unry CPS functions. 
(define pipe$ 
  (lambda (lst-fun$ cont)
    (if (empty? (cdr lst-fun$))
      (cont(car lst-fun$))
      (pipe$ (cdr lst-fun$) 
        (lambda (res) 
          (compose$ (car lst-fun$) res cont))))))

;;; Q2a
; Signature: reduce1-lzl(reducer, init, lzl) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> -> T2
; Purpose: Returns the reduced value of the given lazy list
(define reduce1-lzl 
  (lambda (reducer init lzl)
    (if (empty? (tail lzl))
        (reducer init (head lzl))
        (reduce1-lzl reducer (reducer init (head lzl)) (tail lzl)))
  )
)  

;;; Q2b
; Signature: reduce2-lzl(reducer, init, lzl, n) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> * Number -> T2
; Purpose: Returns the reduced value of the first n items in the given lazy list
(define reduce2-lzl 
  (lambda (reducer init lzl n)
    (if (or (empty? (head lzl)) (= n 0))
        empty-lzl
    (if (or (empty? (tail lzl)) (= n 1)) 
        (reducer init (head lzl))
        (reduce2-lzl reducer (reducer init (head lzl)) (tail lzl) (- n 1))))
  )
)  

;;; Q2c
; Signature: reduce3-lzl(reducer, init, lzl) 
; Type: [T2 * T1 -> T2] * T2 * LzL<T1> -> Lzl<T2>
; Purpose: Returns the reduced values of the given lazy list items as a lazy list
(define reduce3-lzl 
  (lambda (reducer init lzl)
   (letrec ((red3 (lambda (n)
                    (cons-lzl (reduce2-lzl reducer init lzl n) (lambda ()
                                                                 (red3 (+ n 1)))))))
    (red3 1)))) 
 
;;; Q2e
; Signature: integers-steps-from(from,step) 
; Type: Number * Number -> Lzl<Number>
; Purpose: Returns a list of integers from 'from' with 'steps' jumps
(define integers-steps-from
  (lambda (from step)
   (letrec ((init-from (lambda (n)
                        (cons-lzl n (lambda ()
                                      (init-from (+ n step)))))))
     (init-from from))
  )
)

;;; Q2f
; Signature: generate-pi-approximations() 
; Type: Empty -> Lzl<Number>
; Purpose: Returns the approximations of pi as a lazy list
(define generate-pi-approximations
  (lambda ()
    (letrec ((int-step2 (integers-steps-from 0 2))
             (aproximate (lambda (steps)
                           (cons-lzl (/ 1 (*(+ 1 (head steps))(+ 1 (head(tail steps)))))
                                     (lambda () (aproximate (tail(tail steps)))))))
             (reduced (reduce3-lzl + 0 (aproximate int-step2))))
      (lzl-map (lambda (x) (* x 8)) reduced)
      )                                      
   )
 )

(define lzl-map
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                       (lambda () (lzl-map f (tail lzl)))))))
