#lang planet neil/sicp
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (*  (car items) factor)
            (scale-list (cdr items) factor))))

;(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;(map abs (list -10 2.5 -11.6 17))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;(define odds (list 1 3 5 7))
;(length odds)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;(define odds (list 1 3 5 7))
;(length odds)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree ))(list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (squre x) 
  (* x x))
        

(define (sum-odd-squres tree)
  (accumulate +
              0
              (map squre (filter odd?
                                 (enumerate-tree tree)))))


(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (acc accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda(i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1)))
                  (enumerate-interval 1 n))))))

(prime-sum-pairs 10)

