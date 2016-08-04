#lang planet neil/sicp

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)))))

(define (stream-map proc . argstreams)
  (if (null? (car argstresms))
      '()
      (cons-stream
       (apply proc
              (map (lambda(s)
                     (stream-car s))
                   argstreams))
       (apply stream-map
              (cons proc (map (lambda(s)
                                (stream-cdr s))
                              argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-cars))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define one-to-ten (stream-enumerate-interval 1 10))

(display-stream (stream-map + one-to-ten one-to-ten))