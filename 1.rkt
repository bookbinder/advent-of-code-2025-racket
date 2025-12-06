#lang racket
(require "util.rkt")

(define day "1")

(define ex1 "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(define (parse s)
  (ints (string-replace s "L" "-")))

(define (run s)
  (let ([nums (parse s)])
    (for/fold ([pos 50]
               [end-on-zero 0]
               [passed-zero 0]
               #:result (list end-on-zero (+ end-on-zero passed-zero)))
              ([n nums])
      (let* ([npos (modulo (+ pos (modulo n 100)) 100)]
             [passed (if (and (andmap positive? (list pos npos))
                              (or (and (negative? n) (> npos pos))
                                  (and (positive? n) (< npos pos))))
                         1
                         0)])
        (values npos
                (+ end-on-zero (if (zero? npos) 1 0))
                (+ passed-zero passed (quotient (abs n) 100)))))))

((compose run file->string) (format "~a.txt" day))
(time (run (file->string (format "~a.txt" day))))
