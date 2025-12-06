#lang racket
(require "util.rkt")

(define day "6")
(define ex1 "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
")

(define (parse s)
  (let ([nums (map ints (lines s))]
        [ops (regexp-match* #px"[*+]" (last (lines s)))])
    (cons (map (λ (x) (if (string=? x "+") + *)) ops) (butlast nums))))

(define (part1 s)
  (let ([input (parse s)])
    (for/sum ([op (first input)]
              [nums (transpose (cdr input))])
      (apply op nums))))

(define (part2 s)
  (let* ([L (transpose (butlast (lines s)))]
         [ops (map (λ (x) (if (string=? x "+") + *))
                   (regexp-match* #px"[*+]" (last (lines s))))])
    (let loop ([L L] [op ops] [cur '()])
      (cond ((null? L)
             (apply (first op) cur))
            ((string=? (string-normalize-spaces (first L)) "")
             (+ (apply (first op) cur)
                (loop (cdr L) (cdr op) '())))
            (else
             (loop (cdr L) op (cons ((compose string->number
                                              string-normalize-spaces)
                                     (first L))
                                    cur)))))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list (part1 ex1)
                 (part1 input)
                 ))
   (printf "Part 2: ~a\n"
           (list (part2 ex1)
                 (part2 input)
                 ))
   ))
