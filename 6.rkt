#lang racket
(require "util.rkt")

(define day "6")
(define ex1 "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
")

(define (parse s)
  (let*-values ([(nums ops) (split-at-right (lines s) 1)]
                [(ops) (map (λ (x) (if (string=? x "+") + *))
                            (regexp-match* #px"[*+]" (first ops)))])
    (values ops nums)))

(define (part1 s)
  (let-values ([(ops nums) (parse s)])
    (for/sum ([op ops]
              [t-nums (transpose (map ints nums))])
      (apply op t-nums))))

(define (part2 s)
  (let-values ([(ops nums) (parse s)])
    (let loop ([L (transpose nums)] [ops ops] [cur '()])
      (cond ((null? L)
             (apply (first ops) cur))
            ((string=? (string-trim (first L)) "")
             (+ (apply (first ops) cur)
                (loop (cdr L) (cdr ops) '())))
            (else
             (loop (cdr L) ops (cons ((compose string->number string-trim)
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
