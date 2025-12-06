#lang racket
(require "util.rkt")

(define day "3")
(define ex1 "987654321111111
811111111111119
234234234234278
818181911112111")

(define (parse s)
  (map digits (lines s)))

(define (best-n L n)
  "Create max number by concatenating N digits from L, maintaining order."
  (if (or (null? L) (zero? n))
      0
      (let ([NL (best-in-range L n)])
        (+ (* (car NL) (expt 10 (sub1 n)))
              (best-n (cdr NL) (sub1 n))))))

(define (best-in-range L rng)
  "Return sublist starting with first max before RNG at end."
  (let ([mx (apply max (take L (add1 (- (length L) rng))))])
    (member mx L)))

(define (run s)
  (let ([battery-banks (parse s)])
    (λ (n)
      (for/sum ([bank battery-banks]) (best-n bank n)))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list
            ((run ex1) 2)
            ((run input) 2)
            ))
   (printf "Part 2: ~a\n"
           (list
            ((run ex1) 12)
            ((run input) 12)
            ))
   ))
