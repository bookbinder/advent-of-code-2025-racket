#lang racket
(require "util.rkt")

(define day "2")
(define ex1 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(define (parse s)
  (map +ints (string-split s ",")))

(define (valid-id? id)
  (let* ([s (number->string id)]
         [halfway (quotient (string-length s) 2)])
    (not
     (string=? (substring s 0 halfway) (substring s halfway)))))

(define (factors n)
  (for/list ([i (in-range 1 (add1 (quotient n 2)))]
             #:when (zero? (remainder n i)))
    i))

(define (valid-id2? id)
  (or (<= id 9)
      (let* ([s (number->string id)]
             [fac (factors (string-length s))])
        (for/and ([f fac])
          (let* ([ss (substring s 0 f)]
                 [ss (apply string-append
                            (make-list (quotient (string-length s) f) ss))])
            (not (string=? ss s)))))))

(define (run s)
  (let ([ranges (parse s)])
    (λ (fn)
      (for*/sum ([x ranges]
                 [y (in-range (first x) (add1 (second x)))])
        (if (fn y) 0 y)))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list
            ((run ex1) valid-id?)
            ((run input) valid-id?)
            ))
   (printf "Part 2: ~a\n"
           (list
            ((run ex1) valid-id2?)
            ((run input) valid-id2?)
            ))
   ))
