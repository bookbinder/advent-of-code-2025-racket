#lang racket
(require "util.rkt")

(define day "4")
(define ex1 "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(define (parse s)
  (2dlist->2dvector (map string->list (lines s))))

(define (part1 s)
  (let ([G (parse s)])
    (for/sum ([pt (findall-in-G G #\@)]
              #:when (< (count (λ (nei) (char=? (get2 G nei) #\@))
                               (nei8 G pt))
                        4))
      1)))

(define (part2 s)
  (let* ([G (parse s)])
    (let loop ()
      (let ([pts (for/list ([pt (findall-in-G G #\@)]
                            #:when (< (count (λ (nei)
                                               (char=? (get2 G nei) #\@))
                                             (nei8 G pt))
                                      4))
                   pt)])
        (if (null? pts)
            0
            (+ (for/sum ([x pts]) (set2! G x #\.) 1)
               (loop)))))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list
            (part1 ex1)
            (part1 input)
            ))
   (printf "Part 2: ~a\n"
           (list
            (part2 ex1)
            (part2 input)
            ))
   ))
