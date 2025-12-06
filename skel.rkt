(require "util.rkt")

(define day "")
(define ex1 "")

(define (parse s)
  s)

(define (part1 s)
  (let ([input (parse s)])
    input))

(define (part2 s)
  0)

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list (part1 ex1)
                 ;; (part1 input)
                 ))
   ;; (printf "Part 2: ~a\n"
   ;;         (list (part2 ex1)
   ;;               (part2 input)
   ;;               ))
   ))
