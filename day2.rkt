#lang racket

(define input-file-contents (file->lines "day2.txt"))

(define (report->number-list line)
  (map string->number (string-split line)))

(define reports (map report->number-list input-file-contents))

(define (deltas num-list)
  (if (or (not (pair? num-list)) (= 1 (length num-list)))
      '()
      (let ([first-two (take num-list 2)])
        (cons (- (first first-two) (second first-two)) (deltas (cdr num-list))))))

(define (abs-within-interval? lo hi)
  (lambda (x)
    (let ([abs-x (abs x)])
      (and (>= abs-x lo) (<= abs-x hi)))))

(define (report-is-safe? report)
  (let ([d (deltas report)])
    (and (or (andmap negative? d)
             (andmap positive? d))
         (andmap (abs-within-interval? 1 3) d))))

(define part1-answer (count report-is-safe? reports))

(define (dampen fn)
  (lambda (lst)
    (ormap fn (combinations lst (- (length lst) 1)))))

(define part2-answer (count (dampen report-is-safe?) reports))
