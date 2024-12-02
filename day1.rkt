#lang racket

(let ([file-contents (

(define (file-contents file)
  (define (read-file in lines)
    (let ([line (read-line in)])
      (if (eof-object? line)
          lines
          (read-file in (cons line lines)))))

  (read-file file '()))

(define locations (file-contents (open-input-file "day1.txt")))

(define (col col# lst)
  (map (lambda (s)
         (list-ref (string-split s) col#))
       lst))

(define left-side (sort
                   (map string->number (col 0 locations))
                   <))
  
(define right-side (sort
                    (map string->number (col 1 locations))
                    <))

(define right-side-occur# (make-hash (map (lambda (r) (cons r 0)) right-side)))

(for ([r right-side])
  (hash-update! right-side-occur# r add1))

#;(display (foldl + 0 (for/list ([i left-side]
                               [j right-side])
                      (abs (- i j)))))

(display (foldl + 0 (for/list ([i left-side])
                    (* i (hash-ref right-side-occur# i 0)))))