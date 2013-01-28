;P01 - Find the last box of a list.
(define my-last
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (my-last (cdr lst)))))

;P02 - Find the last but one box of a list.
(define my-last-but
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) '())
          ((null? (cddr lst)) lst)
          (else (my-last-but (cdr lst))))))

;P03 - Find the K'th element of a list.
(define element-at
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (element-at (cdr lst) (- i 1)))))

;P04 - Find the number of elements of a list.
(define length-of
  (lambda (lst)
    (length-iter lst 1)))
(define length-iter
  (lambda (lst count)
    (cond ((null? lst) 0)
          ((null? (cdr lst))count)
          (else (length-iter (cdr lst) (+ count 1))))))

;P05 - Reverse a list.
;(reverse lst)
(define reverse-lst
  (lambda (lst)
    (reverse-iter lst '())))
(define reverse-iter
  (lambda (old new)
    (cond ((null? (cdr old)) (cons (car old) new))
          (else (reverse-iter (cdr old) (cons (car old) new))))))

;P06 - Find out whether a list is a palindrome.
(define palindrome?
  (lambda (lst)
    (equal? lst (reverse lst))))

;P07 - Flatten a nested list structure.
(define flatten
  (lambda (lst)
    (flatten-iter lst '())))
(define flatten-iter
  (lambda (old new)
    (cond ((null? old) new)
          ((pair? (car old)) (flatten-iter (cdr old) (append new (flatten-iter (car old) '()))))
          (else (flatten-iter (cdr old) (append new (list (car old))))))))

;P08 - Eliminate consecutive duplicates of list elements.
(define compress 
  (lambda (lst)
    (compress-iter lst '())))
(define compress-iter
  (lambda (lst new)
    (cond ((null? (cdr lst)) (append new (list (car lst))))
          ((= (car lst) (cadr lst)) (compress-iter (cdr lst) new))
          (else (compress-iter (cdr lst) (append new (list (car lst))))))))

;P09 - Pack consecutive duplicates of list elements into sublists.
(define pack
  (lambda (lst)
    (reverse (pack-iter lst '() '()))))
(define pack-iter
  (lambda (lst new prev)
    (cond ((null? lst) new)
          ((eqv? (car lst) prev)(pack-iter (cdr lst) (cons (append (car new) (list (car lst))) (cdr new)) (car lst)))
          (else (pack-iter (cdr lst) (cons (list (car lst)) new) (car lst))))))

    
;P10 - Run-length encoding of a list.



