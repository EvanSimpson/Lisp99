(#%require (only racket/base random))

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
    (if (= i 1)
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
          ((equal? (car lst) (cadr lst)) (compress-iter (cdr lst) new))
          (else (compress-iter (cdr lst) (append new (list (car lst))))))))

;P09 - Pack consecutive duplicates of list elements into sublists.
(define pack
  (lambda (lst)
    (reverse (pack-iter lst '() '()))))
(define pack-iter
  (lambda (lst new prev)
    (cond ((null? lst) new)
          ((equal? (car lst) prev)(pack-iter (cdr lst) (cons (append (car new) (list (car lst))) (cdr new)) (car lst)))
          (else (pack-iter (cdr lst) (cons (list (car lst)) new) (car lst))))))

    
;P10 - Run-length encoding of a list.
(define encode
  (lambda (lst)
    (encode-iter (pack lst) '())))
(define encode-iter
  (lambda (lst new)
    (cond ((null? lst) new)
          (else (encode-iter (cdr lst) (append new (list (list (length-of (car lst)) (caar lst)))))))))

    
;P11 - Modified run-length encoding.
(define encode-modified
  (lambda (lst)
    (en-mod-iter (pack lst) '())))
(define en-mod-iter
  (lambda (lst new)
    (cond ((null? lst) new)
          ((= (length-of (car lst)) 1) (en-mod-iter (cdr lst) (append new (list (caar lst)))))
          (else (en-mod-iter (cdr lst) (append new (list (list (length-of (car lst)) (caar lst)))))))))

               
;P12 - Decode a run-length encoded list.
(define decode
  (lambda lst
    (decode-iter (car lst) '())))
(define decode-iter
  (lambda (lst new)
    (cond ((null? lst) new)
          ((pair? (car lst)) (decode-iter (cdr lst) (append new (mult-sym (caar lst) (cadar lst)))))
          (else (decode-iter (cdr lst) (append new (list (car lst))))))))


;Helpful process.
(define mult-sym
  (lambda (count sym)
    (cond ((= count 0) '())
          (else (cons sym (mult-sym (- count 1) sym))))))
    
;P13 - Run-length encoding of a list (direct solution)
(define encode-direct 
  (lambda lst
    (en-dir-iter (car lst) '() 1)))
(define en-dir-iter
  (lambda (lst new count)
    (cond ((null? (cdr lst)) (if (= count 1)
                                 (append new (list (car lst)))
                                 (append new (list (list count (car lst))))))
          ((= (car lst) (cadr lst)) (en-dir-iter (cdr lst) new (+ count 1)))
          (else (if (= count 1)
                    (en-dir-iter (cdr lst) (append new (list (car lst))) 1)
                    (en-dir-iter (cdr lst) (append new (list (list count (car lst)))) 1))))))


;P14 - Duplicate the elements of a list.
(define dupli
  (lambda lst
    (mult-iter (car lst) 2 '())))

;P15 - Replicate the elements of a list a given number of times.
(define repli
  (lambda (lst times)
    (mult-iter lst times '())))
(define mult-iter
  (lambda (lst times new)
    (cond ((null? lst) new)
          (else (mult-iter (cdr lst) times (append new (mult-sym times (car lst))))))))

;P16 - Drop every N'th element from a list.
(define drop 
  (lambda (lst n)
    (drop-iter lst (- n 1) '() 0)))
(define drop-iter
  (lambda (lst n new count)
    (cond ((null? lst) new)
          ((= count n) (drop-iter (cdr lst) n new 0))
          (else (drop-iter (cdr lst) n (append new (list (car lst))) (+ count 1))))))

;P17 - Split a list into two parts; the length of the first part is given.
(define split
  (lambda (lst mark)
    (split-iter lst mark 0 '() '())))
(define split-iter
  (lambda (lst mark count new1 new2)
    (cond ((null? lst) (list new1 new2))
          ((= mark count) (split-iter (cdr lst) mark count new1 (append new2 (list (car lst)))))
          (else (split-iter (cdr lst) mark (+ count 1) (append new1 (list (car lst))) new2 )))))

;P18 - Extract a slice from a list.
(define slice
  (lambda (lst i k)
    (car (split (cadr (split lst (- i 1))) (- k (- i 1) )))))

;P19 - Rotate a list N places to the left.
(define rotate
  (lambda (lst rot)
    (cond ((> 0 rot) (rotate lst (+ (length-of lst) rot)))
          (else (append (cadr (split lst rot)) (car (split lst rot)))))))

;P20 - Remove the K'th element from a list.
(define remove-at 
  (lambda (lst i)
    (append (car (split lst (- i 1))) (cadr (split lst i)))))

;P21 - Insert an element at a given position into a list.
(define insert-at
  (lambda (new lst i)
    (append (car (split lst (- i 1))) (cons new (cadr (split lst (- i 1)))))))

;P22 - Create a list containing all integers within a given range
(define range
  (lambda (i k)
    (range-iter i k '())))
(define range-iter
  (lambda (i k new)
    (cond ((= i k) (append new (list i)))
          (else (range-iter (step i k) k (append new (list i)))))))

;Helper process
(define step
  (lambda (i k)
    (if (> i k)
        (- i 1)
        (+ i 1))))

;P23 - Extract a given number of randomly selected elements from a list.
(define rnd-select
  (lambda (lst n)
    (rnd-sel-iter n '() (+ 1 (random (- (length-of lst) 1))) lst)))
(define rnd-sel-iter
  (lambda (n new i lst)
    (cond ((= n (length-of new)) new)
          ((= 1 (length-of lst)) (append new lst))
          (else (rnd-sel-iter n (append new (list (element-at lst i))) (+ 1 (random (- (length-of lst) 1) )) (remove-at lst i ))))))


;P24 - Lotto: Draw N different random numbers from the set 1....M.
(define lotto-select
  (lambda (n m)
    (rnd-select (range 1 m) n)))

;P25 - Generate a random permutation of the elements of a list.
(define rnd-permu
  (lambda lst
    (rnd-select (car lst) (length-of (car lst)))))

;P26 - Generate the combinations of K distinct objects chosen from the N elements of a list.

;P27 - Group the elements of a set into disjoint subsets.

;P28 - Dorting a list of lists according to length of sublists.


;Modulus
(define %
  (lambda (num denom)
    (- num (* denom (quotient num denom)))))

;P31 - Determine whether a given integer number is prime.
(define is-prime
  (lambda n
    (cond ((= (car n) 1) #t)
          ((= (car n) 2) #t)
          (else (prime-iter (car n) 2)))))
(define prime-iter
  (lambda (n guess)
    (cond ((= n guess) #t)
          ((= 0 (% n guess)) #f)
          (else (prime-iter n (+ 1 guess))))))


  

