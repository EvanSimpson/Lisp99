;P01 - Find the last box of a list
(defn my-last [lst]
	(if (= 1 (count lst))
		(first lst)
		(my-last (rest lst))))

;P02 - Find the last but one box of a list.
(defn my-but-last [lst]
	(if (= 2 (count lst))
		(first lst)
		(my-but-last (rest lst))))

;P03 - Find the k'th element of a list.
(defn element-at [lst i]
	(if (= 0 i)
		(first lst)
		(element-at (rest lst) (dec i))))

;P04 (*) Find the number of elements of a list.	
(defn cnt [lst]
	(loop [lst lst
		   n 0]
		(if (= () lst)
			n
			(recur (rest lst) (inc n)))))

;P05 (*) Reverse a list.
(defn rev [lst]
	(loop [lst lst
		   revd ()]
		(if (= () lst)
			revd
			(recur (rest lst) (cons (first lst) revd)))))

;P06 (*) Find out whether a list is a palindrome.
(defn is-palindrome [lst]
	(if (= lst (rev lst))
		true
		false))

;P07 (**) Flatten a nested list structure.
(defn my-flatten [lst]
	(loop [lst lst
			flat ()]
		(cond 
			(= 0 (count lst)) flat
			(list? (first lst)) (recur (rest lst) (concat flat (my-flatten (first lst))))
			:else	(recur (rest lst) (concat flat (list (first lst)))))))

;P08 (**) Eliminate consecutive duplicates of list elements.
(defn compress [lst]
	(loop [lst lst
		   compr ()]
		 (cond 
		 	(= 0 (count lst)) (reverse compr)
		 	(= (first compr) (first lst)) (recur (rest lst) compr)
		 	:else (recur (rest lst) (cons (first lst) compr)))))

;P09 (**) Pack consecutive duplicates of list elements into sublists.
(defn pack [lst]
	(loop [lst   lst
		   packd ()
		   prev  nil]
		(cond
			(= 0 (count lst)) (reverse packd)
			(= (first lst) prev) (recur (rest lst) (conj (rest packd) (cons (first lst) (first packd))) (first lst))
			:else (recur (rest lst) (conj packd (list (first lst))) (first lst))
			)))

;P10 (*) Run-length encoding of a list.
(defn encode [lst]
	(loop [lst (pack lst)
		   enc ()]
		(cond
			(= 0 (count lst)) (reverse enc)
			:else (recur (rest lst) (conj enc (list (count (first lst)) (first (first lst)))))
			)))

;P11 (*) Modified run-length encoding.
(defn encode-modified [lst]
	(loop [lst (pack lst)
		   enc ()]
		(cond
			(= 0 (count lst)) (reverse enc)
			(= 1 (count (first lst))) (recur (rest lst) (cons (first (first lst)) enc))
			:else (recur (rest lst) (conj enc (list (count (first lst)) (first (first lst)))))
			)))

;P12 (**) Decode a run-length encoded list.
(defn decode [lst]
	(loop [lst lst
		   deco ()]
		(cond
			(= 0 (count lst)) (reverse deco)
			(list? (first lst)) (recur (rest lst) (concat (repeat (first (first lst)) (last (first lst))) deco))
			:else (recur (rest lst) (cons (first lst) deco))
			)))

;P13 (**) Run-length encoding of a list (direct solution).
(defn encode-direct [lst]
	(loop [lst lst
		   enc ()
		   cnt 1]
		(cond
			(= 0 (count lst)) (reverse enc)
          	(= (first lst) (first (rest lst))) (recur (rest lst) enc (inc cnt))
          	:else (if (= cnt 1)
                    (recur (rest lst) (cons (first lst) enc)1)
                    (recur (rest lst) (cons (list cnt (first lst)) enc) 1))
			)))