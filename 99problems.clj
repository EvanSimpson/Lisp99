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






