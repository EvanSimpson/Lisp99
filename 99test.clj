(load-file "./99problems.clj")

(if (= 4 (my-last '(1 2 3 4)))
	(println "Test 1 passed")
	(println "Test failed: my-last"))

(if (= 3 (my-but-last '(1 2 3 4)))
	(println "Test 2 passed")
	(println "Test failed: my-but-last"))

(if (= 2 (element-at '(1 2 3 4) 1))
	(println "Test 3 passed")
	(println "Test failed: element-at"))

(if (= 4 (cnt '(1 2 3 4)))
	(println "Test 4 passed")
	(println "Test failed: cnt"))

(if (= '(4 3 2 1) (rev '(1 2 3 4)))
	(println "Test 5 passed")
	(println "Test failed: rev"))

(if (and (is-palindrome '(4 3 3 4)) (not(is-palindrome '(1 2 3 4))))
	(println "Test 6 passed")
	(println "Test failed: is-palindrome"))

(if (= '(1 2 3 4) (my-flatten '(1 (2 (3 4) 5))))
	(println "Test 7 passed")
	(println "Test failed: my-flatten"))

(comment
(if ()
	(println "Test  passed")
	(println "Test failed: "))

(if ()
	(println "Test  passed")
	(println "Test failed: "))

(if ()
	(println "Test  passed")
	(println "Test failed: "))

(if ()
	(println "Test  passed")
	(println "Test failed: "))

(if ()
	(println "Test  passed")
	(println "Test failed: "))

(if ()
	(println "Test  passed")
	(println "Test failed: "))

(if ()
	(println "Test  passed")
	(println "Test failed: "))

)