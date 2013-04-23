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

(if (= '(1 2 3 4 5) (my-flatten '(1 (2 (3 4) 5)) ))
	(println "Test 7 passed")
	(println "Test failed: my-flatten"))

(if (= '(1 2 3 4) (compress '(1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4)))
	(println "Test 8 passed")
	(println "Test failed: compress"))

(if (= '((1 1 1 1) (2 2 2 2) (3) (4 4)) (pack '(1 1 1 1 2 2 2 2 3 4 4)))
	(println "Test 9 passed")
	(println "Test failed: pack"))

(if (= '((4 1) (4 2) (1 3) (2 4)) (encode '(1 1 1 1 2 2 2 2 3 4 4)))
	(println "Test 10 passed")
	(println "Test failed: encode"))

(if (= '(1 (4 2) 3 (2 4)) (encode-modified '(1 2 2 2 2 3 4 4)))
	(println "Test 11 passed")
	(println "Test failed: encode-modified"))

(if (= '(1 2 2 2 2 3 4 4) (decode (encode-modified '(1 2 2 2 2 3 4 4))))
	(println "Test 12 passed")
	(println "Test failed: decode"))

(if (= (encode-modified '(1 2 2 2 2 3 4 4)) (encode-direct '(1 2 2 2 2 3 4 4)))
	(println "Test 13 passed")
	(println "Test failed: encode-direct"))
(comment
(if ()
	(println "Test 14 passed")
	(println "Test failed: "))

(if ()
	(println "Test 15 passed")
	(println "Test failed: "))

(if ()
	(println "Test 16 passed")
	(println "Test failed: "))

(if ()
	(println "Test 17 passed")
	(println "Test failed: "))

(if ()
	(println "Test 18 passed")
	(println "Test failed: "))

(if ()
	(println "Test 19 passed")
	(println "Test failed: "))

(if ()
	(println "Test 20 passed")
	(println "Test failed: "))

)