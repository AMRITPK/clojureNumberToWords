(def digit-to-word
  {1 "One"
   2 "Two"
   3 "Three"
   4 "Four"
   5 "Five"
   6 "Six"
   7 "Seven"
   8 "Eight"
   9 "Nine"})

(def tens-to-word
  {
   20 "Twenty"
   30 "Thirty"
   40 "Fourty"
   50 "Fifty"
   60 "Sixty"
   70 "Seventy"
   80 "Eighty"
   90 "Ninety"})

(def teen-to-word
  {10 "Ten"
   11 "Eleven"
   12 "Twelve"
   13 "Thirteen"
   14 "Fourteen"
   15 "Fifteen"
   16 "Sixteen"
   17 "Seventeen"
   18 "Eighteen"
   19 "Nineteen"})

(defn- small-number-to-words
  [x]
  (cond
   (<= x 9) (digit-to-word x)
   (< x 20) (teen-to-word x)
   (< x 100)
 
   (let [ones-part (mod x 10)
         tens-part (- x ones-part)]
     (str (tens-to-word tens-part) " "
          (digit-to-word ones-part)))


   (<= x 999)
   (let [small-part (mod x 100)
         hundreds-digit (-> x  (- small-part) (/ 100))]
     (str (digit-to-word hundreds-digit) " Hundred  " 
			(cond (not= small-part 0) (str "and ")) 
				(small-number-to-words small-part)))
	
   (<= x 99999)
   (let [small-part (mod x 100)
         hundreds-digit (-> x (- small-part) (/ 100) (mod 10)) 
		 ten-thousand-digit (-> x (- small-part) (- (* hundreds-digit 100)) (/ 1000))]
     (str (small-number-to-words ten-thousand-digit) " Thousand " 
			(digit-to-word hundreds-digit) " Hundred  " 
				(cond (not= small-part 0) (str "and ")) 
					(small-number-to-words small-part)))

	(<= x 9999999)
    (let [small-part (mod x 100)
         hundreds-digit (-> x (- small-part) (/ 100) (mod 10))
		 ten-thousand-digit (-> x (- small-part) (- (* hundreds-digit 100)) (/ 1000) (mod 100))
		 lakh-digit (-> x (- small-part) (- (* hundreds-digit 100)) (- (* ten-thousand-digit 1000)) (/ 100000))]
     (str	(small-number-to-words lakh-digit) " Lakh "
       			(small-number-to-words ten-thousand-digit) " Thousand " 
					(digit-to-word hundreds-digit) " Hundred  " 
						(cond (not= small-part 0) (str "and ")) 
							(small-number-to-words small-part)))

	(<= x 999999999)
    (let [small-part (mod x 100)
         hundreds-digit (-> x (- small-part) (/ 100) (mod 10))
		 ten-thousand-digit (-> x (- small-part) (- (* hundreds-digit 100)) (/ 1000) (mod 100))
		 lakh-digit (-> x (- small-part) (- (* hundreds-digit 100)) (- (* ten-thousand-digit 1000)) (/ 100000) (mod 100))
		 ten-crore-digit (-> x (- small-part) (- (* hundreds-digit 100)) (- (* ten-thousand-digit 1000)) (- (* lakh-digit 100000)) (/ 10000000))

			]
     (str	(small-number-to-words ten-crore-digit) " Crore "
       			(small-number-to-words lakh-digit) " Lakh "
       				(small-number-to-words ten-thousand-digit) " Thousand " 
						(digit-to-word hundreds-digit) " Hundred  " 
							(cond (not= small-part 0) (str "and ")) 
								(small-number-to-words small-part)))

	))

(do (println (small-number-to-words 400000)))
