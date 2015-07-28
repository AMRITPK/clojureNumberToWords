(ns number-to-words
  (:require [clojure.string :refer [join]]))

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
     (str (tens-to-word tens-part) "-"
          (digit-to-word ones-part)))


   (<= x 999)
   (let [small-part (mod x 100)
         hundreds-digit (-> x (- small-part) (/ 100))]
     (str (digit-to-word hundreds-digit) " Hundred " (small-number-to-words small-part)))))







( do( println ( small-number-to-words 899) ))