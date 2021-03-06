(ns euler.core
  (:use euler.utils)
  (:use euler.resources)
  (:use clojure.core.typed)
  (:require [clojure.math.numeric-tower :as math]))

(comment
  (check-ns)
  )

(ann prob1 [-> Int])
(defn prob1
  "Sum of all the multiple of 3 or 5 below 1000"
  [] (reduce + (filter (fn> [n :- Int]
                            (or (divides? n 3)
                                (divides? n 5)))
                       (range 1000))))
(ann prob2 [-> Int])
(defn prob2
  "Sum of even Fib numbers that do not exceed 4,000,000"
  [] (reduce + (filter even? (take-while #(< % 4000000) (fib)))))


(ann prob3 [-> Int])
(defn prob3
  "Largest prime factor of 600851475143"
  [] (last (factor 600851475143)))

(ann prob4 [-> Int])
(defn prob4 
  "Largest palidrome of the product of two 3 digit numbers"
  [] (last (sort 
             (for [x (range 100 1000)
                   y (range x 1000)
                   :when (palindrome? (* x y))]
               (* x y)))))

(ann prob5 [-> Int])
(defn prob5
  "Smallest positive number that is evenly divisible by all of the numbers from 1 to 20"
  [] (first (filter (fn [n] (every? #(divides? n %) (range 1 21))) 
                    (iterate (partial + 20) 20))))

(ann prob6 [-> Int])
(defn prob6
  "sum square difference for the first 100 natural numbers"
  [] (- (square (reduce + (range 1 101)))
        (reduce #(+ %1 (square %2)) (range 1 101)))) 

(ann prob7 [-> Int])
(defn prob7
  "10001st prime number"
  [] (nth (filter is-prime? (iterate inc 2)) 10000))

(ann prob8 [-> Int])
(defn prob8
  "Largest product of 5 consecutive digits in euler8"
  [] (apply max (map (partial apply *) (partition 5 1 (map #(Integer/parseInt (str %)) euler-8)))))


(ann prob9 [-> Int])
(defn prob9
  "a*b*c where a+b+c = 1000 && a^2 + b^2 = c^2"
  [] (first (for [a (range 1 1000)
                  b (range a 1000)
                  c [(- 1000 a b)]
                  :when (= (+ (square a) (square b)) (square c))]
              (* a b c))))


(ann prob10 [-> Int])
(defn prob10
  "sum of primes below 2000000"
  [] (let [limit 2000000](reduce + (filter #(and (< % limit) (is-prime? %)) (range 2 limit)))))

(ann prob11 [-> Int])
(defn prob11
  "Greatest product of four adjacent squares"
  [] 
  (apply max (map (partial reduce *)
                  (let [grid (parse-grid grid-11)
                        adjacent 4
                        limit (- 20 adjacent)]
                    (for [dir [[0 1] [1 1] [1 0] [1 -1]]
                          x (range limit)
                          y (range limit)]
                      (map #(apply (partial grid-value grid) %)
                           (take adjacent (iterate #(next-num % dir)
                                                   (if (= [1 -1] dir)  ; Special case for the last direction
                                                     (next-num [x y] [0 3])
                                                     [x y])))))))))

(ann prob12 [-> Int])
(defn prob12
  "First triangle number to have over 500 divisors (28: over 5 divisors)"
  [] (first (filter #(< 500 (count (find-factors %))) (triangle-numbers)))) 

(ann prob13 [-> Int])
(defn prob13
  "Sum of 100 50 digit numbers"
  [] (Long/parseLong (clojure.string/join (take 10 (str (reduce + euler-13))))))

(ann prob14 [-> Int])
(defn prob14
  "Longest Collatz sequence"
  [] (reduce #(if (> (count (collatz %1)) (count (collatz %2))) %1 %2) 
             (range 1 (inc 1000000))))

(ann prob15 [-> Int])
(defn prob15
  "Lattice paths for a 20x20 square"
  [] (let [limit 20]
       (long (/ (reduce * (take limit (iterate dec (bigint (* 2 limit)))))
          (fact limit)))))


(ann prob16 [-> Int])
(defn prob16
  "Sum of the digits of the number 2^1000"
  [] (sum-chars (math/expt 2 1000)))

(ann prob17 [-> Int])
(defn prob17
  "Sum of number of letters used to write digits from 1 - 1000"
  [] (reduce + (map letter-count (range 1 (inc 1000)))))

(ann prob18 [-> Int])
(defn prob18
  "Maximum Sum Path I"
  [] (let [input euler-18] 
       (loop [row (last input)
              rows (butlast input)]
         (if (nil? rows)
           (first row)
           (recur 
             (for [idx (range (dec (count row)))]
               (max (+ (nth (last rows) idx) (nth row idx)) 
                    (+ (nth (last rows) idx) (nth row (inc idx)))))
             (butlast rows))))))


(ann prob19 [-> Int])
(defn prob19
  "Counting Sundays that fell on the first of the month during the 20th century 1 jan 1901 to 31 dec 2000"
  [] ())

(ann prob20 [-> Int])
(defn prob20
  "Sum digits of 100!"
  [] (sum-chars (fact (bigint 100))))

(ann prob21 [-> Int])
(defn prob21
  "Evaluate the sum of all the amicable numbers under 10000."
  ([] (prob21 0 10000))
  ([start nums] 
   (let [amicable-numbers (map amicable-number  (range start (+ start nums)))]
     (->> (for [i (range 0 nums)
                :let [left (+ start i)
                      right (nth amicable-numbers i)]
                :when (and (= left (amicable-number right))
                           (< left right))]
            (conj '() (amicable-number left) left))
          flatten
          (reduce +)))))

