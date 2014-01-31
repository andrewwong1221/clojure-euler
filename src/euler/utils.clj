(ns euler.utils
  (:require [clojure.math.numeric-tower :as math])
  (:use [clojure.core.typed])
  (:import [java.lang Character]))

; Numeric Tower
(ann ^:no-check clojure.math.numeric-tower/sqrt [Number -> Number])
(ann ^:no-check clojure.math.numeric-tower/expt [Number Number -> Number])
;(ann ^:no-check clojure.string/split-lines [String -> (Seq String)])

(comment
  (check-ns)
  )

(ann divides? [Int Int -> boolean])
(defn divides?
  "Returns true if a % b == 0"
  [a b] (zero? (rem a b)))

(ann fib (Fn [-> (Seq Int)]
             [Int Int -> (Seq Int)]))
(defn fib 
  "Lazy fibbonacci sequence"
  ([a b] (cons a (lazy-seq (fib b (+ b a)))))
  ([] (fib 1 1)))

(ann smallest-divisor (Fn [Int -> Int]
                          [Int Int -> Int]))
(defn smallest-divisor 
  "Returns the smallest divisor"
  ([n] (smallest-divisor n 2))
  ([n test-divisor]
   (cond (> (math/expt test-divisor 2) n) n
         (divides? n test-divisor) test-divisor
         :else (recur n (inc test-divisor)))))

(ann is-prime? [Int -> boolean])
(defn is-prime?
  "Determines if the number is prime"
  [n] (= n (smallest-divisor n)))


(comment (defn primes 
  "Lazy seq of primes"
  ([s] (cons (first s)
        (lazy-seq (primes (filter #(not= 0 (mod % (first s))) (rest s))))))))

(ann factor (Fn [Int -> (Seq Int)]
                [Int (Seq Int) -> (Seq Int)]))
(defn factor 
  "Creates list of prime factors for the given number" 
  ([n] (factor n '()))
  ([n factors]
   (cond (= n 1) factors
         :else (let [divisor (smallest-divisor n)]
                 (recur (long (/ n divisor)) (conj factors (long divisor)))))))


(ann ^:no-check find-factors [Int -> (Set Int)])
(defn find-factors
  "Generates list of factors for a given number"
  [n] (let [ffactors (filter (partial divides? n) (range 1 (inc (long (math/sqrt n)))))
            rfactors (map (partial / n) ffactors)]
        (set (apply conj ffactors rfactors))))


(ann num-digits [Int -> Int])
(defn num-digits 
  "Returns the number of digits in the number"
  [n] (if-not (zero? n)
       (int (inc (math/floor (Math/log10 (double n)))))
        1))

(ann digit-at [Int Int -> (Nilable Int)])
(defn digit-at 
  "Only for base-10"
  ([n place]
   (when-not (>= place (num-digits n)) 
     (let [divisor (math/expt 10 (- (dec (num-digits n)) place))]
       (rem (long (/ n  divisor)) 10)))))

(ann square [Int -> Int])
(defn square
  "Squares a number"
  [n] (* n n))

(ann palindrome? (Fn [Int -> boolean]
                        [Int Int Int -> boolean]))
(defn palindrome? 
  "Returns true if number is a palindrome"
  ([n] (palindrome? n 0 (dec (num-digits n))) )
  ([n start end]
   (cond 
     (>= start end) true
     (= (digit-at n start) (digit-at n end)) (recur n (inc start) (dec end))
         :else false ))) 

; Problem 11
;(cf Grid (def-alias Grid (Seqable (Seqable Int))))

(ann ^:no-check parse-grid [String -> (Seqable (Seqable Int))])
(defn parse-grid
  "Turns grid into ints"
  [grid] (map #(map (fn> [n :- String] (Integer/parseInt n)) %)
              (map #(clojure.string/split % #"\s+") (clojure.string/split-lines grid))))

(ann grid-value [(Seqable (Seqable Int)) Int Int -> Int])
(defn grid-value
  "Returns the value at the specified coordinate"
  [grid x y] (nth (nth grid x) y))


(ann next-num ['[Int Int] '[Int Int] -> '[Int Int]])
(defn next-num [[x y] [dx dy]] (vector (+ x dx) (+ y dy)))


; Problem 12

(ann triangle-numbers (Fn [-> (Seq Int)]
                          [Int Int -> (Seq Int)]))
(defn triangle-numbers
  "Lazyseq of triangle numbers"
  ([] (triangle-numbers 2 1))
  ([n m] (cons m (lazy-seq (triangle-numbers (inc n) (apply + (range (inc n))))))))

(ann collatz [Int -> (Seq Int)])
(defn collatz
  "Generate lazy collatz sequence"
  ([n] (cons n (lazy-seq (when (not= n 1)
                           (collatz (long (if (even? n) 
                                           (/ n 2)
                                           (inc (* 3 n))))))))))

(ann explore (Fn [Int -> Int]
                 [Int Int Int -> Int]))
(defn explore 
  ([limit] (explore 0 0 limit))
  ([x y limit]
  (let [right (if (< x limit) (explore (inc x) y limit) 0)
        down  (if (< y limit) (explore x (inc y) limit) 0)]
    (if (and (= x limit) (= y limit))
      1
      (+ right down)))))

(ann fact [Int -> Int])
(defn fact
  "Return the factorial of n"
  [n] (reduce * (take n (iterate dec n))))

(ann letter-count [Int -> Int])
(defn letter-count
  [n] (int (cond 
             (= n 1000) (+ (letter-count (int (/ n 1000))) 8 (letter-count (mod n 1000)))
             (>= n 100) (+ (letter-count (int (/ n 100))) 7 
                           (if (divides? n 100) 0 3) (letter-count (mod n 100)))
             (>= n 80) (+ (letter-count (mod n 10)) 6)
             (>= n 70) (+ (letter-count (mod n 10)) 7)
             (>= n 40) (+ (letter-count (mod n 10)) 5)
             (>= n 20) (+ (letter-count (mod n 10)) 6)
             (= n 0)  0
             (= n 1)  3
             (= n 2)  3
             (= n 3)  5
             (= n 4)  4
             (= n 5)  4
             (= n 6)  3
             (= n 7)  5
             (= n 8)  5
             (= n 9)  4
             (= n 10) 3
             (= n 11) 6
             (= n 12) 6
             (= n 13) 8
             (= n 14) 8
             (= n 15) 7
             (= n 16) 7
             (= n 17) 9
             (= n 18) 8
             (= n 19) 8
             :else 0)))

(ann get-digit [java.lang.Character -> Int])
(defn get-digit
  [^Character c] (Character/digit c 10))

(ann sum-chars [Int -> Int])
(defn sum-chars
  "Sums the digits in a number"
  [n] (reduce + (map (fn> [c :- java.lang.Character] (get-digit c)) (str n))))

(ann proper-divisors [Int -> (Seq Int)])
(defn proper-divisors
  [n] (filter (fn> [c :- Int] divides? n c) (range 1 (inc (/ n 2)))))

(ann amicable-number [Int -> Int])
(defn amicable-number
  "Calculates the sum of all factors <= input / 2"
  [n] (reduce + (proper-divisors n)))


(ann without-nth [Any [x] (Seqable x) Int -> (Seq x)])
(defn without-nth
  [coll n] (cond 
             (> (math/abs n) (dec (count coll))) coll
             (zero? n) (recur coll (+ (count coll) n))
             :else (let [v (vec coll)] (concat (subvec v 0 n) (subvec v (inc n) (count v))))))
