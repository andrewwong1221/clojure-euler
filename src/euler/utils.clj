(ns euler.utils
  (:require [clojure.math.numeric-tower :as math]))

(defn divides?
  "Returns true if a % b == 0"
  [a b] (= 0 (rem a b)))
(defn fib 
  "Lazy fibbonacci sequence"
  ([a b] (cons a (lazy-seq (fib b (+ b a)))))
  ([] (fib 1 1)))

(defn smallest-divisor 
  "Returns the smallest divisor"
  ([n] (smallest-divisor n 2))
  ([n test-divisor]
   (cond (> (math/expt test-divisor 2) n) n
         (divides? n test-divisor) test-divisor
         :else (recur n (+ 1 test-divisor)))))

(defn is-prime?
  "Determines if the number is prime"
  [n] (= n (smallest-divisor n)))


(comment (defn primes 
  "Lazy seq of primes"
  ([s] (cons (first s)
        (lazy-seq (primes (filter #(not= 0 (mod % (first s))) (rest s))))))))

(defn factor 
  "Creates list of prime factors for the given number" 
  ([n] (factor n []))
  ([n factors]
   (cond (= n 1) factors
         :else (let [divisor (smallest-divisor n)]
                 (recur (/ n divisor) (conj factors divisor))))))

(defn find-factors
  "Generates list of factors for a given number"
  [n] (let [ffactors (filter (partial divides? n) (range 1 (inc' (int (math/sqrt n)))))
            rfactors (map (partial / n) ffactors)]
        (set (apply conj ffactors rfactors))))


(defn num-digits 
  "Returns the number of digits in the number"
  [n] (if-not (zero? n)
       (int (+ (math/floor (Math/log10 n)) 1))
        1))

(defn digit-at [n place]
  "Only for base-10 currently"
  (if (>= place (num-digits n)) 
    nil
    (let [divisor (math/expt 10 (- (dec' (num-digits n)) place))]
      (rem (int (/ n  divisor)) 10))))

(defn square
  "Squares a number"
  [n] (* n n))

(defn is-palindrome? 
  "Returns true if number is a palindrome"
  ([n] (is-palindrome? n 0 (dec' (num-digits n))) )
  ([n start end]
   (cond 
     (>= start end) true
     (= (digit-at n start) (digit-at n end)) (recur n (inc' start) (dec' end))
         :else false ))) 

; Problem 11

(defn parse-grid
  "Turns grid into ints"
  [grid] (map #(map (fn [n] (Integer/parseInt n)) %)
              (map #(clojure.string/split % #"\s+") (clojure.string/split-lines grid))))

(defn grid-value
  "Returns the value at the specified coordinate"
  [grid x y] (nth (nth grid x) y))


(defn next-num [[x y] [dx dy]] (vector (+ x dx) (+ y dy)))

; Problem 12

(defn triangle-numbers
  "Lazyseq of triangle numbers"
  ([] (triangle-numbers 2 1))
  ([n m] (cons m (lazy-seq (triangle-numbers (inc n) (apply + (range (inc n))))))))

(defn collatz
  "Generate lazy collatz sequence"
  ([n] (cons n (lazy-seq (when (not= n 1)
                           (collatz (if (even? n) 
                                           (/ n 2)
                                           (+ (* 3 n) 1))))))))

(defn explore 
  ([limit] (explore 0 0 limit))
  ([x y limit]
  (let [right (if (< x limit) (explore (inc x) y limit) 0)
        down  (if (< y limit) (explore x (inc y) limit) 0)]
    (if (and (= x limit) (= y limit))
      1
      (+ right down)))))

(defn fact
  "Return the factorial of n"
  [n] (reduce * (take n (iterate dec n))))

(defn letter-count
  [n] (cond 
        (= n 1000) (+ (letter-count (int (/ n 1000))) 8 (letter-count (rem n 1000)))
        (>= n 100) (+ (letter-count (int (/ n 100))) 7 
                      (if (divides? n 100) 0 3) (letter-count (rem n 100)))
        (>= n 80) (+ (letter-count (rem n 10)) 6)
        (>= n 70) (+ (letter-count (rem n 10)) 7)
        (>= n 40) (+ (letter-count (rem n 10)) 5)
        (>= n 20) (+ (letter-count (rem n 10)) 6)
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
        (= n 19) 8))

(defn sum-chars
  "Sums the digits in a number"
  [n] (reduce + (map #(Character/digit % 10) (str n))))
