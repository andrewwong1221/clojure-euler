(ns euler.utils-test
  (:use clojure.test
        euler.utils))

(deftest num-digits-test
  (testing "num-digits"
    (is (= 2 (num-digits 10)))
    (is (= 1 (num-digits 1)))
    (is (= 1 (num-digits 0)))
    (is (= 2 (num-digits 50)))
    (is (= 3 (num-digits 999)))))

(deftest digit-at-test
  (testing "digit-at"
    (let [testnum (partial digit-at 123456789)]
      (is (= 1 (testnum 0)))
      (is (= 2 (testnum 1)))  
      (is (= 3 (testnum 2)))  
      (is (= 4 (testnum 3)))  
      (is (= 5 (testnum 4)))  
      (is (= 6 (testnum 5)))  
      (is (= 7 (testnum 6)))  
      (is (= 8 (testnum 7)))  
      (is (= 9 (testnum 8)))
      (is (nil? (testnum 9))))))

(deftest is-palindrome-test
  (testing "is-palindrome?"
    (is (is-palindrome? 1))
    (is (is-palindrome? 121))
    (is (is-palindrome? 1221))
    (is (is-palindrome? 0))
    (is (is-palindrome? 906609))))

(deftest proper-divisors-test
  (testing "proper-divisors"
    (is (= (proper-divisors 10) '(1 2 5)))
    (is (= (proper-divisors 220)) '(1 2 4 5 10 11 20 22 44 55 110))))

(deftest amicable-number-test
  (testing  "amicable-number"
    (is (= 284 (amicable-number 220)))
    (is (= 220 (amicable-number 284)))))

(deftest without-nth-test
  (testing "without-nth"
    (let [testseq '(1 2 3 4 5)]
      (is (= (without-nth testseq 2) '(1 2 4 5)))
      (is (= (without-nth testseq 0) '(2 3 4 5)))
      (is (= (without-nth testseq 4) '(1 2 3 4)))
      (is (= (without-nth testseq 5) '(1 2 3 4 5)))
      (is (= (without-nth testseq -15) '(1 2 3 4 5)))
      (is (= (without-nth testseq -1) '(1 2 3 4))))))
