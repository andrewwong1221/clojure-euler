(ns euler.core-test
  (:use clojure.test
        euler.core))

(deftest test1
  (testing "Problem 1"
    (is (= 233168 (prob1)))))

(deftest test2
  (testing "Problem 2"
    (is (= 4613732 (prob2)))))

(deftest test3
  (testing "Problem 3"
    (is (= 6857 (prob3)))))

(deftest test4
  (testing "Problem 4"
    (is (= 906609 (prob4)))))

(deftest test5
  (testing "Problem 5"
    (is (= 232792560 (prob5)))))

(deftest test6
  (testing "Problem 6"
    (is (= 25164150 (prob6)))))

(deftest test7
  (testing "Problem 7"
    (is (= 104743 (prob7)))))

(deftest test8
  (testing "Problem 8"
    (is (= 40824 (prob8)))))

(deftest test9
  (testing "Problem 9"
    (is (= 31875000 (prob9)))))

(deftest test10
  (testing "Problem 10"
    (is (= 142913828922 (prob10)))))

(deftest test11
  (testing "Problem 11"
    (is (= 70600674 (prob11)))))

(deftest test12
  (testing "Problem 12"
    (is (= 76576500 (prob12)))))

(deftest test13
  (testing "Problem 13"
    (is (= 5537376230 (prob13)))))

(deftest test14
  (testing "Problem 14"
    (is (= 837799 (prob14)))))

(deftest test15
  (testing "Problem 15"
    (is (= 137846528820 (prob15)))))

(deftest test16
  (testing "Problem 16"
    (is (= 1366 (prob16)))))

(deftest test17
  (testing "Problem 17"
    (is (= 21124 (prob17)))))

(deftest test18
  (testing "Problem 18"
    (is (= 1074 (prob18)))))

(comment (deftest test19
  (testing "Problem 19"
    (is (= 1074 (prob19))))))

(deftest test20
  (testing "Problem 20"
    (is (= 648 (prob20)))))

(deftest test21
  (testing "Problem 21"
    (is (= 31626 (prob21)))))
