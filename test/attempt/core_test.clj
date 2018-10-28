(ns attempt.core-test
  (:require [attempt.core :refer :all]
            [clojure.test :refer [deftest testing is]]))

(deftest if-let-*-tests
  (is (= :failure
         (if-let-failure? [_ (fail {:field "oeps"})]
           :failure
           :success)))
  (is (= :success
         (if-let-failure? [_ {}]
           :failure
           :success)))

  (is (= :success
         (if-let-success? [_ {}]
           :success
           :failure)))
  (is (= :failure
         (if-let-success? [_ (fail "oeps")]
           :success
           :failure))))

(deftest attemp-tests
  (testing "behaves like regular let if no fails"
    (is (= [2 1]
           (attempt [a 1
                     a 2
                     [_ b] [1 1]]
             [a b]
             (fn when-failed [f]
               [0 0])))))

  (testing "shortcircuits with payload when failed"
    (is (= "failed!"
           (attempt [a 1
                     a (fail "failed!")
                     b 1]
             [a b]
             (fn when-failed [f]
               (:payload f)))))))

(deftest attempt->-test
  (testing "behaves like regular -> if no fails"
    (is (= "it works"
           (attempt-> ""
                      (str "it")
                      (str " ")
                      (str "works")))))
  (testing "shortcircuits when failed"
    (is (= (->Failure 1)
           (attempt-> 1
                      inc
                      dec
                      fail
                      (+ 2))))))

(deftest attempt->>-test
  (testing "behaves like regular -> if no fails"
    (is (= "it works"
           (attempt->> "works"
                       (str " ")
                       (str "it")))))
  (testing "shortcircuits when failed"
    (is (= (->Failure 1)
           (attempt->> 1
                      inc
                      dec
                      fail
                      (+ 2))))))
