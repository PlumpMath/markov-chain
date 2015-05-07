(ns markov-chain.core-test
  (:require [clojure.test :refer :all]
            [markov-chain.core :refer :all]
            [markov-chain.strings :refer :all]
            [markov-chain.test-utils :refer :all]
            [clojure.string :as string])
  (:import (java.util Random)))

(deftest chain-test
  (testing "chain with one sequence"
    (let [babel-chain-one {'(nil) {"b" 1}
                           '("b") {"a" 1 "e" 1}
                           '("a") {"b" 1}
                           '("e") {"l" 1}
                           '("l") {nil 1}}
          babel-chain-two {'(nil nil) {"b" 1}
                           '(nil "b") {"a" 1}
                           '("b" "a") {"b" 1}
                           '("a" "b") {"e" 1}
                           '("b" "e") {"l" 1}
                           '("e" "l") {nil 1}}]
      (is (= babel-chain-one (chain 1 babel-vec)))
      (is (= babel-chain-two (chain 2 babel-vec))))))

(deftest multichain-test
  (testing "2-chain, one sequence"
    (is (= [(chain 0 babel-vec) (chain 1 babel-vec) (chain 2 babel-vec)]
           (multichain 2 babel-vec)))))

(deftest sample-test
  (testing "sampling multichains"
    (binding [*rnd* (Random. 42)]
      (let [model (multichain-words 2 lorem-ipsum)
            samples (repeatedly #(sample 2 model))]
        (is (= (w% riscit exerunt ad strunt eur) (mapv string/join (take 5 samples))))))))
