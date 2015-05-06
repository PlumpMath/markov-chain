(ns markov-chain.strings-test
  (:require [clojure.test :refer :all]
            [markov-chain.core :refer :all]
            [markov-chain.strings :refer :all]
            [markov-chain.test-utils :refer :all]))

(deftest chain-words-test
  (testing "chain-words"
    (is (= (chain-words 1 ["babel"]) (chain 1 babel-vec)))))
