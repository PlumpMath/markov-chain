(ns markov-chain.strings
  (:require [markov-chain.core :refer :all]
            [clojure.string :as string]))

(defn- split-words [words] (map #(string/split % #"") words))
(defn- split-sentences [sentences] (map #(string/split % #"\s+") sentences))

(defn chain-words
  "make a markov chain from a seq of words"
  ([order words]
   (chain order (split-words words))))

(defn multichain-words
  "make a multichain from a seq of words"
  ([order words]
   (multichain order (split-words words))))

(defn chain-sentences
  "make a markov chain from a seq of whitespace-separated words"
  ([order sentences]
   (chain order (split-sentences sentences))))

(defn multichain-sentences
  "make a markov chain from a seq of whitespace-separated words"
  ([order sentences]
   (multichain order (split-sentences sentences))))
