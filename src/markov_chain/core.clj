(ns markov-chain.core
  (:require [clojure.data.generators :as random]))

(def ^:dynamic ^:java.util.Random *rnd* random/*rnd*)

(defn- weighted
  "Delegate to data.generators with a bound *rnd*"
  [m]
  (binding [random/*rnd* *rnd*]
    (random/weighted m)))

(defn- prefix [order] (vec (repeat order nil)))

(defn- update-chain [order memo group]
  "update the count of the next state for the current state"
  (let [key (seq (take order group))
        next-key (last group)
        current-state (get memo key {})
        current-value (get current-state next-key 0)]
    (assoc-in memo [key next-key] (inc current-value))))

(defn- extend-chain
  "process a single seq from the input"
  [order chain input]
  (let [suffixed (conj input nil)
        bookended (into (prefix order) suffixed)
        group-size (inc order)
        grouped (partition group-size 1 bookended)]
    (reduce (partial update-chain order) chain grouped)))

(defn chain
  "makes a markov chain from a sequence of strings"
  ([order input]
   (reduce (partial extend-chain order) {} input)))

(defn multichain
  "Makes n markov chains from 0 to order.
  The returned multichain is a vector where the nth position corresponds
  to the nth order markov chain of the input"
  [order input]
  (reduce (fn [memo o] (conj memo (chain o input)))
          [] (range 0 (inc order))))

(defn- next-valid-sample
  [in-key input-multichain]
  (let [all-keys (map #(take-last % in-key) (range 0 (inc (count in-key))))
        key-chain (map vector all-keys input-multichain)
        valid-key-chain? (fn [[k c]] (get c k))
        [next-key next-chain] (last (filter valid-key-chain? key-chain))]
    (weighted (get next-chain next-key))))

(defn sample
  "use a multichain to generate sample output.
  Will use a lower-level chain as a fallback if a given pattern isn't found"
  ([order input-multichain seed]
   (loop [output []
          key seed]
     (let [next (next-valid-sample key input-multichain)
           new-output (conj output next)
           new-key (take-last order new-output)]
       (if (nil? next) output (recur new-output new-key)))))
  ([order input-chain]
   (sample order input-chain (prefix order))))
