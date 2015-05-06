(ns markov-chain.core)

(defn- prefix [order] (vec (repeat order nil)))

;; TODO replace with external lib
(defn- weighted-choice
  "given a map of key: weight, randomly select a key using the appropriate weightings"
  ([key-weights]
   (let [total (reduce + (vals key-weights))
         choice (rand-int total)
         orderings (vec (map vec key-weights))]
     (loop [[[key weight] & xs] orderings
            remaining choice]
       (if (< remaining weight) key
                                (recur xs (- remaining weight)))))))


(defn- update-chain [order memo group]
  "update the count of the next state for the current state"
  (let [key (take order group)
        next-key (last group)
        current-state (get memo key {})
        current-value (get current-state next-key 0)]
    (assoc-in memo [key next-key] (inc current-value))))

(defn- extend-chain
  "process a single seq from the input"
  [order chain input]
  (let [suffixed (if (> order 0) (conj input nil) input)
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

(defn sample
  "use a multichain to generate sample output"
  ([order seed input-multichain]
   (loop [output []
          key seed]
     (let [input-chain (first (filter #(get % key) input-multichain)) ; account for unseen patterns
           next (weighted-choice (get input-chain key))
           new-output (conj output next)
           new-key (if (> order 0) (take-last order new-output) '())]
       (if (nil? next) output (recur new-output new-key)))))
  ([order input-chain] (sample order (prefix order) input-chain)))
