(ns markov-chain.test-utils
  (:require [clojure.string :as string]))

(def babel-vec [(string/split "babel" #"")])

(defmacro w% [& words]
  (mapv str words))

(def lorem-ipsum-raw (w% Lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua Ut enim ad minim veniam quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur Excepteur sint occaecat cupidatat non proident sunt in culpa qui officia deserunt mollit anim id est laborum))

(def lorem-ipsum
  (into #{} (map string/lower-case lorem-ipsum-raw)))
