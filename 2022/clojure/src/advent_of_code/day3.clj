(ns advent-of-code.day3
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]))

(def content (puzzle 3))

(defn char->int [c]
  (- (int c)
     (if (Character/isLowerCase c) 96 38)))

(def solution1
  (->> content
       (map #(map set (split-at (/ (count %) 2) (set %))))
       (mapcat (partial apply set/intersection))
       (map char->int)
       (reduce +)))

(def solution2
  (->> content
       (map set)
       (partition-all 3)
       (mapcat (partial apply set/intersection))
       (map char->int)
       (reduce +)))
