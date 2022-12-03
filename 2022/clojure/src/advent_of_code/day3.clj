(ns advent-of-code.day3
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]))

(def content (puzzle 3))

(defn char->int [c]
  (if (Character/isLowerCase c)
    (- (int c) 96)
    (- (int c) 38)))

(def solution1
  (->> content
       (map #(mapv set (split-at (/ (count %) 2) %)))
       (mapcat (partial apply set/intersection))
       (map char->int)
       (reduce +)))

(def solution2
  (->> content
        (map #(set (map char %)))
        (partition-all 3)
        (mapcat (partial apply set/intersection))
        (map char->int)
        (reduce +)))
