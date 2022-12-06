(ns advent-of-code.day6
  (:require [advent-of-code.core :refer [raw-puzzle]]))

(def content (raw-puzzle 6))

(defn solve [nb]
  (->> content
       (partition-all nb 1)
       (map (comp count set))
       (reduce (fn [acc cnt] (if (= cnt nb) (reduced acc) (inc acc))) nb)))

(def solution1 (solve 4))
(def solution2 (solve 14))
