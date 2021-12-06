(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 1))

(->> content
     (map #(Integer/parseInt %))
     (partition 3 1)
     (map (partial apply +))
     (partition 2 1)
     (filter (fn [[a b]] (> b a)))
     (count))
