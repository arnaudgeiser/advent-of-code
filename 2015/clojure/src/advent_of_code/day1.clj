(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 1))

(def solution1
  (->> content
     (first)
     (map #(if (= \( %) 1 -1))
     (reduce +)))

(def solution2
  (->> content
       (first)
       (map-indexed (fn [idx itm] [idx (if (= \( itm) 1 -1)]))
       (reduce (fn [acc [idx itm]]
                 (let [s (+ acc itm)]
                   (if (= -1 s)
                     (reduced (inc idx))
                     s)))
               0)))
