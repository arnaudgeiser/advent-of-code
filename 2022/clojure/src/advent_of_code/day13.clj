(ns advent-of-code.day13
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (->> (puzzle 13) (filter seq) (map read-string)))

(defn compare-pair [pair1 pair2]
  (cond
    (and (int? pair1) (int? pair2)) (- pair2 pair1)
    (and (coll? pair1) (int? pair2)) (compare-pair pair1 [pair2])
    (and (int? pair1) (coll? pair2)) (compare-pair [pair1] pair2)
    :else (let [res (->> (map vector pair1 pair2) (map (partial apply compare-pair)) (filter (complement zero?)) (first))]
            (if (some? res) res (- (count pair2) (count pair1))))))

(def dividers #{[[2]][[6]]})

(def solution1
  (->> (partition 2 content)
       (mapv (partial apply compare-pair))
       (keep-indexed (fn [i x] (when (pos-int? x) (inc i))))
       (reduce +)))

(def solution2
  (->> content
       (concat dividers)
       (sort compare-pair)
       (reverse)
       (keep-indexed (fn [i x] (when (dividers x) (inc i))))
       (reduce *)))
