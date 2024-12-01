(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 1))

(def sorted
  (->> (map #(str/split % #"   ") content)
       (apply map vector)
       (map (partial map parse-long))
       (map sort)))

(def solution1
  (->> sorted
       (apply map vector)
       (map (fn [[v1 v2]] (Math/abs (- v1 v2))))
       (reduce +)))

(def counted
  (reduce #(update %1 %2 (fnil inc 0)) {} (second sorted)))

(def solution2
  (->> (first sorted)
       (map #(* % (get counted % 0)))
       (reduce +)))

solution1 ;; 1660292
solution2 ;; 22776016
