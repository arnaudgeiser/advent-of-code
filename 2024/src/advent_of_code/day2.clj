(ns advent-of-code.day2
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (map #(map parse-long (str/split % #" ")) (puzzle 2)))

(defn safe? [line]
  (and (or (apply < line)
           (apply > line))
       (every? (fn [[v1 v2]] (<= 1 (Math/abs (- v1 v2)) 3))
               (partition 2 1 line))))

(defn varients [line]
  (map #(vec (concat (take % line) (drop (inc %) line))) (range (count line))))

(def solution1 (count (filter safe? content)))
(def solution2 (count (filter #(some safe? (varients %)) content)))

(prn solution1) ;; 230
(prn solution2) ;; 301
