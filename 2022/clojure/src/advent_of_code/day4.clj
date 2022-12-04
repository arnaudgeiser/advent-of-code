(ns advent-of-code.day4
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 4))

(defn solve [pred]
  (->> content
       (mapcat #(str/split % #","))
       (mapcat #(str/split % #"-"))
       (map parse-long)
       (partition-all 4)
       (filter pred)
       (count)))

(def solution1
  (solve
   (fn [[x1 y1 x2 y2]]
     (or (and (<= x2 x1 y2)
              (<= x2 y1 y2))
         (and (<= x1 x2 y1)
              (<= x1 y2 y1))))))

(def solution2
  (solve
   (fn [[x1 y1 x2 y2]]
     (or (<= x2 y1 y2)
         (<= x1 y2 y1)))))
