(ns advent-of-code.day2
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 2))

(def mapping {["A" "X"] 4
              ["B" "Y"] 5
              ["C" "Z"] 6
              ["A" "Y"] 8
              ["A" "Z"] 3
              ["B" "X"] 1
              ["B" "Z"] 9
              ["C" "X"] 7
              ["C" "Y"] 2})

(def mapping2 {["A" "X"] 3
               ["B" "Y"] 5
               ["C" "Z"] 7
               ["A" "Y"] 4
               ["A" "Z"] 8
               ["B" "X"] 1
               ["B" "Z"] 9
               ["C" "X"] 2
               ["C" "Y"] 6})

(defn solve [mapping]
  (->> content
       (map #(str/split % #" "))
       (map mapping)
       (reduce +)))

(def solution1 (solve mapping))
(def solution2 (solve mapping2))
