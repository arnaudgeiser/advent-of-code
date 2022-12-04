(ns advent-of-code.day4
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def content (puzzle 4))

(defn solve [pred]
  (->> content
       (mapcat #(str/split % #","))
       (mapcat #(str/split % #"-"))
       (map parse-long)
       (partition-all 2)
       (map (fn [[x y]] (range x (inc y))))
       (map set)
       (partition-all 2)
       (filter pred)
       (count)))

(def solution1
  (solve
   (fn [[l r]]
     (or (empty? (set/difference l r))
         (empty? (set/difference r l))))))

(def solution2
  (solve
   (fn [[l r]]
     (or (seq (set/intersection l r))
         (seq (set/intersection r l))))))
