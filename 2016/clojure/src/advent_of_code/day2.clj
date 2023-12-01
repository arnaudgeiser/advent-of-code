(ns advent-of-code.day2
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 2))

(def mapping {\L [0 -1]
              \R [0 1]
              \U [1 0]
              \D [-1 0]})

(map min [3 0] [2 2])

(def solution1
  (->> content
       (mapcat #(str/split % #"x"))
       (map parse-long)
       (partition-all 3)
       (map (fn [[l w h :as s]] (+ (* 2 l w) (* 2 w h) (* 2 h l) (apply * (take 2 (sort s))))))
       (reduce +)))

(def solution2
  (->> content
       (mapcat #(str/split % #"x"))
       (map parse-long)
       (partition-all 3)
       (map (fn [[l w h :as s]]
              (let [[l1 l2] (sort s)]
                (+ (+ l1 l1 l2 l2) (* l w h)))))
       (reduce +)))
