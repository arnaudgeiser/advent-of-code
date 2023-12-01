(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [raw-puzzle]]
            [clojure.string :as str]))

(def content (raw-puzzle 1))

(->> (str/split content #"\n\n")
     (map #(map parse-long (str/split % #"\n")))
     (map (partial reduce +))
     (reduce max))


(def solution1
  (->> (str/split content #"\n\n")
       (map #(map parse-long (str/split % #"\n")))
       (map (partial reduce +))
       (reduce max)))

(def solution2
  (->> (str/split content #"\n\n")
       (map #(map parse-long (str/split % #"\n")))
       (map (partial reduce +))
       (sort)
       (reverse)
       (take 3)
       (reduce +)))
