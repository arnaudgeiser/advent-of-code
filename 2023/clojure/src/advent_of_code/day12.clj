(ns advent-of-code.day12
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 12))

(defn parse []
  (->> (map #(str/split % #" ") content)
       (mapv (fn [[line groups]] [line (mapv parse-long (str/split groups #","))]))))

(defn valid-suffixes [row number]
  (for [i (range (inc (- (count row) number)))
        :while (every? #{\. \?} (take i row))
        :when (every? #{\# \?} (take number (drop i row)))
        :when (#{\. \?} (nth row (+ i number) \.))]
    (drop (+ i number 1) row)))

(def acs
  (memoize
   (fn
     [row numbers]
     (if-let [[n & nrs] (seq numbers)]
       (reduce + (for [s (valid-suffixes row n)] (acs s nrs)))
       (if (every? #{\. \?} row) 1 0)))))

(defn *5 [[line numbers]]
  [(str/join "?" (repeat 5 line))
   (mapcat identity (repeat 5 numbers))])

(defn solution1 []
  (->> (map (partial apply acs) (parse))
       (reduce +)))

(defn solution2 []
  (->> (parse)
       (map *5)
       (map (partial apply acs))
       (reduce +)))

(solution1) ;; 6981
(solution2) ;; 4546215031609
