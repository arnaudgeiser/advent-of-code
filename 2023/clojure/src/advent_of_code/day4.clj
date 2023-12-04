(ns advent-of-code.day4
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def content (puzzle 4))

(def line (first content))

(defn parse-line [line]
  (-> line
      (str/split #": ")
      (second)
      (str/split #" \| ")
      (->> (map (fn [numbers] (set (keep #(parse-long (str/trim %)) (str/split numbers #" "))))))))

(defn matches [line] (apply set/intersection (parse-line line)))
(defn points [matches] (Math/pow 2 (dec (count matches))))

(defn solution1 []
  (->> content
       (map matches)
       (filter seq)
       (map points)
       (reduce +)))

(solution1) ;; 21558
