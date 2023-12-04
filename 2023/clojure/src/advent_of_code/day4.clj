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

(defn accumulate-cards [cards [card-number nb-matches]]
  (let [copies (get cards card-number)]
    (reduce (fn [cards' card-number']
              (update cards' card-number' + copies))
            cards
            (range (inc card-number) (+ card-number (inc nb-matches))))))

(defn solution2 []
  (->> content
       (map (comp count matches))
       (map-indexed (fn [i x] [(inc i) x]))
       (reduce accumulate-cards (into {} (for [x (range (count content))] {(inc x) 1})))
       (vals)
       (reduce +)))

(solution1) ;; 21558
(solution2) ;; 10425665
