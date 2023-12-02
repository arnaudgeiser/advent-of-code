(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 1))

(def mapping
  {"one" "1"
   "two"   "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn solution1 [content]
  (->> content
       (map (fn [line]
              (->> (re-seq #"\d" line)
                   (#(parse-long (str (first %) (last %)))))))
       (reduce +)))

(defn solution2 [content]
  (->> content
       (map (fn [line]
              (->> (re-seq (re-pattern (str "(?=(\\d|" (str/join "|" (keys mapping)) "))")) line)
                   (map second)
                   (map (fn [s] (get mapping s s)))
                   (#(parse-long (str (first %) (last %)))))))
       (reduce +)))

(solution1 content) ;; 55130
(solution2 content) ;; 54985
