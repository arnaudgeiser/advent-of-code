(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 1))

(def mapping
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})

(defn parse-line1 [line]
  (re-seq #"\d" line))

(defn parse-line2 [line]
  (->> (re-seq (re-pattern (str "(?=(\\d|" (str/join "|" (keys mapping)) "))")) line)
       (map second)
       (map (fn [s] (get mapping s s)))))

(defn calibration [line]
  (parse-long (str (first line) (last line))))

(defn solve [content parse-fn]
  (->> content
       (map (comp calibration parse-fn))
       (reduce +)))

(solve content parse-line1) ;; 55130
(solve content parse-line2) ;; 54985
