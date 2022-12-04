(ns advent-of-code.day3
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 3))

(defn char->location [[x y] c]
  (case c
    \v [x (dec y)]
    \^ [x (inc y)]
    \< [(inc x) y]
    \> [(dec x) y]))

(defn deliver [moves]
  (reduce
   (fn [{:keys [location] :as acc} c]
     (let [new-location (char->location location c)]
       (-> acc
           (update :houses conj new-location)
           (assoc :location new-location))))
   {:location [0 0]
    :houses []}
   moves))

(def solution1
  (->> content
       first
       deliver
       :houses
       (count)))

(def solution2
  (->> content
        first
        (partition-all 2)
        (apply map vector)
        (mapcat (comp :houses deliver))
        (set)
        (count)))
