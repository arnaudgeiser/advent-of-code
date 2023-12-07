(ns advent-of-code.day7
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 7))

(def order "AKQJT98765432")

(def content
  ["32T3K 765"
   "T55J5 684"
   "KK677 28"
   "KTJJT 220"
   "QQQJA 483"])

(def line (-> (str/split (first content) #" ")))

(def cards (first line))

(def freq (frequencies (sort cards)))

(defn value [hand]
  (let [cards (frequencies hand)
        v (vals cards)]
    (cond
     (some (partial = 5) v)
     60
     (some (partial = 4) v)
     50
     (and (some (partial = 3) v)
          (some (partial = 2) v))
     40
     (some (partial = 3) v)
     30
     (= 2 (count (filter (partial = 2) v)))
     20
     (= 1 (count (filter (partial = 2) v)))
     18
     :else 0)))

(defn compare-hand [f s]
  (prn "compare hand!" f s)
  (->> (map vector f s)
       (map (fn [[x y]] (- (str/index-of order x) (str/index-of order y))))
       (remove (partial = 0))
       (first)))

(defn compare-hand2 [f s]
  (if (zero? (- (value f) (value s)))
    (compare-hand f s)
    (- (value s)(value f))))

(->> (sort #(compare-hand2 (first %1) (first %2)) (map #(str/split % #" ") content))
     (reverse)
     (map-indexed (fn[i x] (* (inc i) (parse-long (last x)))))
     (reduce +))
