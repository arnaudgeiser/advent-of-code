(ns advent-of-code.day7
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 7))

(def order "AKQT98765432J")

(def line (-> (str/split (first content) #" ")))

(defn hand-value [hand]
  (let [cards (dissoc (frequencies hand) \J)
        jokers (count (filter (partial = \J) hand))
        v (if (seq cards) (update (vec (sort (vals cards))) (dec (count cards)) + jokers)
              [5])]
    (cond
      (some (partial = 5) v) 6
      (some (partial = 4) v) 5
      (and (some (partial = 3) v)
           (some (partial = 2) v)) 4
      (some (partial = 3) v) 3
      (= 2 (count (filter (partial = 2) v))) 2
      (= 1 (count (filter (partial = 2) v))) 1
      :else 0)))

(defn compare-cards [f s]
  (->> (map vector f s)
       (map (fn [[x y]] (- (str/index-of order x) (str/index-of order y))))
       (remove (partial = 0))
       (first)))

(defn compare-hands [f s]
  (if (zero? (- (hand-value f) (hand-value s)))
    (compare-cards f s)
    (- (hand-value s) (hand-value f))))

(->> (sort #(compare-hands (first %1) (first %2)) (map #(str/split % #" ") content))
     (reverse)
     (map-indexed (fn[i x] (* (inc i) (parse-long (last x)))))
     (reduce +))
