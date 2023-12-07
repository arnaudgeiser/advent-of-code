(ns advent-of-code.day7
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 7))

(defn hand-value [hand]
  (cond
    (some (partial = 5) hand) 6
    (some (partial = 4) hand) 5
    (and (some (partial = 3) hand)
         (some (partial = 2) hand)) 4
    (some (partial = 3) hand) 3
    (= 2 (count (filter (partial = 2) hand))) 2
    (= 1 (count (filter (partial = 2) hand))) 1
    :else 0))

(defn compare-cards [f s order]
  (->> (map vector f s)
       (map (fn [[x y]] (- (str/index-of order x) (str/index-of order y))))
       (remove (partial = 0))
       (first)))

(defn compare-hands [f s order hand-fn]
  (let [f' (hand-fn f)
        s' (hand-fn s)
        diff (- (hand-value s') (hand-value f'))]
   (if (zero? diff)
     (compare-cards f s order)
     diff)))

(defn hand-frequencies [hand]
  (vals (frequencies hand)))

(defn hand-frequencies2 [hand]
  (let [cards (dissoc (frequencies hand) \J)
        jokers (count (filter (partial = \J) hand))]
    (if (seq cards)
      (update (vec (sort (vals cards))) (dec (count cards)) + jokers)
      [5])))

(defn solve [order hand-fn]
 (->> (sort #(compare-hands (first %1) (first %2) order hand-fn) (map #(str/split % #" ") content))
      (reverse)
      (map-indexed (fn [i x] (* (inc i) (parse-long (last x)))))
      (reduce +)))

(solve "AKQJT98765432" hand-frequencies) ;; 253954294
(solve "AKQT98765432J" hand-frequencies2) ;; 254837398
