(ns advent-of-code.day2
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 2))

(def bag {:red   12
          :green 13
          :blue  14})

(defn parse-game [game]
  (-> game
      (str/split #"[:;]")
      (rest)
      (->> (map (fn [round]
                  (->> (str/split round #",")
                       (map str/trim)
                       (map (fn [l] (str/split l #" ")))
                       (map (fn [[n color]] [(keyword color) (parse-long n)]))
                       (into {})))))))

(defn possible-game? [game]
  (every? (fn [rounds]
            (every? (fn [[color n]]
                      (<= n (get bag color))) rounds))
          (parse-game game)))

(defn solution1 []
  (->> (map-indexed (fn [i game] (if (possible-game? game) (inc i) 0)) content)
       (reduce +)))

(defn solution2 []
  (->> (map parse-game content)
       (map (partial apply merge-with max))
       (map vals)
       (map (partial reduce *))
       (reduce +)))

(solution1) ;; 2617
(solution2) ;; 59795
