(ns advent-of-code.day8
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 8))

#_
(def content
  ["RL"
   ""
  "AAA = (BBB, CCC)"
  "BBB = (DDD, EEE)"
  "CCC = (ZZZ, GGG)"
  "DDD = (DDD, DDD)"
  "EEE = (EEE, EEE)"
  "GGG = (GGG, GGG)"
  "ZZZ = (ZZZ, ZZZ)"])

(def directions (cycle (first content)))

(def maps
  (->> (drop 2 content)
       (map #(re-find #"([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)" %))
       (map (fn [[_ s l r]] [s [l r]]))
       (into {})))

(reduce (fn [{:keys [location steps]} direction]
          (if (= "ZZZ" location)
            (reduced steps)
            {:location (if (= direction \L)
                         (first (get maps location))
                         (second (get maps location)))
             :steps (inc steps)}))
        {:location "AAA" :steps 0}
        directions)
