(ns advent-of-code.day8
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.math.numeric-tower :refer [lcm]]
            [clojure.string :as str]))

(def content (puzzle 8))
(def directions (cycle (first content)))

(def maps
  (->> (drop 2 content)
       (map #(re-find #"([1-9A-Z]{3}) = \(([1-9A-Z]{3}), ([1-9A-Z]{3})\)" %))
       (map (fn [[_ s l r]] [s [l r]]))
       (into {})))

(defn map-location [direction location]
  (if (= direction \L)
    (first (get maps location))
    (second (get maps location))))

(defn find-target [location]
  (reduce (fn [{:keys [location steps]} direction]
            (if (str/ends-with? location "Z")
              (reduced steps)
              {:location (map-location direction location)
               :steps (inc steps)}))
          {:location location :steps 0}
          directions))

(def starts
  (->> maps (map first)
       (filter #(str/ends-with? % "A"))))

(defn solution1 []
  (find-target "AAA"))

(defn solution2 []
  (reduce lcm (map find-target starts)))

(solution1) ;; 13771
(solution2) ;; 13129439557681
