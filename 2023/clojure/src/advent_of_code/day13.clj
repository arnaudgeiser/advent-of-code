(ns advent-of-code.day13
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 13))

#_(def content ["#.##..##."
                "..#.##.#."
                "##......#"
                "##......#"
                "..#.##.#."
                "..##..##."
                "#.#.##.#."
                ""
                "#...##..#"
                "#....#..#"
                "..##..###"
                "#####.##."
                "#####.##."
                "..##..###"
                "#....#..#"])

(defn parse []
  (reduce (fn [acc v]
            (let [cluster (last acc)]
              (if (seq v)
                (conj (vec (butlast acc)) (conj cluster v))
                (conj acc []))))
          [[]]
          content))

(defn split-point [mountain]
  (->> (filter (fn [i] (let [[top bottom] (split-at i mountain)
                             top (reverse top)
                             cnt (min (count top) (count bottom))
                             top (take cnt top)
                             bottom (take cnt bottom)]
                         (and (seq top) (seq bottom) (= top bottom))))
               (range (count mountain)))
       (first)))

(defn revert-mountain [mountain]
  (apply map str mountain))

(defn split-mountain [mountain]
  (or
   (split-point (revert-mountain mountain))
   (* (or (split-point mountain) 0) 100)))

(->> (parse)
     (map split-mountain)
     (reduce +))
