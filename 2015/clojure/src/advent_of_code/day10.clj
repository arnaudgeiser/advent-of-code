(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]))

(def content (puzzle 10))

(def score-mapping {\) 3 \] 57 \} 1197 \> 25137})
(def matching {\( \) \[ \] \{ \} \< \>})
(def openers (set (keys matching)))
(def invert-matching (set/map-invert matching))
(def score-closing-mapping {\) 1 \] 2 \} 3 \> 4})

(defn corrupted-character [line]
  (reduce (fn [acc delimiter]
            (if (contains? openers delimiter)
              (conj acc delimiter)
              (if (not= (get matching (peek acc)) delimiter)
                (reduced delimiter)
                (pop acc))))
          []
          line))

(defn solve1 []
  (->> content
       (map corrupted-character)
       (remove vector?)
       (filter some?)
       (map score-mapping)
       (reduce +)))

(solve1)

(defn solve2 []
  (let [result (->> content
                    (map corrupted-character)
                    (filter vector?)
                    (map (fn [coll]
                           (->> (reverse coll)
                                (map (comp score-closing-mapping matching ))
                                (reduce #(+ %2 (* 5 %1)) 0))))
                    (sort))]
    (nth result (/ (count result) 2))))

(solve2)
