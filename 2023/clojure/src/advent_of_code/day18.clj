(ns advent-of-code.day18
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 18))

(defn parse-part1 [s]
  (let [[d nb] (str/split s #" ")
        nb (parse-long nb)]
    [d nb]))

(defn parse-part2 [s]
  (let [color (last (re-find #"\(#([0-9a-f]*)\)" s))
        d ({\0 "R" \1 "D" \2 "L" \3 "U"} (last color))
        nb (read-string (str "0x" (apply str (butlast color))))]
    [d nb]))

(defn parse-vertices [parse-fn]
  (reduce (fn [acc s]
            (let [[d nb] (parse-fn s)
                  [x y] (last acc)]
              (conj acc
                    (condp = d
                      "R" [(+ x nb) y]
                      "L" [(- x nb) y]
                      "D" [x (+ y nb)]
                      "U" [x (- y nb)]))))
          [[0 0]]
          content))

(defn shoelace [vertices]
  (/ (->> (partition 2 1 vertices)
          (map (fn [[[x1 y1] [x2 y2]]] (- (* x1 y2) (* x2 y1))))
          (reduce +))
     2))

(defn exterior-points [vertices]
  (reduce (fn [acc [[x1 y1] [x2 y2]]]
            (+ acc (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))) 0 (partition 2 1 vertices)))

(defn interior-points [area outside-points]
  (- (inc area) (/ outside-points 2)))

(defn solve [parse-fn]
  (let [vertices (doall (parse-vertices parse-fn))
        area (shoelace vertices)
        epoints (exterior-points vertices)]
    (+ epoints (interior-points area epoints))))

(solve parse-part1) ;; 39194
(solve parse-part2) ;; 78242031808225
