(ns advent-of-code.day19
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 19))

(def input (->> (str/split (str/join "\n" content) #"\n\n")
                (map #(str/split % #"\n"))
                (mapv #(mapv (fn [r] (mapv parse-long (str/split r #",")))
                             (drop 1 %)))))

(defn diff [[x1 y1 z1] [x2 y2 z2]] [(- x1 x2) (- y1 y2) (- z1  z2)])

(defn rotate4 [v]
  (take 4 (iterate (fn [[x y z]] [(- y) x z]) v)))

(defn rotations [[x y z]]
  (mapcat rotate4 [[x y z] [z y (- x)] [(- z) y x]
                   [x z (- y)] [x (- z) y] [x (- y) (- z)]]))

(defn all-rotations [overlay]
  (apply map vector (mapv rotations overlay)))

(defn match-scanner [s1 s2]
  (first
   (for [s2 (all-rotations s2)
         bs1 s1
         bs2 s2
         :let [off (diff bs2 bs1)
               beacons (into #{} (map #(diff % off)) s2)]
         :when (>= (count (set/intersection s1 beacons)) 12)]
     {:position off :beacons beacons})))

(defn solve [[scanner0 & scanners]]
  (prn (count scanners))
  (loop [result {:position [[0 0 0]]
                 :beacons (set scanner0)}
         rem-scanners (apply conj (clojure.lang.PersistentQueue/EMPTY)
                             (map #(into #{} %) scanners))]
    (if (empty? rem-scanners)
      result
      (let [{:keys [position beacons]} (match-scanner (:beacons result)
                                                      (peek rem-scanners))]
        (if beacons
          (recur {:beacons (set/union (:beacons result) beacons)
                  :position (conj (:position result) position)}
                 (pop rem-scanners))
          (recur result (conj (pop rem-scanners) (peek rem-scanners))))))))

(def result (solve input))

;; part 1
(-> result :beacons count)

;; part 2
(->> (for [p1 (:position result)
           p2 (:position result)]
       (map #(Math/abs (- %1 %2)) p1 p2))
     (map (partial reduce +))
     (sort >)
     (first))
