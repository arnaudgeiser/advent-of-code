(ns advent-of-code.day12
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 12))

#_
(def content ["???.### 1,1,3"
              ".??..??...?##. 1,1,3"
              "?#?#?#?#?#?#?#? 1,3,1,6"
              "????.#...#... 4,1,1"
              "????.######..#####. 1,6,5"
              "?###???????? 3,2,1"])

(defn parse []
  (->> (map #(str/split % #" ") content)
       (mapv (fn [[line groups]] [line (mapv parse-long (str/split groups #","))]))))

(defn valid? [[line expected]]
  (let [cnt (->> (filter seq (re-seq #"\#*" line))
                 (mapv count))]
    (= cnt expected)))

(defn question-marks [tuple]
  (keep-indexed (fn [i x] (when (= x \?) i)) (first tuple)))

(defn generate-combinations [length]
  (if (= length 0)
    [""]
    (for [rest (generate-combinations (dec length))
          s ["#" "."]]
      (str s rest))))

(defn possibilities [tuple]
 (let [question-marks (question-marks tuple)]
   (->> (for [g (generate-combinations (count question-marks))]
          (do
            [(apply str
                    (reduce (fn [s [idx match]]
                              (assoc s match (nth g idx)))
                            (vec (first tuple)) (map-indexed (fn [i x] [i x]) question-marks)))
             (second tuple)]))
        (filter valid?)
        (count))))

(->> (map possibilities (parse))
     (reduce +))
