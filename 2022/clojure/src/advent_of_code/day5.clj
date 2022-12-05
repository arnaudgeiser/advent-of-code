(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 5))

(def stack
  (->> (take 8 content)
       (map #(partition-all 4 %))
       (mapcat (fn [line] (map #(nth % 1) line)))
       (map #(if (= \space %) nil %))
       (partition-all 9)
       (apply map vector)
       (mapv (comp reverse (partial filterv identity)))
       (into [])))

(defn parse-line [s]
  (let [[_ & tail] (re-find #"move ([0-9]*) from ([0-9]*) to ([0-9]*)" s)]
    (mapv parse-long tail)))

(defn process-command [append-fn stack [nb from to]]
  (let [from'                 (dec from)
        to'                   (dec to)
        from-stack            (nth stack from')
        to-stack              (nth stack to')
        [from-stack' to-move] (split-at (- (count from-stack) nb) from-stack)
        to-stack'             (into [] (concat to-stack (append-fn to-move)))]
    (-> stack
        (assoc from' (into [] from-stack'))
        (assoc to' (into [] to-stack')))))

(defn solve [commands init append-fn]
  (->> commands
       (map parse-line)
       (reduce (partial process-command append-fn) init)
       (map last)
       (str/join)))

(def solution1 (solve (drop 10 content) stack reverse))

(def solution2 (solve (drop 10 content) stack identity))
