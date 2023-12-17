(ns advent-of-code.day15
  (:require [advent-of-code.core :refer [raw-puzzle]]
            [clojure.string :as str]))

(def content (str/split (str/replace (raw-puzzle 15) "\n" "") #","))

(defn compute-hash [line]
  (reduce (fn [acc c] (mod (* 17 (+ acc (int c))) 256)) 0 line))

(defn solution1 [] (reduce + (map compute-hash content)))

(defn boxes [content]
  (reduce (fn [acc line]
            (let [[label sign fl] (rest (re-find #"(.*)([=-])(.*)" line))
                  hash (compute-hash label)]
              (condp = sign
                "=" (update acc hash (fnil assoc {}) label (parse-long fl))
                "-" (update acc hash dissoc label))))
          {}
          content))

(defn focusing-power [boxes box-nb]
  (map-indexed (fn [slot [_ fl]] (* (inc box-nb) (inc slot) fl))
               (get boxes box-nb)))

(defn solution2 []
  (let [boxes (boxes content)]
    (->> (mapcat (partial focusing-power boxes) (range 256))
         (reduce +))))

(solution1) ;; 515495
(solution2) ;; 229349
