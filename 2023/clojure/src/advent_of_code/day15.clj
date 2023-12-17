(ns advent-of-code.day15
  (:require [advent-of-code.core :refer [raw-puzzle]]
            [clojure.string :as str]))

(def content (str/replace (raw-puzzle 15) "\n" ""))

(def content "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(def lines (str/split content #","))

(defn compute-hash [line]
  (reduce (fn [acc c]
            (let [i (+ acc (int c))
                  i (* 17 i)
                  i (mod i 256)]
              i)) 0 line))

(->> (map compute-hash lines)
     (reduce +))

(def res (reduce (fn [acc line]
                   (let [[label sign fl] (rest (re-find #"(.*)([=-])(.*)" line))
                         hash (compute-hash label)]
                     (condp = sign
                       "=" (update acc hash (fnil assoc {}) label (parse-long fl))
                       "-" (do
                             (update acc hash dissoc label)))))
                 {}
                 lines))

(->> (mapcat (fn [fp]
               (map-indexed (fn [slot [_ fl]] (* (inc fp) (inc slot) fl)) (get res fp)))  (range 256))
     (reduce +))
