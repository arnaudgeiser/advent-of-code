(ns advent-of-code.day6)

(def races [[46 347] [82 1522] [84 1406] [79 1471]])

(defn count-possible [race]
  (->> (map #(* % (- (first race) %)) (range (first race)))
       (filter #(> % (last race)))
       (count)))

(defn solution1 []
  (->> (map count-possible races)
       (reduce *)))

(def race [46828479 347152214061471])

(defn solution2 []
  (count-possible race))

(solution1) ;; 449550
(solution2) ;; 28360140
