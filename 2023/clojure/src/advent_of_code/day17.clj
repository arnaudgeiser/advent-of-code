(ns advent-of-code.day17
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.data.priority-map :refer [priority-map]]))

(def content (puzzle 17))

(def max-x (dec (count content)))
(def max-y (dec (count (first content))))
(def goal [max-y max-x])

(defn solve [grid minval maxval]
  (loop [unvisited (priority-map [0 0 0] 0 [0 0 1] 0)
         seen #{}]
    (let [[[y x direction] cost] (peek unvisited)]
      (cond
        (= [y x] goal)
        cost

        (seen [y x direction])
        (recur (pop unvisited) seen)

        :else
        (let [{:keys [unvisited seen]}
              (reduce (fn [acc s]
                        (reduce (fn [{:keys [unvisited seen cost] :as acc} i]
                                  (let [new-x (if (= 1 direction) (+ x (* i s)) x)
                                        new-y (if (= 0 direction) (+ y (* i s)) y)]
                                    (if (or (neg? new-x) (neg? new-y) (> new-x max-x) (> new-y max-y))
                                      acc
                                      (let [ncost (parse-long (str (get-in grid [new-y new-x])))
                                            cost' (+ cost ncost)]
                                        (if (or (seen [new-y new-x (- 1 direction)])
                                                (< i minval))
                                          (merge acc {:cost cost'})
                                          (let [ccost (or (unvisited [new-y new-x (- 1 direction)]) Integer/MAX_VALUE)]
                                            {:unvisited (if (< ccost cost') unvisited (assoc unvisited [new-y new-x (- 1 direction)] cost'))
                                             :seen seen
                                             :cost cost'}))))))
                                (merge acc {:cost cost})
                                (range 1 (inc maxval))))
                      {:seen (conj seen [y x direction])
                       :unvisited (pop unvisited)} [-1 1])]
          (recur unvisited seen))))))

(solve content 1 3) ;; 686
(solve content 4 10) ;; 801
