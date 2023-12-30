(ns advent-of-code.day23
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 23))

(def start [0 1])
(def max-y (dec (count content)))
(def end [max-y (first (filter #(= \. (get-in content [max-y %])) (range (count (first content)))))])

(def deltas [[1 0] [-1 0] [0 1] [0 -1]])
(def delta-mapping {\. deltas \> [[0 1]] \< [[0 -1]] \v [[1 0]] \^ [-1 0]})

(defn neighbors [[y x]]
  (for [[y' x'] (delta-mapping (get-in content [y x]))
        :let [coord [(+ y y') (+ x x')]
              v (get-in content coord)]
        :when (#{\. \> \^ \v \<} v)]
    coord))

(defn paths-part1 [start-pos]
  (loop [[[seen pos] & rem :as stack] (list [#{} start-pos])
         result []]
    (cond
      (nil? seen)
      result

      (= pos end)
      (recur rem
             (conj result (count seen)))

      :else
      (recur (concat rem (map (fn [pos'] [(conj seen pos) pos']) (remove seen (neighbors pos))))
             result))))

(defn neighbors2 [[y x]]
  (for [[y' x'] deltas
        :let [coord [(+ y y') (+ x x')]
              v (get-in content coord)]
        :when (#{\. \> \^ \v \<} v)]
    coord))

;; All credits goes to : https://github.com/bhauman/adv2023/blob/main/src/adv2023/day23/sol.clj
(defn segment-positions-to-filter [segments]
  (->> segments
       (remove #(<= (count %) 4))
       (mapcat (comp (partial drop 2) (partial drop-last 2)))))

(defn stack-path-segments [start-pos]
  (loop [[[seen segment] & rs :as stack] (list [#{} (list start-pos)])
         segment-list []]
    (let [pos (first segment)]
      (cond
        (nil? seen) segment-list
        (= end pos) (recur rs (conj segment-list segment))
        :else
        (if-let [next (->> (neighbors2 pos)
                           (remove (into seen (segment-positions-to-filter segment-list)))
                           not-empty)]
          (let [next-seen (conj seen pos)]
            (if (= 1 (count next))
              (recur (conj rs [next-seen (cons (first next) segment)])
                     segment-list)
              (recur (into rs (map #(vector next-seen (list % pos)) next))
                     (conj segment-list segment))))
          (recur rs segment-list))))))

(def path-segments
    (->> (stack-path-segments start)
         (map (fn [path] [#{(last path) (first path)} path]))))

(def segment-graph
  (->> path-segments
       (reduce (fn [acc [nodes rs]]
                 (let [[node-1 node-2] (seq nodes)
                       len (dec (count rs))]
                   (-> acc
                       (update node-1 conj [node-2 len])
                       (update node-2 conj [node-1 len]))))
               {})))

(defn longest-path-length [graph seen pos]
  (if (= pos end)
    0
    (some->> (get graph pos)
             (remove (comp seen first))
             not-empty
             (keep #(when-let [res (longest-path-length
                                    graph
                                    (conj seen pos) (first %))]
                      (+ res (second %))))
             not-empty
             (reduce max 0))))

(defn solution1 []
  (apply max (paths-part1 start)))

(defn solution2 []
  (longest-path-length segment-graph #{} start))


(solution1) ;; 94
(solution2) ;; 6298
