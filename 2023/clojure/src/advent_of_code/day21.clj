(ns advent-of-code.day21
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (mapv vec (puzzle 21)))
(def start [(/ (dec (count content)) 2) (/ (dec (count (first content))) 2)])
(def deltas [[0 1] [0 -1] [1 0] [-1 0]])

(defn garden-plots [point]
  (for [d deltas
        :let [point' (mapv + d point)]
        :when (#{\. \S} (get-in content point'))]
    point'))

(defn solution1 []
  (-> (iterate #(set (mapcat garden-plots %)) [start])
      (nth 64)
      (count)))

;; Totally "adapted" (stolen) from Jan Szejko:
;; https://github.com/janek37/advent-of-code/blob/main/2023/day21.py

;; Create a graph from `matrix` that maps each plot square to the neighbors it
;; can reach. This keeps us from re-calculating this every time we visit a
;; given square.

(defn- to-graph [matrix]
  (let [max-y  (count matrix)
        max-x  (count (first matrix))]
    (into {} (for [y (range max-y), x (range max-x)
                   :let [pos [y x]]
                   :when (= \. (get-in matrix pos))
                   :let [neighbors (garden-plots pos)]]
               (hash-map pos neighbors)))))

;; Update the positions set passed in with new points. This is the part of the
;; original Python that I understand the least.
(defn- update-positions [graph width height positions]
  (loop [[[pos y x] & positions] positions, pos-set #{}]
    (if (nil? pos)
      pos-set
      (let [updates (map #(vector % y x) (graph pos))
            px1     (when (zero? (last pos))
                      [[(first pos) (dec width)] y (dec x)])
            px2     (when (= (dec width) (last pos))
                      [[(first pos) 0] y (inc x)])
            py1     (when (zero? (first pos))
                      [[(dec height) (last pos)] (dec y) x])
            py2     (when (= (dec height) (first pos))
                      [[0 (last pos)] (inc y) x])
            corners (filter (comp not nil?) (list px1 px2 py1 py2))]
        (recur positions (into pos-set (concat updates corners)))))))

;; Get the possible destinations over the "infinite" grid, through `steps`
;; total steps. `steps` here is a modulo of the total steps that reduces the
;; necessary computation.
(defn- get-dests-inf [graph width height start steps]
  (loop [i 0, positions #{start}]
    (if (= i steps)
      positions
      (recur (inc i) (update-positions graph width height positions)))))

;; Count the occurrences of grid-positions relative to the "main" grid.
(defn- get-positions-by-grid [positions]
  (reduce (fn [counts [_ y x]]
            (assoc counts [y x] (inc (get counts [y x] 0))))
          {} positions))

;; Do the multi-step calculation of the final answer, based on the `counts`
;; map created in the previous fn.
(defn- calculate [counts num]
  (let [tip     (apply + (map counts [[0 -2] [0 2] [-2 0] [2 0]]))
        edge1   (apply + (map counts [[-1 -2] [1 -2] [-1 2] [1 2]]))
        edge2   (apply + (map counts [[-1 -1] [1 -1] [-1 1] [1 1]]))
        center1 (counts [1 0])
        center2 (counts [0 0])]
    (+ tip
       (* edge1 num)
       (* edge2 (dec num))
       (* center1 num num)
       (* center2 (dec num) (dec num)))))

(defn- find-start [matrix]
  (let [start (first (for [y (range (count matrix))
                           x (range (count (first matrix)))
                           :when (= \S (get-in matrix [y x]))]
                       [y x]))]
    (list start (assoc-in matrix start \.))))

;; Find the total number of reachable plots in the "infinite" grid, for `steps`
;; total steps.
(defn- find-plots-inf [steps [start matrix]]
  (let [graph     (to-graph matrix)
        inf-start [start 0 0]
        height    (count matrix)
        width     (count (first matrix))]
    (-> graph
        (get-dests-inf width height inf-start (+ (mod steps width)
                                                 (* width 2)))
        get-positions-by-grid
        (calculate (quot steps width)))))

(defn solution2 []
  (find-plots-inf 26501365 (find-start content)))

(solution1) ;; 3722
(solution2) ;; 614864614526014
