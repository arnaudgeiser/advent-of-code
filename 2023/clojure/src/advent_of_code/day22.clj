(ns advent-of-code.day22
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 22))

(defn parse-block [line]
  (mapv #(mapv parse-long (str/split % #",")) (str/split line #"~")))

(defn collisions [[[x1 y1 z1] [x2 y2 z2]] bricks]
  (filter (fn [[[x1' y1' z1'] [x2' y2' z2']]]
            (and
             (or (<= x1 x1' x2)
                 (<= x1 x2' x2)
                 (<= x1' x1 x2')
                 (<= x1' x2 x2'))
             (or (<= y1 y1' y2)
                 (<= y1 y2' y2)
                 (<= y1' y1 y2')
                 (<= y1' y2 y2'))))
          bricks))

(def res
  (reduce (fn [{:keys [world supported-by supports]} [[x1 y1 z1] [x2 y2 z2] :as brick]]
            (loop [z' z1]
              (let [top-z (- z2 (- z1 z'))
                    bricks (get world (dec z'))
                    collisions (collisions [[x1 y1 (dec z')] [x2 y2 (dec top-z)]] bricks)
                    moved [[x1 y1 z'] [x2 y2 top-z]]]
                (if (and (not= z' 1) (empty? collisions))
                  (recur (dec z'))
                  {:world (update world top-z (fnil conj []) moved)
                   :supported-by (reduce (fn [acc col] (update acc moved (fnil conj []) col)) supported-by collisions)
                   :supports (reduce (fn [acc col] (update acc col (fnil conj []) moved)) supports collisions)}))))
          {:world {}
           :supports {}
           :supported-by {}}
          (sort-by (fn [[[] [_ _ z]]] z) (mapv parse-block content))))

(defn fall? [bricks brick]
  (every? (fn [b] (bricks b)) ((:supported-by res) brick)))

(defn count-falls [brick]
  (loop [queue [brick]
         bricks #{brick}]
    (if (seq queue)
      (let [supported (filter (partial fall? bricks) ((:supports res) (first queue)))]
        (recur (concat (rest queue) (remove bricks supported))
               (set (concat bricks supported))))
      (dec (count bricks)))))

(def bricks (mapcat second (:world res)))

(defn solution1 []
 (->> bricks
      (filter (fn [brick] (every? #(> (count ((:supported-by res) %)) 1) ((:supports res) brick))))
      (count)))

(defn solution2 []
 (->> bricks
      (map count-falls)
      (reduce +)))

(solution1) ;; 443
(solution2) ;; 69915
