(ns advent-of-code.day22
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 22))

(def content
  ["1,0,1~1,2,1"
   "0,0,2~2,0,2"
   "0,2,3~2,2,3"
   "0,0,4~0,2,4"
   "2,0,5~2,2,5"
   "0,1,6~2,1,6"
   "1,1,8~1,1,9"])

#_#_#_#_(def content
          ["0,0,1~0,0,1"
           "0,0,3~0,0,3"])

      (def content
        ["0,0,1~0,0,10"
         "0,1,1~0,1,10"
         "0,0,300~0,1,300"])

    (def content
      ["0,0,1~0,0,10"
       "0,0,300~0,1,300"
       "1,0,50~1,0,50"])

  (def content
    ["0,0,1~0,0,10"
     "0,0,300~0,1,300"])

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
                 (<= y1' y2 y2'))
             (or (<= z1 z1' z2)
                 (<= z1 z2' z2)
                 (<= z1' z1 z2')
                 (<= z1' z2 z2'))))
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

(def cnt (count content))

(defn fall-bricks [brick]
  (let [supported ((:supports res) brick)]
    (filter #(= (count ((:supported-by res) %)) 1) supported)))

(defn fall? [brick]
  (prn brick ((:supported-by res) brick))
  (= (count ((:supported-by res) brick)) 1))

(->> (mapcat second (:world res))
     (filter (fn [brick] (every? #(> (count ((:supported-by res) %)) 1) ((:supports res) brick))))
     (count))

(defn count-falls [brick]
  (let [fall-bricks (fall-bricks brick)]
    (if (seq fall-bricks)
      (loop [queue fall-bricks
             bricks (set fall-bricks)]
        (if (seq queue)
          (let [supported ((:supports res) (first queue))]
            (recur (concat (rest queue) supported)
                   (set (concat bricks supported))))
          (count bricks)))
      0)))

#_(->> (mapcat val res)
       (map second)
       (filter identity)
       (count))

(->> (mapcat second (:world res))
     (map count-falls)
     #_
     (reduce +))
