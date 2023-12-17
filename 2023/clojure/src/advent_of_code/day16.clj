(ns advent-of-code.day16
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 16))

(def mapping {:right [0 1]
              :left [0 -1]
              :top [-1 0]
              :bottom [1 0]})

(def mapping-slash {:right :top
                    :top :right
                    :bottom :left
                    :left :bottom})

(def mapping-backslash {:right :bottom
                        :bottom :right
                        :left :top
                        :top :left})

(defn continue [[x y] direction]
  (let [[x' y'] (mapping direction)]
    [(+ x x') (+ y y')]))

(defn solve [start-ray]
  (loop [visited {}
         rays [start-ray]]
    (if (empty? rays)
      (keys visited)
      (let [[[x y :as loc] direction] (first rays)
            val (get-in content loc)]
        (cond
          (= nil val)
          (recur visited (rest rays))

          (contains? (get visited loc) direction)
          (recur visited (rest rays))

          (= \. val)
          (recur
           (update visited loc (fnil conj #{}) direction)
           (conj (rest rays) [(continue loc direction) direction]))

          (= \| val)
          (if (#{:left :right} direction)
            (recur
             (update visited loc (fnil conj #{}) direction)
             (conj (rest rays) [[(inc x) y] :bottom] [[(dec x) y] :top]))
            (recur
             (update visited loc (fnil conj #{}) direction)
             (conj (rest rays) [(continue loc direction) direction])))

          (= \- val)
          (if (#{:bottom :top} direction)
            (recur
             (update visited loc (fnil conj #{}) direction)
             (conj (rest rays) [[x (dec y)] :left] [[x (inc y)] :right]))
            (recur
             (update visited loc (fnil conj #{}) direction)
             (conj (rest rays) [(continue loc direction) direction])))

          (= \/ val)
          (recur
           (update visited loc (fnil conj #{}) direction)
           (let [newdir (mapping-slash direction)
                 [x' y'] (mapping newdir)]
             (conj (rest rays) [[(+ x x') (+ y y')] newdir])))

          (= \\ val)
          (recur
           (update visited loc (fnil conj #{}) direction)
           (let [newdir (mapping-backslash direction)
                 [x' y'] (mapping newdir)]
             (conj (rest rays) [[(+ x x') (+ y y')] newdir]))))))))

(def possibilities
  (for [x (range (count content))
        y (range (count (first content)))
        d [:top :bottom :left :right]
        :when (or (zero? x) (zero? y) (= x (dec (count content))) (= y (dec (count content))))]
    [[x y] d]))

(defn solution1 []
  (count (solve [[0 0] :right])))

(defn solution2 []
  (->> (map solve possibilities)
       (map count)
       (apply max)))

(solution1) ;; 8551
(solution2) ;; 8754
