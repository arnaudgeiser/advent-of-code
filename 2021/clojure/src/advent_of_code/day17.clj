(ns advent-of-code.day17)

(def target [96 125 -144 -98])

(defn hit-target [[velx vely :as velocity]]
  (let [[min-targetx max-targetx min-targety max-targety] target]
    (loop [x 0
           y 0
           velx velx
           vely vely
           high 0]
      (cond
        (and (>= x min-targetx)
             (<= x max-targetx)
             (>= y min-targety)
             (<= y max-targety))
        [velocity high]

        (or (> x max-targetx)
            (< y min-targety))
        nil

        :else
        (recur (+ x velx)
               (+ y vely)
               (cond (pos-int? velx) (dec velx)
                     (neg-int? velx)  (inc velx)
                     :else 0)
               (dec vely)
               (max high (+ y vely)))))))

(defn matched-velocities []
  (for [x (range -200 200)
        y (range -200 200)
        :let [velocity [x y]
              hit (hit-target velocity)]
        :when hit]
    hit))

(defn solve1 []
  (->> (matched-velocities)
       (sort-by second)
       (reverse)
       (first)
       (last)))

(defn solve2 []
  (->> (matched-velocities)
       (count)))

(solve1)
(solve2)
