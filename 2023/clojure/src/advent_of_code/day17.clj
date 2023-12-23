(ns advent-of-code.day17
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.data.priority-map :refer [priority-map]]))

(def content (puzzle 17))

#_(def content
    ["2413432311323"
     "3215453535623"
     "3255245654254"
     "3446585845452"
     "4546657867536"
     "1438598798454"
     "4457876987766"
     "3637877979653"
     "4654967986887"
     "4564679986453"
     "1224686865563"
     "2546548887735"
     "4322674655533"])

(def start [0 0])

(def height (count content))
(def width (count (first content)))

;;def navigate(grid, minval=1, maxval=3):
;;q = PriorityQueue()
;;    max_y, max_x = (v - 1 for v in grid.shape)
;;    goal = max_y, max_x
;;    q.put((0, (0, 0, 0)))
;;    q.put((0, (0, 0, 1)))
;;    seen = set()
;;
;;    while q:
;;        cost, (y, x, direction) = q.get()
;;        if (y, x) == goal:
;;            break
;;        if (y, x, direction) in seen:
;;            continue
;;        seen.add((y, x, direction))
;;        original_cost = cost
;;        for s in [-1, 1]:
;;            cost = original_cost
;;            new_y, new_x = y, x
;;            for i in range(1, maxval + 1):
;;                if direction == 1:
;;                new_x = x + i * s
;;                else:
;;                new_y = y + i * s
;;                if new_x < 0 or new_y < 0 or new_x > max_x or new_y > max_y:
;;                break
;;                cost += grid[new_y, new_x]
;;                if ((new_y, new_x, 1 - direction)) in seen:
;;                    continue]
;;                if i >= minval:
;;                    q.put((cost, (new_y, new_x, 1 - direction)))]
;;    return cost)

(defn navigate [grid minval maxval]
  (let [[max-y max-x] [(dec height) (dec width)]
        goal [max-y max-x]]
    (loop [unvisited (priority-map [0 0 0] 0 [0 0 1] 0)
           seen #{}]
      (let [[[y x direction] cost] (peek unvisited)]
        (cond
          (= [y x] goal)
          cost

          (seen [y x direction])
          (recur (pop unvisited) seen)

          :else
          (do
            (let [{:keys [unvisited seen]}
                  (reduce (fn [acc s]
                            (reduce (fn [{:keys [unvisited seen cost] :as acc} i]
                                      (let [new-x (if (= 1 direction) (+ x (* i s)) x)
                                            new-y (if (= 0 direction) (+ y (* i s)) y)]
                                        (if (or (neg? new-x) (neg? new-y) (> new-x max-x) (> new-y max-y))
                                          acc
                                          (let [ncost (parse-long (str (get-in grid [new-y new-x])))
                                                cost' (+ cost ncost)]
                                            (if (seen [new-y new-x (- 1 direction)])
                                              (merge acc {:cost cost'})
                                              (let [ccost (or (unvisited [new-y new-x (- 1 direction)]) Integer/MAX_VALUE)]
                                                {:unvisited (if (< ccost cost') unvisited (assoc unvisited [new-y new-x (- 1 direction)] cost'))
                                                 :seen seen
                                                 :cost cost'}))))))
                                    (merge acc {:cost cost})
                                    (range 1 (inc maxval))))
                          {:seen (conj seen [y x direction])
                           :unvisited (pop unvisited)} [-1 1])]
              (recur unvisited seen))))))))

(navigate content 1 3)

#_(defn adjacents [[x1 y1]]
    (->> [[:right [0 1]] [:left [0 -1]] [:bottom [1 0]] [:top [-1 0]]]
         (map (fn [[direction [x2 y2]]] [direction [(+ x1 x2) (+ y1 y2)]]))
         (remove (fn [[_ [x y]]] (or (neg-int? x) (neg-int? y) (>= x height) (>= y width))))))

#_(def res
    (loop [unsettled {start [0 (Node. start nil nil 0)]}
           visited {}
           found nil]
      (let [[position [cost node]] (first (sort-by (comp first val) unsettled))
            adjacents (remove (fn [pos] (contains? visited pos)) (adjacents position node visited))]
        (if (and (not found) (seq unsettled))
          (recur (reduce (fn [acc neighbor]
                           (let [neigh-loc (:coord neighbor)
                                 direction (:direction neighbor)
                                 blocks (:blocks neighbor)
                                 [icost] (get acc neigh-loc)
                                 ncost (parse-long (str (get-in content neigh-loc)))]
                             (if (< (+ cost ncost) (or icost Integer/MAX_VALUE))
                               (assoc acc neigh-loc [(+ cost ncost) (Node. neigh-loc node direction blocks)])
                               acc)))
                         (dissoc unsettled position)
                         adjacents)
                 (assoc visited position cost)
                 (when (= [12 12] position) node))
          found))))

#_(defn move [[x y] direction]
    (condp = direction
      :left [x (dec y)]
      :right [x (inc y)]
      :top [(dec x) y]
      :bottom [(inc x) y]))

#_(def res
    (loop [unsettled {0 [start nil]}
           visited {}
           found nil]
      (let [[cost [position :as node] :as p] (first (sort-by key unsettled))
            _ (prn p)
            adjacents (remove (fn [pos] (contains? visited pos)) (adjacents position))]
        (if (and (not found) (seq unsettled))
          (recur (reduce (fn [acc [neigh-direction coord]]
                           (:acc (reduce
                                  (fn [{:keys [prev-node coord cost acc]} _]
                                    (if (or (neg? (first coord)) (neg? (second coord)) (>= (first coord) 13) (>= (second coord) 13))
                                      (reduced {:acc acc})
                                      (let [[icost] (get acc coord [Integer/MAX_VALUE])
                                            ncost (parse-long (str (get-in content coord)))
                                            cost' (+ cost ncost)]

                                        {:acc (if (< cost' icost)
                                                (assoc acc cost' [coord neigh-direction prev-node])
                                                acc)
                                         :cost cost'
                                         :prev-coord coord
                                         :coord (move coord neigh-direction)})))
                                  {:acc acc
                                   :cost cost
                                   :prev-node node
                                   :coord coord}
                                  (range 3))))
                         (dissoc unsettled position)
                         adjacents)
                 (assoc visited position cost)
                 (when (= [12 12] position) node))
          found))))

#_(def c [(dec (count content)) (dec (count (first content)))])

#_(loop [node res
         path []]
    (if-let [{:keys [coord direction blocks]} node]
      (recur (:prev node) (conj path [coord direction blocks]))
      (doseq [s (reverse path)]
        (prn s))))

#_(:cost res)

#_(loop [path [c]]
    (Thread/sleep 300)
    (prn path)
    (let [[_ n] (get res (last path))]
      (if-let [p (:coord (:prev n))]
        (recur (conj path p))
        path)))
