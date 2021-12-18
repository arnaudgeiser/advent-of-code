(ns advent-of-code.day18
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (->> (puzzle 18)
                  (map read-string)))

(defn land-left-location [coll path]
  (let [index (last path)
        path' path]
    (cond
      (empty? path)
      nil
      (> index 0)
      (loop [path' (conj (pop path') 0)]
        (if (coll? (get-in coll path'))
          (recur (conj path' 1))
          path'))
      :else
      (recur coll (pop path')))))

(defn land-right-location [coll path]
  (let [path' path]
    (cond
      (empty? path)
      nil
      (zero? (last path'))
      (loop [path' (conj (pop path') 1)]
        (if (coll? (get-in coll path'))
          (recur (conj path' 0))
          path'))
      :else
      (recur coll (pop path')))))

(def sample3 [[6,[5,[4,[3,2]]]],[2 3]])
(land-right-location sample3 [0 1 1 1])

(defn explode? [tuple path]
  (and (= (count path) 3) (coll? tuple)))

(defn split? [nb]
  (and (int? nb) (> nb 10)))

(defn process2 [coll [left right] path]
  (cond
    (explode? left path)
    (let [left-location (land-left-location coll path)]
      (cond-> (update-in coll path (fn [[[_ v2] v]]
                                     (if (coll? v)
                                       [0 [(+ v2 (first v)) (second v)]]
                                       [0 (+ v v2)])))
        left-location
        (update-in left-location + (first left))))

    (explode? right path)
    (let [right-location (land-right-location coll path)]
      (cond-> (update-in coll path (fn [[v [v2 _]]] (if (coll? v)
                                                      [[(first v) (+ v2 (second v))] 0]
                                                      [(+ v v2) 0])))
        right-location
        (update-in right-location + (second right))))

    (split? left)
    (update-in coll (conj path 0) (fn [x] [(quot x 2) (+ (quot x 2) (mod x 2))]))

    (split? right)
    (update-in coll (conj path 1) (fn [x] [(quot x 2) (+ (quot x 2) (mod x 2))]))

    :else
    (let [coll-left (if (coll? left) (process2 coll left (conj path 0)) coll)]
      (cond
        (not= coll-left coll)
        coll-left

        (coll? right)
        (process2 coll right (conj path 1))

        :else
        coll))))


(def sample [[[[[9,8],1],2],3],4])
(def sample2 [7,[6,[5,[4,[3,2]]]]])

(process2 sample sample [])
(process2 sample2 sample2 [])

(def sample3 [[6,[5,[4,[3,2]]]],1])
(process2 sample3 sample3 [])

(def sample4 [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
(process2 sample4 sample4 [])

(def sample5 [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
(process2 sample5 sample5 [])


(def t
  [[[[[1,1],[2,2]],[3,3]],[4,4]][5 5]]
  )

(def t2 (process2 t t []))

t2

(defn solve [coll]
  (loop [prev nil
         curr coll]
    (if (= prev curr)
      curr
      (recur curr (process2 curr curr [])))))

(def asd1 [[[[1,1],[2,2]],[3,3]],[4,4]])
(def asd [[[[3,0],[5,3]],[4,4]],[5,5]])

(solve (concat asd1 asd))


(def input [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
            [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
            [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
            [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
            [7,[5,[[3,8],[1,4]]]]
            [[2,[2,2]],[8,[8,1]]]
            [2,9]
            [1,[[[9,3],9],[[9,0],[0,7]]]]
            [[[5,[7,4]],7],1]
            [[[[4,2],2],6],[8,7]]])

(def input
  [
   [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
   [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
   [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
   [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
   [7,[5,[[3,8],[1,4]]]]
   [[2,[2,2]],[8,[8,1]]]
   [2,9]
   [1,[[[9,3],9],[[9,0],[0,7]]]]
   [[[5,[7,4]],7],1]
   [[[[4,2],2],6],[8,7]]
   ]
  )

(def input
  [[1 1]
   [2 2]
   [3 3]
   [4 4]
   [5 5]
   [6 6]])

(reduce #(solve (vector %1 %2)) (solve (first input)) (take 1 (rest input)))

(solve (vector (solve (first content)) (rest content)))

(def a [[[[[1 1][2 2]] 3] 4] 5])

(process2 a a [])
