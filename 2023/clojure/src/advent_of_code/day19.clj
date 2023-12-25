(ns advent-of-code.day19
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 19))

(defn rule? [s] (str/includes? s ":"))

(defn parse-rule [acc s]
  (let [[expr rest] (str/split s #":" 2)
        [left right] (str/split rest #"," 2)
        [_ v sign value] (re-find #"(.*)([<>])(.*)" expr)]
    (cond-> (assoc acc :expr [(keyword v) (keyword sign) (parse-long value)])
      (rule? left)
      (assoc :left (parse-rule {} left))
      (not (rule? left))
      (assoc :left left)
      (rule? right)
      (assoc :right (parse-rule {} right))
      (not (rule? right))
      (assoc :right right))))

(defn parse-name-rule [s]
  (let [[name rule] (str/split s #"\{")]
    [name (parse-rule {} (apply str (butlast rule)))]))

(defn parse-workflow [s]
  (let [[_ & r] (re-find #"x=([0-9]*),m=([0-9]*),a=([0-9]*),s=([0-9]*)" s)
        [x m a s] (mapv parse-long r)]
    {:x x :m m :a a :s s}))

(def rules (into {} (map parse-name-rule (first (split-with #(not= % "") content)))))
(def workflows (map parse-workflow (rest (second (split-with #(not= % "") content)))))

(defn evaluate-rule [workflow {:keys [expr left right] :as r}]
  (let [[c sign v] expr]
    (case sign
      :< (if (< (c workflow) v) (evaluate-rule workflow left) (evaluate-rule workflow right))
      :> (if (> (c workflow) v) (evaluate-rule workflow left) (evaluate-rule workflow right))
      nil r)))

(defn evaluate-workflow [workflow]
  (loop [workflow-id "in"]
    (if (#{"A" "R"} workflow-id)
      workflow-id
      (let [res (evaluate-rule workflow (get rules workflow-id))]
        (recur res)))))

(defn solution1 []
  (->> (filter #(= "A" (evaluate-workflow %)) workflows)
       (mapcat vals)
       (reduce +)))

(def init ["in" {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}])

(defn evaluate-rule2 [workflow {:keys [expr left right]}]
  (let [[c sign v] expr
        [begin end] (c workflow)
        sign-fn (if (= sign :<) < >)
        left-begin-end  (if (sign-fn begin v end) [begin (dec v)] [(inc v) end])
        right-begin-end (if (sign-fn begin v end) [v end] [begin v])]
    (cond-> []
      (string? left)
      (conj [left (assoc workflow c left-begin-end)])
      (string? right)
      (conj [right (assoc workflow c right-begin-end)])
      (not (string? left))
      (concat (evaluate-rule2 (assoc workflow c left-begin-end) left))
      (not (string? right))
      (concat (evaluate-rule2 (assoc workflow c right-begin-end) right)))))

(defn block-size [part]
  (->> (vals part)
       (map (fn [[a b]] (inc (Math/abs (- a b)))))
       (reduce *)))

(defn solution2 []
  (loop [workflows [init]
         accepted 0]
    (if (seq workflows)
      (let [[workflow-id workflow] (first workflows)
            res (evaluate-rule2 workflow (get rules workflow-id))
            accepted' (filter (fn [[wid]] (= wid "A")) res)
            workflows' (filter (fn [[wid]] (not (#{"A" "R"} wid))) res)]
        (recur (concat (rest workflows) workflows')
               (+ accepted (reduce + (map (comp block-size second) accepted')))))
      accepted)))

(solution1) ;; 391132
(solution2) ;; 128163929109524
