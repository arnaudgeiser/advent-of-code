(ns advent-of-code.day19
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 19))

#_(def content
    ["px{a<2006:qkq,m>2090:A,rfg}"
     "pv{a>1716:R,A}"
     "lnx{m>1548:A,A}"
     "rfg{s<537:gd,x>2440:R,A}"
     "qs{s>3448:A,lnx}"
     "qkq{x<1416:A,crn}"
     "crn{x>2662:A,R}"
     "in{s<1351:px,qqz}"
     "qqz{s>2770:qs,m<1801:hdj,R}"
     "gd{a>3333:R,R}"
     "hdj{m>838:A,pv}"
     ""
     "{x=787,m=2655,a=1222,s=2876}"
     "{x=1679,m=44,a=2067,s=496}"
     "{x=2036,m=264,a=79,s=2244}"
     "{x=2461,m=1339,a=466,s=291}"
     "{x=2127,m=1623,a=2188,s=1013}"])

(def rules (first (split-with #(not= % "") content)))
(def workflows (rest (second (split-with #(not= % "") content))))

(defn rule? [s]
  (str/includes? s ":"))

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

(def rules'
  (->> (map parse-name-rule rules)
       (into {})))

(defn parse-workflow [s]
  (let [[_ & r] (re-find #"x=([0-9]*),m=([0-9]*),a=([0-9]*),s=([0-9]*)" s)
        [x m a s] (mapv parse-long r)]
    {:x x :m m :a a :s s}))

(def workflows' (mapv parse-workflow workflows))

(get rules' "in")

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
      (let [res (evaluate-rule workflow (get rules' workflow-id))]
        (recur res)))))

(defn solution1 []
  (->> (filter #(= "A" (evaluate-workflow %)) workflows')
       (mapcat vals)
       (reduce +)))

(def init ["in" {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}])

(defn evaluate-rule2 [workflow {:keys [expr left right]}]
  (let [[c sign v] expr
        [begin end] (c workflow)
        sign-fn (if (= sign :<) < >)
        ;; I check on sign "v" is probably missing here...
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

(defn solution2 [workflow]
  (loop [workflows [workflow]
         accepted 0
         refused []]
    (if (seq workflows)
      (let [[workflow-id workflow] (first workflows)
            res (evaluate-rule2 workflow (get rules' workflow-id))
            accepted' (filter (fn [[workflow-id]] (= workflow-id "A")) res)
            refused' (filter (fn [[workflow-id]] (= workflow-id "R")) res)
            workflows' (filter (fn [[workflow-id]] (not (#{"A" "R"} workflow-id))) res)]
        (recur (concat (rest workflows) workflows')
               (+ accepted (reduce + (map (comp block-size second) accepted')))
               (concat refused refused')))
      accepted)))

(solution1) ;; 391132
(solution2 init) ;; 128163929109524
