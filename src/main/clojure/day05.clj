(ns day05
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day05.in") #"\n\n"))

(defn str->ints [x]
  (->> (re-seq #"\d+" x)
       (map read-string)))

(defn make-predicate [range]
  (let [[a b] (str->ints range)]
    (fn [x] (<= a x b))))

(defn part1 [[ranges ids]]
  (let [predicates (->> (split ranges #"\n")
                        (map make-predicate))]
    (->> (str->ints ids)
         (filter (fn [x] ((apply some-fn predicates) x)))
         (count))))

(defn overlaps? [[a b] [c d]]
  (or (<= a c b) (<= c a d)))

(defn merge-ranges [[a b] [c d]]
  [(min a c) (max b d)])

(defn part2 [[ranges _]]
  (->> (loop [answer #{}
              queue (->> (split ranges #"\n")
                         (map str->ints))]
         (if (empty? queue)
           answer
           (let [[r & rs] queue]
             (if-let [overlap (->> (filter (fn [x] (overlaps? r x)) answer)
                                   (first))]
               (recur (disj answer overlap) (conj rs (merge-ranges r overlap)))
               (recur (conj answer r) rs)))))
       (map (fn [x] (abs (dec (apply - x)))))
       (apply +)))

(comment
  (time (part1 input))
  (time (part2 input))
  )