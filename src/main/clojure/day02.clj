(ns day02
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day02.in") #","))

(defn string->ints [s]
  (map read-string (re-seq #"\d+" s)))

(defn split-number-part1 [n]
  (let [len (count n)
        half (quot len 2)]
    [(subs n 0 half) (subs n half)]))

(defn invalid-part1? [n]
  (apply = (split-number-part1 (str n))))

(defn get-possible-chunk-sizes [len]
  (filter (fn [x] (= (mod len x) 0)) (range 1 len)))

(defn split-number [chunk-size n]
  (map clojure.string/join (partition chunk-size n)))

(defn invalid-part2? [n]
  (let [s (str n)]
    (->> (get-possible-chunk-sizes (count s))
         (map (fn [chunk-size] (split-number chunk-size s)))
         (map (fn [x] (apply = x)))
         (some true?))))

(defn solver [input invalid?]
  (->> (map string->ints input)
       (map (fn [[s e]] (filter invalid? (range s (inc e)))))
       (flatten)
       (apply +)))

(comment
  (time (solver input invalid-part1?))
  (time (solver input invalid-part2?))
  )