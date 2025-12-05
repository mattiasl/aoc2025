(ns day03
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day03.in") #"\n"))

(defn jolt [batteries]
  (reduce (fn [a c] (+ (* 10 a) c)) batteries))

(defn max-jolt [on bat]
  (let [best (jolt on)]
    (or (->> (range (count on))
             (map (fn [x] (into [] (concat (subvec on 0 x) (subvec on (inc x)) [bat]))))
             (drop-while (fn [on'] (> best (jolt on'))))
             (first))
        on)))

(defn max-jolt-for-bank [n bank]
  (let [batteries (->> (re-seq #"\d" bank)
                       (map read-string))]
    (reduce max-jolt (vec (take n batteries)) (drop n batteries))))

(defn solver [input n]
  (->> (map (partial max-jolt-for-bank n) input)
       (map jolt)
       (apply +)))

(comment
  (time (solver input 2))
  (time (solver input 12))
  )