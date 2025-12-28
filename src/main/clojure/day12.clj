(ns day12
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day12.in") #"\n"))

(defn part1 [input]
  (->> (filter (fn [x] (clojure.string/includes? x "x")) input)
       (map (fn [x] (->> (re-seq #"\d+" x)
                         (map read-string))))
       (filter (fn [[x y & z]]
                 (>= (* x y) (* 3 3 (apply + z)))))
       (count)))

(comment
  (time (part1 input))
  )