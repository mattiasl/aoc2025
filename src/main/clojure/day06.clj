(ns day06
  (:require [clojure.string]
            [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day06.in") #"\n"))

(defn transpose [m]
  (apply mapv vector m))

(defn part1 [input]
  (->> (transpose (map (fn [x] (->> (re-seq #"\d+|\+|\*" x)
                                    (map read-string))) input))
       (map (fn [x] (apply (if (= (last x) (symbol "+")) + *) (butlast x))))
       (reduce +)))

(defn spaces? [row]
  (empty? (clojure.string/trim (clojure.string/join row))))

(defn make-cephalopod-math [input]
  (loop [answer []
         current []
         rows (transpose input)]
    (let [[row & rs] rows]
      (if (nil? row)
        (conj answer current)
        (if (spaces? row)
          (recur (conj answer current) [] rs)
          (recur answer (conj current row) rs))))))

(defn cephalopod-math [maths]
  (let [op (if (= (last (first maths)) \+) + *)]
    (->> (reverse maths)
         (map butlast)
         (map (fn [x] (clojure.string/join x)))
         (map read-string)
         (apply op))))

(defn part2 [input]
  (->> (make-cephalopod-math input)
       (map cephalopod-math)
       (reduce +)))

(comment
  (time (part1 input))
  ; note that for part 2 the input must contain correct number of spaces on all rows
  (time (part2 input))
  )