(ns day07
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day07.in") #"\n"))

(defn parse-row [row]
  (reduce (fn [a [x v]] (if (= v \.) a (conj a x))) #{} (map-indexed vector row)))

(defn parse [input]
  (->> (reduce (fn [a c] (conj a (parse-row c))) [] input)
       (filter not-empty)))

(defn part1 [beams a c]
  (let [[l r] ((juxt inc dec) c)]
    (-> (update a l (fn [x] (+ 1 (or x 0) (beams c))))
        (update r (fn [x] (or x 0))))))

(defn part2 [beams a c]
  (reduce (fn [acc beam']
            (assoc acc beam' (+ (or (acc beam') 0) (beams c))))
          a
          ((juxt inc dec) c)))

(defn reducer [part beams splitters]
  (let [beams-in (into #{} (keys beams))]
    (reduce (partial part beams)
            (select-keys beams (clojure.set/difference beams-in splitters))
            (clojure.set/intersection beams-in splitters))))

(defn solver [input part start-value]
  (let [diagram (parse input)]
    (->> (reduce (partial reducer part)
                 {(ffirst diagram) start-value}
                 (rest diagram))
         (vals)
         (reduce +))))

(comment
  (time (solver input part1 0))
  (time (solver input part2 1))
  )