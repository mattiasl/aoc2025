(ns day08
  (:require [clojure.string :refer [split]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (split (slurp "./src/main/clojure/inputs/day08.in") #"\n"))

(defn dist [p q]
  (->> (map - p q)
       (map (fn [x] (* x x)))
       (apply +)))

(defn make-junction-boxes [input]
  (->> (map (fn [x] (->> (re-seq #"\d+" x)
                         (map read-string))) input)))

(defn all-junction-box-distances [junction-boxes]
  (reduce (fn [acc [a b]]
            (assoc acc (dist a b) (set [a b])))
          (sorted-map)
          (combinations junction-boxes 2)))

(defn find-circuits [circuits junction]
  (let [[p q] (vec junction)]
    (filter (fn [x] (or (contains? x p) (contains? x q))) circuits)))

(defn reducer [stop dist->junction-boxes]
  (reduce (fn [circuits [_ junction]]
            (let [circuits-found (find-circuits circuits junction)]
              (if (empty? circuits-found)
                (conj circuits junction)
                (let [merged (->> (conj circuits-found junction)
                                  (apply clojure.set/union))]
                  (if (= (count merged) stop)
                    (reduced junction)                      ;circuit consists of the correct number of junctions
                    (conj (apply disj circuits circuits-found) merged))))))
          #{}
          dist->junction-boxes))

(defn part1 [input connections]
  (->> (make-junction-boxes input)
       (all-junction-box-distances)
       (take connections)
       (reducer Integer/MAX_VALUE)
       (map count)
       (sort >)
       (take 3)
       (reduce *)))

(defn part2 [input]
  (->> (make-junction-boxes input)
       (all-junction-box-distances)
       (reducer (count input))
       (map first)
       (reduce *)))

(comment
  (time (part1 input 1000))
  (time (part2 input))
  )