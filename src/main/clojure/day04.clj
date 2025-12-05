(ns day04
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day04.in") #"\n"))

(defn parse-diagram [input]
  (apply clojure.set/union (for [[y row] (map-indexed vector input)
                                 [x ch] (map-indexed vector row)]
                             (if (= \@ ch) #{[x y]} #{}))))

(defn get-adjacent [diagram pos dirs]
  (reduce (fn [a dir]
            (let [pos' (mapv + dir pos)]
              (if (contains? diagram pos') (conj a pos') a)))
          #{} dirs))

(defn get-accessible [diagram]
  (let [dirs [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]]
    (filter (fn [pos] (> 4 (count (get-adjacent diagram pos dirs)))) diagram)))

(defn part1 [input]
  (->> (parse-diagram input)
       (get-accessible)
       (count)))

(defn part2 [input]
  (loop [diagram (parse-diagram input)
         removed 0]
    (let [acc (get-accessible diagram)]
      (if (empty? acc)
        removed
        (recur (clojure.set/difference diagram acc) (+ removed (count acc)))))))

(comment
  (time (part1 input))
  (time (part2 input))
  )