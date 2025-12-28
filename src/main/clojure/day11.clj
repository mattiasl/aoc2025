(ns day11
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day11.in") #"\n"))

(defn make-graph [input]
  (reduce (fn [a c]
            (let [[src & dest] (split c #"[:\ ]")]
              (assoc a src (into #{} (rest dest))))) {} input))

(def count-paths
  (memoize (fn ([src target graph] (count-paths src target #{} graph))
             ([src target req graph] (if (= src target)
                                       (if (empty? req) 1 0)
                                       (reduce (fn [a dest]
                                                 (+ a (count-paths dest target (disj req dest) graph)))
                                               0 (graph src)))))))

(defn part1 [input]
  (->> (make-graph input)
       (count-paths "you" "out")))

(defn part2 [input]
  (->> (make-graph input)
       (count-paths "svr" "out" #{"dac" "fft"})))

(comment
  (time (part1 input))
  (time (part2 input))
  )