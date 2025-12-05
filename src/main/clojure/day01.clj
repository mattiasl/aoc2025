(ns day01
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day01.in") #"\n"))

(defn parse-input [input]
  (map (fn [x]
         (let [dir (first x)
               rot (read-string (subs x 1))]
           [dir rot]))
       input))

(def dir->fn {\L - \R +})

;not pretty or fast but it works
(defn count-zero-clicks [dial rot dir]
  (let [fn (dir->fn dir)]
    (loop [rot rot
           dial dial
           zeros 0]
      (if (zero? rot)
        zeros
        (let [pos' (mod (fn dial 1) 100)]
          (recur (dec rot) pos' (if (zero? pos') (inc zeros) zeros)))))))

(defn solver [input]
  (loop [dial 50
         answers [0 0]
         rotations (parse-input input)]
    (let [[[dir rot] & tail] rotations
          [part1 part2] answers]
      (if (nil? dir)
        answers
        (let [fn (dir->fn dir)
              dial' (mod (fn dial rot) 100)
              part1' (+ part1 (if (zero? dial') 1 0))
              part2' (+ part2 (count-zero-clicks dial rot dir))]
          (recur dial' [part1' part2'] tail))))))

(comment
  (time (solver input))
  )