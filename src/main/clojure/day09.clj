(ns day09
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day09.in") #"\n"))

(defn str->ints [x]
  (->> (re-seq #"\d+" x)
       (map read-string)))

(defn area [[p q]]
  (->> (map - p q)
       (map abs)
       (map inc)
       (apply *)))

(defn part1 [input]
  (->> (combinations (map str->ints input) 2)
       (map area)
       (apply max)))

(defn make-green-boundary [red-tiles]
  (reduce (fn [green [r1 r2]]
            (let [[x1 y1] r1
                  [x2 y2] r2]
              (let [greens (if (= x1 x2)
                             (for [y (range (inc (min y1 y2)) (max y1 y2))] [x1 y])
                             (for [x (range (inc (min x1 x2)) (max x1 x2))] [x y1]))]
                (apply conj green greens))))
          #{}
          (partition 2 1 (conj red-tiles (last red-tiles)))))

(defn find-horizontal-lines [red-tiles]
  (reduce (fn [lines [r1 r2]]
            (let [[x1 y1] r1
                  [x2 y2] r2]
              (if (= y1 y2)
                (let [x (abs (- x2 x1))]
                  (update lines x (fn [e] (if (nil? e) [y1] (conj e y1)))))
                lines)))
          (sorted-map)
          (partition 2 1 (conj red-tiles (last red-tiles)))))

(defn inside? [y->x [x y]]
  (let [[xs xm] (y->x y)]
    (<= xs x xm)))

(defn find-largest-area [included? inside? red-tiles]
  (as-> (filter (fn [[_ y]] (included? y)) red-tiles) $
        (combinations $ 2)
        (filter inside? $)
        (map area $)
        (apply max $)))

(defn make-all-inside-predicate [boundary]
  (let [y->x (reduce (fn [a [x y]]
                       (update a y (fn [z]
                                     (if (nil? z)
                                       [x x]
                                       (apply (juxt min max) (conj z x))))))
                     {}
                     boundary)
        inside? (partial inside? y->x)]
    (fn [[[x1 y1] [x2 y2]]]
      (let [ul [(min x1 x2) (min y1 y2)]
            ur [(max x1 x2) (second ul)]
            br [(first ur) (max y1 y2)]
            bl [(first ul) (second br)]]
        (every? inside? [ul ur br bl])))))

(defn part2 [input]
  (let [red-tiles (map str->ints input)
        [upper-boundary lower-boundary] (->> (find-horizontal-lines red-tiles)
                                             (take-last 2)
                                             (vals)
                                             (flatten)
                                             (sort))
        green-boundary (make-green-boundary red-tiles)
        inside? (make-all-inside-predicate green-boundary)]
    (max (find-largest-area (partial >= upper-boundary) inside? red-tiles)
         (find-largest-area (partial <= lower-boundary) inside? red-tiles))))

(comment
  (time (part1 input))
  (time (part2 input))
  )

;below are some utils used to visualize the shape
(defn boundary-max [red-tiles]
  (let [xm (apply max (map first red-tiles))
        ym (apply max (map second red-tiles))]
    [(int xm) (int ym)]))

(defn draw-tiles [input scale]
  (let [red-tiles (->> (map str->ints input)
                       (mapv (fn [[x y]] [(int (/ x scale)) (int (/ y scale))])))
        green-boundary (make-green-boundary red-tiles)
        red-tiles (into #{} red-tiles)
        [xm ym] (boundary-max red-tiles)]
    (spit "./day09.md" (->> (reduce (fn [rows y]
                                      (let [row (reduce (fn [row x]
                                                          (let [ch (cond
                                                                     (contains? red-tiles [x y]) "#"
                                                                     (contains? green-boundary [x y]) "X"
                                                                     :else " ")]
                                                            (conj row ch)))
                                                        [] (range (inc xm)))]
                                        (conj rows row "\n")))
                                    [] (range (inc ym)))
                            (flatten)
                            (clojure.string/join)))))

(comment
  (time (draw-tiles input 1000))
  )