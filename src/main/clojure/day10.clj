(ns day10
  (:require [clojure.math.combinatorics]
            [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/inputs/day10.in") #"\n"))

(defn machine-factory [line]
  (let [part (split line #" ")]
    {:exp (->> (re-seq #"[\.#]" (first part))
               (reduce (fn [a c] (conj a (if (= "#" c) 1 0))) []))
     :but (->> (rest (butlast part))
               (mapv (fn [button]
                       (->> (re-seq #"\d+" button)
                            (mapv read-string)))))
     :jol (->> (re-seq #"\d+" (last part))
               (mapv read-string))}))

(defn fewest-presses-p1
  ([machine] (fewest-presses-p1 (machine :exp) (machine :but)))
  ([expected buttons]
   (->> (reduce (fn [presses buttons]
                  (let [actual (->> (flatten buttons)
                                    (frequencies)
                                    (reduce (fn [a [k v]]
                                              (update a k (fn [_] (mod v 2)))) (vec (repeat (count expected) 0))))]
                    (if (= actual expected)
                      (conj presses buttons)
                      presses)))
                []
                (clojure.math.combinatorics/subsets buttons)))))

(defn part1 [input]
  (->> (map machine-factory input)
       (map fewest-presses-p1)
       (map (fn [x] (->> (map count x)
                         (apply min))))
       (reduce +)))

(def inf Integer/MAX_VALUE)

(defn all-button-combos [buttons]
  (let [size (->> (flatten buttons)
                  (apply max))]
    (->> (clojure.math.combinatorics/subsets buttons)
         (remove empty?)
         (reduce (fn [jolts buttons]
                   (let [jolt (reduce (fn [a [k v]]
                                        (update a k (fn [x] (+ x v))))
                                      (vec (repeat (inc size) 0)) (frequencies (flatten buttons)))
                         b (count buttons)]
                     (update jolts jolt (fn [val] (if (nil? val) b (min val b)))))) {}))))

(def fewest-presses-p2
  (memoize (fn [jolt all-combos]
             (if (every? zero? jolt)
               0
               (if (some neg? jolt)
                 inf
                 (let [jolt-mod-2 (mapv (fn [x] (mod x 2)) jolt)
                       combos (filter (fn [[delta-jolt _]]
                                        (= jolt-mod-2 (mapv (fn [x] (mod x 2)) delta-jolt))) all-combos)]
                   (if (every? even? jolt)
                     (let [[even presses] (first combos)]
                       (if (contains? all-combos even)
                         (min
                           (+ presses (fewest-presses-p2 (mapv - jolt even) all-combos))
                           (* 2 (fewest-presses-p2 (mapv (fn [x] (quot x 2)) jolt) all-combos)))
                         (* 2 (fewest-presses-p2 (mapv (fn [x] (quot x 2)) jolt) all-combos))))
                     (if (empty? combos)
                       inf
                       (->> (map (fn [[delta-jolt presses]]
                                   (+ presses (fewest-presses-p2 (mapv - jolt delta-jolt) all-combos)))
                                 combos)
                            (apply min))))))))))

(defn part2 [input]
  (->> (map machine-factory input)
       (pmap (fn [machine]
               (fewest-presses-p2 (machine :jol) (all-button-combos (machine :but)))))
       (reduce +)))

(comment
  (time (part1 input))
  (time (part2 input))
  )