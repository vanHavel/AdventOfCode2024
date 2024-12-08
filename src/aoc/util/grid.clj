(ns aoc.util.grid
  (:require [clojure.string :as str]))

(defn in-bounds [grid ix]
  (if (empty? ix)
    true
    (let [[i & is] ix]
       (if (<= 0 i (dec (count grid)))
         (in-bounds (nth grid i) is)
         false))))

(defn infer-dim [grid]
  (if (coll? grid)
    (inc (infer-dim (first grid)))
    0))

(defn all-indices
  ([grid] (all-indices grid (infer-dim grid)))
  ([grid dim] (if (zero? dim)
                [[]]
                (into [] (apply concat (for [i (range(count grid))]
                  (map #(into [i] %) (all-indices (nth grid i) (dec dim)))))))))

(defn get [grid ix]
  (get-in grid ix))
(defn get-or [grid ix default]
  (if (in-bounds grid ix)
    (get-in grid ix)
    default))
(defn find [grid value]
  (filter #(= value (get grid %)) (all-indices grid)))

(defn move [pos vec] (into [] (map + pos vec)))
(defn invert [pos] (into [] (map - pos)))
(defn diff [p q] (move p (invert q)))

(defn from-string [s]
  (let [lines (str/split-lines s)]
    (vec (map vec lines))))

(def dir2 {:up [-1 0] :down [1 0] :left [0 -1] :right [0 1]})
(defn turn-right [dir]
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up))
(defn turn-left [dir] (nth (iterate turn-right dir) 3))

(defn adjust [grid ix val] (assoc-in grid ix val))