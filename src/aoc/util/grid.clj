(ns aoc.util.grid
  (:require [clojure.string :as str]))

(defn in-bounds [grid ix]
  (if-not
    (coll? grid)
    true
    (let [[i & is] ix]
       (if (<= 0 i (dec (count grid)))
         (in-bounds (nth grid i) is)
         false))))

(defn get [grid ix]
  (get-in grid ix))

(defn get-or [grid ix default]
  (if (in-bounds grid ix)
    (get-in grid ix)
    default))

(defn move [pos vec] (map + pos vec))

(defn from-string [s]
  (let [lines (str/split-lines s)]
    (vec (map vec lines))))