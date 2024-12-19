(ns aoc.day19
  (:require [clojure.string :as str]))

(defn parse [s]
  (let [[up down] (str/split s #"\n\n")
        parts (str/split up #", ")
        todo (str/split-lines down)]
    [parts todo]))

(defn ways [parts s cache]
  (cond
    (empty? s) 1
    (contains? @cache s) (get @cache s)
    :else (let [recs (for [part parts
                           :let [l (count part)]
                           :when (>= (count s) l)
                           :when (= part (subs s 0 l))]
                       (ways parts (subs s l) cache))
                res (reduce + recs)]
            (vswap! cache #(assoc % s res))
            res)))

(defn -main []
  (let [[parts todo] (parse (slurp "input/day19.txt"))
        cache (volatile! {})
        counts (map #(ways parts % cache) todo)
        good (filter #(> % 0) counts)
        total (reduce + counts)]
    (println (count good))
    (println total)))