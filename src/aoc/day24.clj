(ns aoc.day24
  (:require [clojure.string :as str] [loom.alg :as alg] [loom.graph :as graph] [loom.io :as io] [loom.label :as label]))

(defn parse-op [op] (case op "OR" (fn [a b] (or a b)) "AND" (fn [a b] (and a b)) "XOR" not=))
(defn parse [input]
  (let [[upper lower] (str/split input #"\n\n")
        init (into {} (for [line (str/split-lines upper)
                            :let [[left right] (str/split line #": ")]]
                        [left, (> (parse-long right) 0)]))
        rules (str/split-lines lower)
        gates (into {} (for [rule rules
                             :let [[i1 op i2 _ node] (str/split rule #" ")]]
                         [node [op i1 i2]]))
        edges (apply concat (for [rule rules
                                  :let [[i1 _ i2 _ node] (str/split rule #" ")]]
                              [[i1 node] [i2 node]]))
        ]
    [init gates edges]))

(defn -main []
  (let [[init, gates, edges] (parse (slurp "input/day24.txt"))
        g (apply graph/digraph edges)
        topsort (alg/topsort g)
        final (reduce (fn [m node]
                        (if (contains? m node)
                          m
                          (let [[op i1 i2] (get gates node)]
                            (assoc m node ((parse-op op) (get m i1) (get m i2))))))
                      init topsort)
        z (reduce +
                  (map #(bit-shift-left (if (get final %) 1 0) (parse-long (subs % 1)))
                       (sort-by #(subs % 1) (filter #(str/starts-with? % "z") (keys final)))))
        annotated (reduce (fn [graph [node [op _ _]]] (label/add-label graph node (str op "-" node))) g gates)]
    (println z)
    (io/view annotated)))