(ns aoc.day23
  (:require [clojure.string :as str]))

(defn refine-cliques [current nodes edges]
  (set (for [cur current new nodes
             :when (every? (fn [node] (contains? edges [node new])) cur)]
         (conj cur new))))

(defn -main []
  (let [lines (str/split-lines (slurp "input/day23.txt"))
        edges (set (mapcat (fn [s] (let [[s t] (str/split s #"-")] [[s t] [t s]])) lines))
        nodes (set (map first edges))
        init (set (for [node nodes] #{node}))
        iter (iterate (fn [cur] (refine-cliques cur nodes edges)) init)
        ans1 (count (filter (fn [clique] (contains? (set (map first clique)) \t)) (nth iter 2)))
        max-clique (first (last (take-while not-empty iter)))]
    (println ans1)
    (println (str/join "," (sort max-clique)))))