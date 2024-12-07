(ns aoc.day7
  (:require [clojure.string :as str]))
(require '[clojure.core.match :refer [match]])

(defn parse [input]
  (for [line (str/split-lines input)
        :let [[res parts] (str/split line #": ")]]
    [(parse-long res) (map parse-long (str/split parts #" "))]))

(defn possible? [fns challenge]
  (let [[res nums] challenge]
    (match [nums]
           [([n] :seq)] (= res n)
           [([n1 n2 & ns] :seq)] (some identity (for [fn fns] (possible? fns [res (cons (fn n1 n2) ns)]))))))

(defn -main []
  (let [challenges (parse (slurp "input/day7.txt"))
        correct (filter #(possible? [+ *] %) challenges)
        conc (fn [n1 n2] (parse-long (str n1 n2)))
        correct2 (filter #(possible? [+ * conc] %) challenges)]
    (println (reduce + (map first correct)))
    (println (reduce + (map first correct2)))))