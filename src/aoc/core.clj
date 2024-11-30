(ns aoc.core
  (:require [clojure.string :as str])
  (:gen-class))

(:use 'clojure.java.io)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def content (slurp "input/test.txt"))
  (println content))

