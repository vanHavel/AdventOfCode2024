(ns aoc.day9)
(require '[clojure.core.match :refer [match]])

(def FREE -1)
(defn expand ([digits] (expand digits 0 []))
  ([digits id acc] (match [digits]
                      [([] :seq)] acc
                      [([d] :seq)] (expand [] (inc id) (into acc (repeat d id)))
                      [([d1 d2 & ds] :seq)] (expand ds (inc id) (into acc (concat (repeat d1 id) (repeat d2 FREE)))))))

(defn defrag ([disk] (defrag disk (reverse (filter #(not= FREE %) disk))))
  ([disk elems](let [cell (first disk) elem (first elems)]
                 (if (= cell FREE) (cons elem (defrag (rest disk) (rest elems)))
                   (if (<= elem cell) (take-while #(= cell %) elems)
                     (cons cell (defrag (rest disk) elems)))))))

(defn block-lists ([digits] (block-lists digits 0 0))
  ([digits index id] (if (empty? (rest digits))
                       ['() (cons [(first digits) index id] '())]
                       (let [[d1 d2 & ds] digits [frees blocks] (block-lists ds (+ index d1 d2) (inc id))]
                         [(cons [d2 (+ index d1)] frees) (cons [d1 index id] blocks)]))))

(defn move [disk frees block]
  (let [condition #(< (first %) (first block))
        [skipped found] [(take-while condition frees) (drop-while condition frees)]]
    (if (or (empty? found) (> (second (first found)) (second block)))
      [disk frees]
      (let [[[fsize fpos] & others] found
            [bsize bpos bid] block
            update [(- fsize bsize) (+ fpos bsize)]
            newfree (concat skipped (if (= fsize bsize) [] [update]) others)
            newdisk1 (apply (partial assoc disk) (interleave (range fpos (+ fpos bsize)) (repeat bid)))
            newdisk2 (apply (partial assoc newdisk1) (interleave (range bpos (+ bpos bsize)) (repeat FREE)))]
        [newdisk2 newfree]))))

(defn move-all [disk frees blocks]
  (first (reduce #(move (first %1) (second %1) %2) [disk frees] blocks)))

(defn -main []
  (let [digits (map #(parse-long (str %)) (slurp "input/day9.txt"))
        expanded (expand digits)
        defragged (defrag expanded)
        [frees blocks] (block-lists digits)
        defragged-2 (move-all expanded frees (reverse blocks))
        checksum #(reduce + (map-indexed * %))]
    (println (checksum defragged))
    (println (checksum (replace {FREE 0} defragged-2)))))