(ns day02.main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn dims2sides [dims]
  [(* (:w dims) (:l dims)) (* (:w dims) (:h dims)) (* (:h dims) (:l dims))])

(defn parse-dim [text-line]
  (zipmap [:l :w :h] (map #(Integer/parseInt %) (str/split text-line #"x" 3))))

(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map parse-dim (line-seq reader)))))

; ribbon
(defn volume [dims]
  (* (:w dims) (:l dims) (:h dims)))

(defn small-side-sum [dims]
  (let [sorted-sides (sort < (vals dims))
        side-a (first sorted-sides)
        side-b (second sorted-sides)]
    (+ side-a side-b)))

(defn smallest-side-perimeter [dims]
  (* 2 (small-side-sum dims)))

(defn total-box-ribbon [dims]
  (+ (smallest-side-perimeter dims) (volume dims)))

(defn total-needed-feet-ribbon [dims-list]
  (reduce + (map total-box-ribbon dims-list)))


; box surface
(defn surface-area [dims]
  (* 2 (apply + (dims2sides dims))))

(defn extra-slack [dims]
  (apply min (dims2sides dims)))

(defn total-box-area [dims]
  (+ (surface-area dims) (extra-slack dims)))

(defn total-needed-feet-wrap [dims-list]
  (reduce + (map total-box-area dims-list)))



(defn -main [& args]
  (let [path (first args)
        presents-dims (read-input path)]
    (println "Reading " path "...")
    (println "Needed feet to wrap box: " (total-needed-feet-wrap presents-dims))
    (println "Needed feet for ribbon: " (total-needed-feet-ribbon presents-dims))))
