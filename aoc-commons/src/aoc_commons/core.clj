(ns aoc-commons.core
  (:require [clojure.java.io :as io]))

; all permutations of col, taken from https://stackoverflow.com/a/26076537
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn max-by [f col]
  (first (reverse (sort-by f col))))

(defn read-input-lines [transform-f path]
  (with-open [reader (io/reader path)]
    (doall (map transform-f (line-seq reader)))))

(defn find-first [f col]
  (first (drop-while (complement f) col)))

(defn parse-int [num-string]
  (try
    (Integer/parseInt num-string)
    (catch Exception e nil)))