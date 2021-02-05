(ns aoc-commons.core
  (:require [clojure.java.io :as io]))

(defn read-input-lines [transform-f path]
  (with-open [reader (io/reader path)]
    (doall (map transform-f (line-seq reader)))))

(defn find-first [f col]
  (first (drop-while (complement f) col)))

(defn parse-int [num-string]
  (try
    (Integer/parseInt num-string)
    (catch Exception e nil)))