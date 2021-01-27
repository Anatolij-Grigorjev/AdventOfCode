(ns day02.main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn dims2sides [dims]
  [(* (:w dims) (:l dims)) (* (:w dims) (:h dims)) (* (:h dims) (:l dims))])

(defn surface-area [dims]
  (* 2 (apply + (dims2sides dims))))

(defn extra-slack [dims]
  (apply min (dims2sides dims)))

(defn parse-dim [text-line]
  (zipmap [:l :w :h] (map #(Integer/parseInt %) (str/split text-line #"x" 3))))

(defn read-input [path] 
  (with-open [reader (io/reader path)]
    (doall (map parse-dim (line-seq reader)))))

(defn total-box-area [dims]
  (+ (surface-area dims) (extra-slack dims)))

(defn total-needed-feet [dims-list]
  (reduce + (map total-box-area dims-list)))

(defn -main [& args]
  (let [path (first args)]
    (println "Reading " path "...")
    (println "Needed feet: " (total-needed-feet (read-input path)))))
