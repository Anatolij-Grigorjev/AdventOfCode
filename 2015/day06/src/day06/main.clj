(ns day06.main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def lights-commands {"turn on " :on
                      "turn off " :off
                      "toggle " :toggle})

(defn find-first [f col]
  (first (drop-while (complement f) col)))

(defn decode-pair [xy-csv-string]
  (let [xy (str/split xy-csv-string #"," 2)]
    {
     :x (Integer/parseInt (first xy))
     :y (Integer/parseInt (second xy))
    }))

(defn parse-lights-command [line]
  (let [command-entry (find-first #(.startsWith line (first %)) lights-commands)
        coords-line (.substring line (count (first command-entry)))
        coords (str/split coords-line #" through " 2)]
    {
     :command (second command-entry)
     :from (decode-pair (first coords))
     :to (decode-pair (second coords))
     }))

(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map parse-lights-command (line-seq reader)))))

(defn make-row [width value]
  (apply vector (take width (repeat value))))

(defn off-grid [width height]
  (apply vector (take height (repeat (make-row width 0)))))

(defn x-range [command]
  (range (:x (:from command)) (inc (:x (:to command)))))

(defn y-range [command]
  (range (:y (:from command)) (inc (:y (:to command)))))

(defn command-positions [command]
  (apply vector 
         (for [x (x-range command)
               y (y-range command)]
           {:command (:command command) :at [x y]})))

(defn apply-command-light [command light]
  (case command
    :on 1
    :off 0
    :toggle (mod (inc light) 2)))

(defn command-light [grid command-position]
  (assoc-in 
   grid 
   (:at command-position) 
   (apply-command-light 
    (:command command-position) 
    (get-in grid (:at command-position)))))

(defn apply-command [grid command]
  (reduce command-light grid (command-positions command)))

(defn apply-commands [grid commands]
  (reduce apply-command grid commands))


(defn count-on [grid]
  (reduce + (flatten grid)))

(defn -main [& args]
  (let [path (first args)
        parsed-commands (read-input path)]
    (println "Parsed input at path " path)
    (println "Found total commands: " (count parsed-commands))
    (println "Day01 turned on lights: " (count-on (apply-commands (off-grid 1000 1000) parsed-commands)))))
