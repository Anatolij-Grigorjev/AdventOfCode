(ns day06.main
  (:require [aoc-commons.core :refer :all])
  (:require [clojure.string :as str]))

(def lights-commands {"turn on " :on
                      "turn off " :off
                      "toggle " :toggle})

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

(defn flick-light [command light]
  (case command
    :on 1
    :off 0
    :toggle (mod (inc light) 2)))

(defn adjust-light [command light]
  (case command
    :on (inc light)
    :off (max (dec light) 0)
    :toggle (inc (inc light))))

(defn command-light [light-transform grid command-position]
  (assoc-in 
   grid 
   (:at command-position) 
   (light-transform 
    (:command command-position) 
    (get-in grid (:at command-position)))))

(def command-light-day01 (partial command-light flick-light))
(def command-light-day02 (partial command-light adjust-light))

(defn apply-command [light-commander grid command]
  (reduce light-commander grid (command-positions command)))

(def apply-command-day02 (partial apply-command command-light-day02))
(def apply-command-day01 (partial apply-command command-light-day01))

(defn apply-commands [applier grid commands]
  (reduce applier grid commands))


(defn count-on [grid]
  (reduce + (flatten grid)))

(defn -main [& args]
  (let [path (first args)
        parsed-commands (read-input-lines parse-lights-command path)]
    (println "Parsed input at path " path)
    (println "Found total commands: " (count parsed-commands))
    (println "Day01 turned on lights: " (count-on (apply-commands apply-command-day01 (off-grid 1000 1000) parsed-commands)))
    (println "Day02 lights brightness: " (count-on (apply-commands apply-command-day02 (off-grid 1000 1000) parsed-commands)))))
