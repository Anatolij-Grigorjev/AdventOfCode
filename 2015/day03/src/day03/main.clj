(ns day03.main)

(defn read-input [path] (slurp path))

(def opposite-dir {\< \>
                   \> \<
                   \v \^
                   \^ \v})

(defn move-between-houses [all-directions]
  
  (loop [ 
         total 1
         current-house {}
         directions all-directions]
    (let [next-direction (first directions)
        next-house (get current-house next-direction)]
    (if (nil? next-direction)
      total
      (if (nil? next-house)
        (recur 
         (inc total)
         (get (assoc current-house next-direction {(get opposite-dir next-direction) current-house}) next-direction)
         (rest directions))
        (recur total (get current-house next-direction) (rest directions)))))))



(defn -main [& args]
  (let [path (first args)]
    (println "Reading " path "...")
    (println "houses visited: " (move-between-houses (read-input path)))))
