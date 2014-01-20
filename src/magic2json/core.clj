(ns magic2json.core
  (:use [clojure-csv.core]
        [clojure.pprint])
  (:require [clojure.data.json :as json]))

(defn parse-row [agg row]
  (let [discipline (nth row 3)
        type (nth row 4)
        category (nth row 5)
	name (nth row 0)
	value (nth row 1)]
    (if (= name category)
      (assoc-in agg [discipline type category] {"name" category "size" (Integer. value)})
      (update-in agg [discipline type category] #(conj (or % []) {"name" name "size" (Integer. value)})))))

(defn mapify [x y]
  (hash-map "name" (first x) "children" y))

(defn get-map []
  (hash-map "name" "" "children"
  (map
    (fn [disc-map]
      (mapify
        disc-map
	(if (> (count (last disc-map)) 1)
          (map
            (fn [type-map]
              (mapify
                type-map
	        (if (> (count (last type-map)) 1)
                  (map #(hash-map "name" (first %) "children" (last %)) (last type-map))
                  (last type-map))))
            (last disc-map))
          (map #(hash-map "name" (first %) "children" (last %)) (last disc-map)))))
    (reduce parse-row {} (parse-csv (slurp "practice.csv"))))))

(defn -main []
  (let [x (get-map)]
    (spit "magic.json" (json/write-str x))))
