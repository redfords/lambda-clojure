(pr "Hola mundo")

;; return the third angle of a triangle
(defn tercer-angulo [x y] (- 180 (+ x y)))
(tercer-angulo 100 20)

;; convert a day to seconds
(defn segundos [d h m s] (+ (* d 24 60 60) (* h 60 60) (* m 60) s))
(segundos 1 1 1 1)

;; return the first multiple of 10
(defn sig-mul-10 [i]
  (if (= (mod i 10) 0)
  i
  (recur (inc i))
  ))

;; convert a number to a list of integers
(require '[clojure.math :as math])
(defn digs [result n]
  (if (> n 0)
    (recur (conj result (mod n 10)) (math/floor-div n 10))
    result)
    )
(digs (list) 123)

;; split a list with a formatted string
(defn impr [result x]
(if (empty? x)
result
(recur (conj result (format "Uno para %s, uno para mí" (first x))) (rest x))
))

(defn repartir [x]
(if (empty? x)
(println "Uno para vos, uno para mí")
(impr (list) x))
)
