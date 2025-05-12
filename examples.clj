(pr "Hola mundo")

;; return the third angle of a triangle
(defn tercer-angulo
  ([a b] (- 180 a b))
([] (println "No se permite sin args"))
([a] (println "Un solo arg"))
([a b &c] (println "Demasiados args"))
)
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

;; return a list of values with pair index
(defn pares [i]
  (loop [x 0 result []]
  (if (< x (count i))
  (recur (+ x 2) (conj result (nth i x))) result))
)
(defn unir [x y]
  (concat (pares x) (pares y)
))

;; check if a number is palindrome
(defn capicua [x]
  (= (seq (str x)) (reverse (str x))))

;; convert 32 bit number to rgba
(defn rgba [x]
  (if (< x 1000)
      [x]
      (conj (rgba (quot x 1000)) (rem x 1000))))
(rgba 4294967296)

;; count the number of digits
(defn cant-dig [n]
  (if (< n 10)
    1
    (+ 1 (cant-dig (quot n 10)))
))

;; check if a numbre is power of another
(defn pot? [x y]
  (if (and (> (/ y x) 1) (not= x 1))
  (pot? x (/ y x))
  (= (/ y x) 1)
))
