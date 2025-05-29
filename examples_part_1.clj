;; 1. Return the third angle of a triangle
(defn tercer-angulo
  ([a b] (- 180 a b))
([] (println "No se permite sin args"))
([a] (println "Un solo arg"))
([a b &c] (println "Demasiados args"))
)
(tercer-angulo 100 20)

;; 2. Convert a day to seconds
(defn segundos [d h m s] (+ (* d 24 60 60) (* h 60 60) (* m 60) s))
(segundos 1 1 1 1)

;; 3. Return the first multiple of 10
(defn sig-mul-10 [i]
  (if (= (mod (+ i 1) 10) 0)
  (+ i 1)
  (sig-mul-10 (inc i))
  ))

;; 4. Convert 32 bit number to rgba
(defn rgba [x]
  (if (< x 1000)
      [x]
      (conj (rgba (quot x 1000)) (rem x 1000))))
(rgba 4294967296)

;; 5. Check if a number is palindrome
(defn capicua [x]
  (= (seq (str x)) (reverse (str x))))

;; 6. Return the nth approximation to pi
(defn pi [i c n]
  (if (< i (* n 2))
    (+ (pi (+ i 2) (- c) n) (/ (- c) i))
    0
  ))
(defn aprox-pi [n] (* 4 (pi 1 -1 n)))

;; 7. Reverse a number
(defn inv-num [r n]
(if (>= n 10)
    (+ (inv-num (+ (* r 10) (rem n 10)) (quot n 10)))
    (+ (* r 10) (rem n 10))
))
(defn invertir [n] (inv-num 0 n))

(defn invertir[x] (->> x str reverse clojure.string/join Integer/parseInt))

;; 8. Return the nth of fibonacci sequence
(defn fibo [x y iter num]
  (if (= iter num)
    x
    (fibo (+ x y) x (inc iter) num)
  ))
(defn nth-fibo [num] (fibo 1 0 1 num))

;; 9. Count the number of digits
(defn cant-dig [n]
  (if (< n 10)
    1
    (+ 1 (cant-dig (quot n 10)))
))

(defn cant-dig [x] (count (str x)))

;; 10. Check if a number is power of another
(defn pot? [x y]
  (if (and (> (/ y x) 1) (not= x 1))
  (pot? x (/ y x))
  (= (/ y x) 1)
))
