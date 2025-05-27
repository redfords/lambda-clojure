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
  (if (= (mod (+ i 1) 10) 0)
  (+ i 1)
  (sig-mul-10 (inc i))
  ))

;; convert a number to a list of integers
(defn digitos [n]
  (if (> n 0)
    (conj (digitos (quot n 10)) (mod n 10))
    ))
(defn digs [n] (reverse (digitos n)))

(defn digs [x]
(map Integer/parseInt (seq (clojure.string/split (str x) #""))))

;; split a list with a formatted string
(defn repartir
  ([] (repartir "vos"))
  ([& more] (map #(str "Uno para " % ", uno para mi") more)))

;; return a list of values with pair index
(defn pares [i]
  (loop [x 0 result []]
  (if (< x (count i))
  (recur (+ x 2) (conj result (nth i x)))
  result)
  )
)
(defn unir [x y]
  (concat (pares x) (pares y)
))

(defn pares [x y]
(map (fn [x] (keep-indexed #(when (even? %1) %2) x)) (conj (list x) y)))

;; check if a number is palindrome
(defn capicua [x]
  (= (seq (str x)) (reverse (str x))))

;; convert 32 bit number to rgba
(defn rgba [x]
  (if (< x 1000)
      [x]
      (conj (rgba (quot x 1000)) (rem x 1000))))
(rgba 4294967296)

;; reverse a number
(defn inv-num [r n]
(if (>= n 10)
    (+ (inv-num (+ (* r 10) (rem n 10)) (quot n 10)))
    (+ (* r 10) (rem n 10))
))
(defn invertir [n] (inv-num 0 n))

(defn invertir[x] (->> x str reverse clojure.string/join Integer/parseInt))

;; return the nth approximation to pi
(defn pi [i c n]
(if (< i (* n 2))
(+ (pi (+ i 2) (- c) n) (/ (- c) i))
0
))
(defn aprox-pi [n] (* 4 (pi 1 -1 n)))

;; return the nth of fibonacci sequence
(defn fibo [x y iter num]
(if (= iter num)
x
(fibo (+ x y) x (inc iter) num)
))
(defn nth-fibo [num] (fibo 1 0 1 num))

;; count the number of digits
(defn cant-dig [n]
  (if (< n 10)
    1
    (+ 1 (cant-dig (quot n 10)))
))

(defn cant-dig [x] (count (str x)))

;; check if a number is power of another
(defn pot? [x y]
  (if (and (> (/ y x) 1) (not= x 1))
  (pot? x (/ y x))
  (= (/ y x) 1)
))

;; replace string adn to arn
(defn adn2arn [x]
(clojure.string/join (replace '{"g" "c", "c" "g", "t" "a", "a" "u"}
(clojure.string/split x #""))))

;; remove element from nested list
(defn lista [x]
(if (not (empty? x))
    (if (list? (first x))
        (conj (lista (first x)) (lista (rest x)))
        (conj (lista (rest x)) (first x))
    )
(list)
))
(lista '(1 2 (3 4) 5 6))

(defn eliminar [lista n]
  (filter #(not= % n)
  (map #(if (list? %) (eliminar % n) %) lista)))

;; get the element in the middle
(defn medio [x] (nth x (/ (count x) 2)))

(defn central [li]
(let [len (count li), num (first li)]
(cond
    (or (= len 1) (< len 3)) num
    true (central (butlast (rest li)))
)))

;; get the last item in a nested list
(defn getLast[lista] (->> lista flatten (filter symbol?) last))

;; order a list os lists by length
(defn ordenar [lista] (sort-by count lista))
(reverse (ordenar lista))
(ordenar '((1 2 3) (1 2) (1 2 3 4) (1)))

;; remove duplicates from a list
(defn existe [x y] (> (count (filter #(= % y) x)) 0))

(defn sin-repetidos [x]
(if (= (count x) 1)
x
    (if (existe (rest x) (first x))
    (conj (sin-repetidos (rest x)))
    (conj (sin-repetidos (rest x)) (first x))
    )
))

;; check if a string is in ISBN-10 format
(defn formato [x]
(replace '{"-" "", " " "", "X" ""} (clojure.string/split x #"")))

(defn suma [x y i]
(if (= y 9)
10
(+ (suma x (inc y) (dec i)) (* (Integer/parseInt (nth x y)) i))
))

(defn isbn-10? [x]
(and (= (mod (suma (formato x) 0 10) 11) 0) (= (nth x 9) '\X)))

(isbn-10? "359821507X")

;; count each adn char in a string
(defn contar [x y]
(if (= (count x) 0)
0
    (if (= (first x) y)
    (+ (contar (rest x) y) 1)
    (contar (rest x) y)
    )
))

(defn contar [x y] (count (filter #(= % y) (seq x))))

(defn contar-adn [x]
(printf "g: %s%n" (contar x '\g))
(printf "c: %s%n" (contar x '\c))
(printf "t: %s%n" (contar x '\t))
(printf "a: %s%n" (contar x '\a))
)

;; get maximum depth of a list
(defn profundidad (lista)
(if (list? lista)
(if (> (+ 1 (profundidad (first lista))) (profundidad (next lista)))
    (+ 1 (profundidad (first lista)))
    (profundidad (next lista)))
    0)
)

(defn profundidad ([lista]
  (last (sort (map #(if (list? %) (inc (profundidad %)) 1) lista)))))
(profundidad '((2 3)(3 ((7))) 5))

;; count the number of appearances of a word in a sentence
(defn contar-palabras [x]
  (frequencies (clojure.string/split (str x) #" ")))

;; check if a word has duplicate letters
(defn letra-repetida [x]
  (> (count (filter #(-> % val (> 1)) (frequencies x))) 0))

;; slice a string
(defn rango [palabra i]
    (let [len (+ (count palabra) 1)]
    (zipmap (range 0 (- len i)) (range i len))))

(defn slice [palabra i]
    (map #(subs palabra (first %) (val %)) (rango palabra i)))
(slice "abcdef" 3)

;; return the acronym of a word
(defn acronimo [x]
(clojure.string/join (map first (clojure.string/split (str x) #" "))))
(acronimo "objeto volador no identificado")

;; check if a number is narcissistic
(defn narcissistic? [x]
(= (int (reduce + (map #(Math/pow % (count (str x))) (digs x)))) x))
