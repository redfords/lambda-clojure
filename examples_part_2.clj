;; 1. Convert a number to a list of integers
(defn digitos [n]
  (if (> n 0)
    (conj (digitos (quot n 10)) (mod n 10))
    ))
(defn digs [n] (reverse (digitos n)))

(defn digs [x]
(map Integer/parseInt (seq (clojure.string/split (str x) #""))))

;; 2. Split a list with a formatted string
(defn repartir
  ([] (repartir "vos"))
  ([& more] (map #(str "Uno para " % ", uno para mi") more)))

;; 3. Return a list of values with pair index
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

;; 4. Replace string adn to arn
(defn adn2arn [x]
(clojure.string/join (replace '{"g" "c", "c" "g", "t" "a", "a" "u"}
(clojure.string/split x #""))))

;; 5. Remove element from nested list
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

;; 6. Get the last item in a nested list
(defn getLast[lista] (->> lista flatten (filter symbol?) last))

;; 7. Get the element in the middle
(defn medio [x] (nth x (/ (count x) 2)))

(defn central [li]
(let [len (count li), num (first li)]
(cond
    (or (= len 1) (< len 3)) num
    true (central (butlast (rest li)))
)))

;; 8. Remove duplicates from a list
(defn existe [x y] (> (count (filter #(= % y) x)) 0))

(defn sin-repetidos [x]
(if (= (count x) 1)
x
    (if (existe (rest x) (first x))
    (conj (sin-repetidos (rest x)))
    (conj (sin-repetidos (rest x)) (first x))
    )
))

;; 9. Order a list os lists by length
(defn ordenar [lista] (sort-by count lista))
(reverse (ordenar lista))
(ordenar '((1 2 3) (1 2) (1 2 3 4) (1)))

;; 10. Check if a string is in ISBN-10 format
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

;; 11. Return the upper triangle matrix
(defn calc [x i]
  (if (= (count x) 1)
    (list (first x))
    (conj (calc (rest x) (dec i)) (concat (repeat (- i 1) 0) 
        (keep-indexed #(when (>= (+ %1 1) i) %2) (first x))
))))

(defn mtSup [x] (reverse (calc (reverse x) (count x))))

;; 12. Return the main diagonal of a matrix
(defn calc [x i]
  (if (>= i 0)
  (conj (calc (rest x) (dec i)) (nth (first x) i))
))

(defn diagSup [x] (reverse (calc (reverse x) (- (count x) 1))))

;; 13. Return the transpose of a matrix

(defn calc [x i len]
  (if (<= i len)
  (conj (calc x (inc i) len) (map #(nth % i) x))
))

(defn mTrasp [x] (calc x 0 (- (count x) 1)))

;; 14. Count each adn char in a string
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

;; 15. Count the number of appearances of a word in a sentence
(defn contar-palabras [x]
  (frequencies (clojure.string/split (str x) #" ")))

;; 16. Return the nth prime number
(defn primo [n div]
(cond
    (< n 2) "false"
    (> (* div div) n) "true"
    (= (mod n div) 0) "false"
    true (primo n (inc div))
))
(defn es-primo [n] (primo n 2))

(defn obtener-primos [n x y]
(let [p (es-primo x)]
(cond
    (and (= p "true") (= n y)) x
    (= p "true") (obtener-primos n (inc x) (inc y))
    true (obtener-primos n (inc x) y)
)))

(defn n-primo [n] (obtener-primos n 2 1))

;; 17. Check if a word has duplicate letters
(defn letra-repetida [x]
  (> (count (filter #(-> % val (> 1)) (frequencies x))) 0))

;; 19. Return the triangle type based on the lengths of their sides
(defn tipo-triangulo [a b c]
(cond
    (and (= a b) (= b c)) "equilatero"
    (or (= a b) (= b c) (= a c)) "isosceles"
    true "escaleno"
))

;; 20. Slice a string
(defn rango [palabra i]
    (let [len (+ (count palabra) 1)]
    (zipmap (range 0 (- len i)) (range i len))))

(defn slice [palabra i]
    (map #(subs palabra (first %) (val %)) (rango palabra i)))
(slice "abcdef" 3)

;; 21. Return only the anagrams of a word in a list
(defn anagramas [p a] (filter #(= (sort p) (sort %)) a))

;; 22. Return the acronym of a word
(defn acronimo [x]
(clojure.string/join (map first (clojure.string/split (str x) #" "))))
(acronimo "objeto volador no identificado")

;; 23. Check if a sentence is a panagram
(defn pangrama? [x]
(= (conj (into [] (range 97 123)) 241)
(->> x sin-repetidos (map clojure.string/lower-case) clojure.string/join
  seq (map int) sort (filter #(or (and (>= % 97) (<= % 122)) (= % 241))))
))

;; 24. Check if a number is narcissistic
(defn narcissistic? [x]
(= (int (reduce + (map #(Math/pow % (count (str x))) (digs x)))) x))

;; 25. Get the number of list with more Vs than Fs
(defn filas-max-V [x]
(let [mapa (map #(frequencies %) x)]
(let [resul (map #(- (get % 'V) (get % 'F)) mapa)]
(let [indice (map vector (range 1 (+ (count x) 1)) resul)]
(let [resto (filter #(> (second %) 0) indice)]
(map #(first %) resto)
)))))

(defn mas-V-o-F [x]
(let [mapa (map #(frequencies %) x)]
(let [max (map #(sort-by second > %) mapa)]
(sort-by second > (frequencies (map #(ffirst %) max)))
)))

;; 26. Get a sublist from a list
(defn sublist [lista x y]
(let [i (- x 1)]
(keep-indexed #(when (and (>= %1 i) (< %1 (+ i y) )) %2) lista)))
(sublist '(A B C D E F G) 3 2)

;; 27. Return the index of a sublist
(defn pos-inicial [x y]
(let [i (clojure.string/index-of (clojure.string/join x) y)]
(if i i 0)
))

;; 28. Create a function distribution
(defn distl [x lista] (map #(conj (list %) x) lista))
(distl 'a '(b c d))

;; 29. Return a matrix with the even colunmns and the odd rows
(defn filas [x] (keep-indexed #(when (odd? %1) %2) x))

(defn col-par-fil-imp [x]
(let [col (keep-indexed #(when (even? %1) %2) x)]
(map #(filas %) col)))

;; 30. Return the difference between the sum of the odd rows and the sum of the even rows

(defn dif-sumas[x]
(let [isOdd (keep-indexed #(when (even? %1) %2) x)]
(let [isEven (keep-indexed #(when (odd? %1) %2) x)]
(- (reduce + (map #(reduce + %) isOdd))
    (reduce + (map #(reduce + %) isEven)))
)))

;; 31. Combine two lists into one
(defn intercalar [x y]
(flatten (map #(conj (list (val %)) (first %)) (zipmap x y))))

;; 32. Get maximum depth of a list
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

;; 33. Return a vector with the first 100 prime numbers
(defn obtener-primeros-primos [x y]
(let [p (es-primo x)]
(if (and (= p "true") (= y 100))
    [x]
    (if (= p "true")
        (conj (obtener-100-primos (inc x) (inc y)) x)
        (obtener-100-primos (inc x) y)
    )
)))

(defn cienPrimos [] (obtener-primeros-primos 2 1))

;; 34. Create a vector of n vectors
(defn vectores [n]
(into [] (map #(into [] (range 1 %)) (range 2 (+ n 2)))))

;; 35. Return the undefeated teams in a tournament
(defn get-undefeated [x]
(set (filter #(not (some (fn [u] (= u %)) (map second x)))
    (map first x))))

(defn undefeated []
(get-undefeated (map vector '(1 2 1 4 3 5) '(2 1 3 1 2 2))))

;; 36. Return the teams that lost every match
(defn losers-only [x]
(set (filter #(not (some (fn [u] (= u %)) (map first x))) 
    (map second x))))

(defn losers []
(losers-only (map vector '(5 2 5 4 2 5) '(2 1 3 1 2 2))))

;; 37. Return a list with the winners, losers and ties
(defn results []
(let [x (map vector '(1 2 1 4 3 5) '(2 1 3 1 2 2))]
(equipos x)))

(defn equipos [x]
(let [gano (frequencies (map first x)), perdio (frequencies (map second x))]
(let [eq (set (concat (keys gano) (keys perdio)))]
(let [res (map vector eq (map #(- (or (get gano %) 0) (or (get perdio %) 0)) eq))]
(conj
  (list (map #(first %) (filter #(= (second %) 0) res)))
  (map #(first %) (filter #(< (second %) 0) res))
  (map #(first %) (filter #(> (second %) 0) res))
)))))

;; 38. Check if a sentence is a palindrome
(defn palindromo? [x]
(let [y (filter #(or (and (>= % 97) (<= % 122)) (and (>= % 48) (<= % 57)) (= % 241))
  (map int (seq (clojure.string/join
  (replace '{"á" "a", "é" "e", "í" "i", "ó" "ó", "ú" "u"} 
  (map clojure.string/lower-case x))))))
]
(= y (reverse y))
))

;; 40. Convert integer to Roman
(require '[clojure.math :as math])
(defn romano [x]
(let [
    mil (math/floor-div x 1000),
    cien (math/floor-div (mod x 1000) 100),
    dece (math/floor-div (mod x 100) 10),
    uni (mod x 10)
    ]
))
