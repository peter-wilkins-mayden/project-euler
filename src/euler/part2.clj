(ns euler.part2)

; 19
(defn leap-year? [year]
  (cond
    (zero? (mod year 400)) true
    (zero? (mod year 100)) false
    (zero? (mod year 4)) true
    :else false))

(defn pinch-punch []
  (iterate (fn [[year start-day _]]
             [(inc year)
              (if (leap-year? year) (+ start-day 366) (+ start-day 365))
              (->> (if (leap-year? year)
                     '(0 31 60 91 121 152 182 213 244 274 305 335)
                     '(0 31 59 90 120 151 181 212 243 273 304 334))
                   (map #(+ start-day %)))]) [1900 0 nil]))

(->> (take 101 (pinch-punch))
     (mapcat last)
     (filter #(= 5 (mod % 7)))
     (count)
     (+ 1)) ;; losing a month somehow!

; 20
(defn fact [n]
  (if (= n 1)
    1
    (* n (fact (dec n)))))

(->> (fact (bigint 100))
     str
     (map #(Character/digit % 10))
     (apply +))
