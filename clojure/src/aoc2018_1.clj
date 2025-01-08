(ns aoc2018-1)

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(def sample-input [+10 -2 -5 +1])

(defn sum-input [input]
  (reduce + input))

(comment
  (println (sum-input sample-input))
)

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(def sample-input2 [+3 +3 +4 -2 -4])

;; (defn find-first-duplicate [input]
;;   (loop [sum 0
;;          seen? #{}
;;          next (cycle input)]
;;     (let [new-sum (+ sum (first next))]
;;       (if (seen? new-sum)
;;         new-sum
;;         (recur new-sum (conj seen? new-sum) (rest next))))))

;; (comment
;;   (println (find-first-duplicate sample-input2))
;; )

(defn parse-input [input]
  (cycle input))

(defn process-data [input]
  (reduce
   (fn [[seen? sum] n]
     (let [new-sum (+ sum n)]
       (if (seen? new-sum)
         (reduced new-sum)
         [(conj seen? new-sum) new-sum])))
   [#{} 0]
   input))

(defn print-result [result]
  (println "First duplicate sum:" result))

(comment
  (let [parsed-input (parse-input sample-input2)
      result (process-data parsed-input)]
  (print-result result))
  )