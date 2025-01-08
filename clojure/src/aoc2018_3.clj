(ns aoc2018-3
  (:require [utils :refer [read-resource]] 
            [clojure.set :as set]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(def exp #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(def sample-input ["#1 @ 1,3: 4x4"
                   "#2 @ 3,1: 4x4"
                   "#3 @ 5,5: 2x2"])

(defn get-matrix
  "주어진 입력을 격자로 변환한 좌표 리스트를 반환한다."
  [input]
  (when-let [coord (re-matches exp input)]
      (let [[_ id x y w h] (mapv #(Integer/parseInt %) (rest coord))]
        {id (for [i (range x (+ x w)) ;; recur
                  j (range y (+ y h))] 
              [i,j])})
      ))

(defn get-all-coords
  "주어진 입력을 격자로 변환한 좌표 리스트를 반환한다."
  [input]
  (->> input
    (map get-matrix)
    (mapcat vals)
    (apply concat)))

;; (apply concat (mapcat vals (map get-matrix input)))

(defn count-overlap
  "주어진 격자에서 겹치는 부분의 갯수를 반환한다."
  [input] 
    (let [matrix (get-all-coords input)
          overlap (frequencies matrix)] 
      (count (filter #(> (second %) 1) overlap))))

(comment
    (println (count-overlap (read-resource "day3.sample.txt")))
    )

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn find-independent-id
  "주어진 격자에서 겹치지 않는 ID를 반환한다."
  [input]
  (let [matrix_overlab (frequencies (get-all-coords input))
        single-points (keys (filter #(= (second %) 1) matrix_overlab))
        matrixies (map get-matrix input)]
    (loop [[f & remaining] matrixies]
      (when (some? f)
        (if (set/subset? (set (first (vals f))) (set single-points))
          f
          (recur remaining))))))

(comment
    (println (find-independent-id (read-resource "day3.sample.txt")))
    )