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

(defn get-area
  "claim 을 ID와 좌표 정보로 변환한다."
  [claim]
  (when-let [[_ id x y w h] (re-matches exp claim)]
    {:id (parse-long id)
     :coords {:x (parse-long x)
              :y (parse-long y)
              :w (parse-long w)
              :h (parse-long h)}}))

(defn get-squares
  "claim 데이터를 파싱하여 ID와 좌표 정보를 가진 벡터로 반환한다"
  [claims]
  (map get-area claims))

(defn generate-points
  "square 에 포함된 모든 좌표를 생성한다."
  [{:keys [x y w h]}]
  (reduce
   (fn [acc i]
     (reduce (fn [inner-acc j]
               (conj inner-acc [i j]))
             acc
             (range y (+ y h))))
   []
   (range x (+ x w))))

(defn generate-all-points
  "squares 에 포함된 모든 좌표를 생성한다."
  [squares]
  (map (fn [{:keys [id coords]}]
         {:id id
          :points (generate-points coords)})
       squares))

(defn calculate-overlaps
  "겹치는 점을 계산한다."
  [all-points]
  (->> all-points
       (mapcat :points)
       frequencies))

(defn count-overlap
  "겹치는 좌표의 수를 계산한다."
  [overlap-frequencies]
  (count (filter #(> (val %) 1) overlap-frequencies)))

(println
 (->> "day3.sample.txt"
      read-resource
      get-squares
      generate-all-points
      calculate-overlaps
      count-overlap))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn find-independent-area
  "겹치지 않는 ID를 찾음."
  [input]
  (let [parsed (get-squares input)
        all-coords (generate-all-points parsed)
        overlaps (calculate-overlaps all-coords)
        non-overlap-points (->> overlaps
                                (filter #(= (val %) 1))
                                (map key)
                                set)]
    (->> all-coords
         (filter (fn [{:keys [id points]}]
                   (set/subset? (set points) non-overlap-points)))
         first
         :id)))

(comment
    (println (find-independent-area (read-resource "day3.sample.txt")))
    )