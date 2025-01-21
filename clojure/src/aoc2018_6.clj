(ns aoc2018-6
  (:require
   [clojure.string :as string]
   [utils :refer [read-resource]]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

;; 1. 각 좌표들을 숫자 리스트로 변환한다.
;; 2. 각 좌표들의 최소 및 최대 경계를 계산한다.
;; 3. 각 좌표들로부터 가장 가까운 좌표를 계산한다.
;; 4. 경계에 가까운 좌표들을 무한 영역으로 표시한다.
;; 5. 유한 영역의 크기를 계산한다.
;; 6. 가장 큰 유한 영역의 크기를 계산한다.

(def sample-coordinates ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])

(defn parse-coordinates
  "좌표 문자열 리스트를 [x y] 형태의 숫자 리스트로 변환한다.
   ([1 1] [1 6] [8 3] [3 4] [5 5] [8 9])"
  [coordinates]
  (map (fn [coord]
         (let [[x y] (map #(parse-long %) (string/split coord #", "))]
           [x y]))
       coordinates))

(defn calculate-boundaries
  "좌표의 최소 및 최대 경계를 계산한다.
   {:x-min 1, :x-max 8, :y-min 1, :y-max 9}"
  [coordinates] 
  {:x-min (apply min (map first coordinates))
   :x-max (apply max (map first coordinates))
   :y-min (apply min (map second coordinates))
   :y-max (apply max (map second coordinates))})

(defn manhattan-distance
  "맨해튼 거리 계산"
  [x1 y1 x2 y2]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn closest-coordinate
  "좌표 (x, y)에서 가장 가까운 좌표를 계산한다.
  거리가 동일한 경우 nil 반환."
  [x y coordinates]
  (let [closest (reduce (fn [[min-dist min-coord] [cx cy]]
                          (let [dist (manhattan-distance x y cx cy)]
                            (cond
                              (< dist min-dist) [dist [cx cy]]
                              (= dist min-dist) [min-dist nil]
                              :else [min-dist min-coord])))
                        [Integer/MAX_VALUE nil]
                        coordinates)]
    (second closest)))

(defn infinite-areas
  "경계에 가까운 좌표들을 무한 영역으로 표시한다."
  [boundaries coordinates]
  (let [{:keys [x-min x-max y-min y-max]} boundaries]
    (->> (concat
          (for [x (range x-min (inc x-max))]
            [[x y-min] [x y-max]])
          (for [y (range y-min (inc y-max))]
            [[x-min y] [x-max y]]))
         (mapcat identity)
         (map (fn [[x y]]
                (closest-coordinate x y coordinates)))
         (remove nil?)
         set)))

(defn calculate-closest-coordinates
  "주어진 경계 내 모든 좌표에 대해 가장 가까운 좌표를 계산한다."
  [boundaries coordinates]
  (let [{:keys [x-min x-max y-min y-max]} boundaries]
    (for [x (range x-min (inc x-max))
          y (range y-min (inc y-max))]
      (closest-coordinate x y coordinates))))

(defn calculate-finite-areas
  "유한 영역의 크기를 계산한다."
  [boundaries coordinates infinite-set?] 
  (let [closest-coordinates (calculate-closest-coordinates boundaries coordinates)] 
    (->> closest-coordinates
         (remove infinite-set?)
         frequencies)))

(defn find-largest-finite-area
  "가장 큰 유한 영역의 크기를 계산한다."
  [coordinates] 
  (let [parsed-coodinates (parse-coordinates coordinates)
        boundaries (calculate-boundaries parsed-coodinates)
        infinite-set (infinite-areas boundaries parsed-coodinates)
        finite-areas (calculate-finite-areas boundaries parsed-coodinates infinite-set)]
    (->> finite-areas
         vals
         (apply max))))

(comment 
  #_(find-largest-finite-area sample-coordinates) 
  (time (find-largest-finite-area (read-resource "day6.sample.txt")))
  )

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.
;; 1. 각 좌표들로부터 모든 좌표로의 맨해튼 거리 합계를 계산한다.
;; 2. 거리 합계가 N 미만인 좌표들의 개수를 계산한다.

(defn total-distance
  "주어진 (x, y) 좌표에서 모든 좌표로의 맨해튼 거리 합계를 계산한다."
  [x y coordinates]
  (reduce
   (fn [sum [cx cy]]
     (+ sum (manhattan-distance x y cx cy)))
   0
   coordinates))

(defn safe-region-size
  "안전 영역의 크기를 계산한다."
  [coordinates max-distance]
  (let [parsed-coordinates (parse-coordinates coordinates)
        {:keys [x-min x-max y-min y-max]} (calculate-boundaries parsed-coordinates)]
    (->> (for [x (range x-min (inc x-max))
               y (range y-min (inc y-max))]
           (total-distance x y parsed-coordinates))
         (filter #(< % max-distance))
         count)))

(comment
  #_(safe-region-size sample-coordinates 32)
  (time (safe-region-size (read-resource "day6.sample.txt") 10000))
  )