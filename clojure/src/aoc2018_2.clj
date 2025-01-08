(ns aoc2018-2
  ;;(:require [utils :refer [read-resource]])
  )

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(def sample-input ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(defn count-letters [input]
  (let [letter-counts (map (fn [s] (frequencies s)) input)
        two-counts (filter #(some #{2} (vals %)) letter-counts)
        three-counts (filter #(some #{3} (vals %)) letter-counts)]
    (* (count two-counts) (count three-counts))))

(comment
  (println (count-letters sample-input))
)

;;(comment
;;  (println (count-letters (read-resource "day2.sample.txt")))
;;)

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

(def sample-input2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn generate-pairs [input]
  (for [s1 input
        s2 input
        :when (not= s1 s2)]
    [s1 s2]))

(defn find-diff [s1 s2]
  (let [common (filter (fn [[c1 c2]] (= c1 c2)) (map vector s1 s2))]
    (if (= 1 (count (filter (fn [[c1 c2]] (not= c1 c2)) (map vector s1 s2))))
      (apply str (map first common))
      nil)))

(defn find-similar-pair [input]
  (some (fn [[s1 s2]] (find-diff s1 s2)) (generate-pairs input)))

(comment
  (println (find-similar-pair sample-input2))
)

;; #################################
;; ###        Refactoring        ###
;; #################################
