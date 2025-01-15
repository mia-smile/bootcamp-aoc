(ns aoc2018-5
  (:require [utils :refer [read-resource]]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

(def sample-input "dabAcCaCBAcCcaDA")

(defn is-reactable-polymer
  "입력된 두 문자가 같은 종류의 소문자와 대문자인지 확인한다."
  [a b]
  (and (or (and (Character/isUpperCase a) (Character/isLowerCase b))
           (and (Character/isLowerCase a) (Character/isUpperCase b)))
       (= (Character/toLowerCase a) (Character/toLowerCase b))))

(defn react-polymer-seq
  "입력된 시퀀스에서 반응이 일어나는 문자들을 제거한다."
  [s]
  (->> s
       #dbg(reduce (fn [acc c]
                     (if (and (seq acc)
                              (is-reactable-polymer (peek acc) c))
                       (pop acc)
                       (conj acc c)))
                   [])
       (apply str)))

(comment
  (-> "day5.sample.txt"
      (read-resource)
      (first)
      (react-polymer-seq)
      (count))
  )


;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn remove-unit
  "문자열에서 특정 유닛(대문자와 소문자)을 제거한다."
  [s unit]
  (let [unit-chars (set [(Character/t
                          oLowerCase unit) (Character/toUpperCase unit)])]
    (remove unit-chars s)))

(defn shortest-polymer-length
  "주어진 문자열에서 모든 유닛을 하나씩 제거한 후 반응시켰을 때,
   가장 짧은 문자열의 길이를 반환한다."
  [s]
  (let [unique-units (set (map #(Character/toLowerCase %) s))]
    (->> unique-units
         (map (fn [unit]
                (-> s
                    (remove-unit unit)    ;; 특정 유닛 제거
                    (react-polymer-seq)  ;; 반응 처리
                    count)))            ;; 결과 길이 계산
         (apply min))))                  ;; 가장 짧은 길이 반환

(comment
  (shortest-polymer-length sample-input)
  )
