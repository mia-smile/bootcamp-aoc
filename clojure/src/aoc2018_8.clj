(ns aoc2018-8
  (:require
   [clojure.string :as string]
   [utils :refer [read-resource]]))

;; 파트 1
;; 입력 : 트리 노드의 메타데이터가 주어짐
;; 노드는 자식 노드의 개수와 메타데이터의 개수를 가진 헤더와 0개 이상의 자식 노드, 1개 이상의 메타데이터를 가짐

;; 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2

;; A----------------------------------
;;    B----------- C-----------
;;                     D-----

;; A 노드:
;;   자식 노드 2개 (B, C) 
;;   메타데이터 항목 3개 (1, 1, 2)
;; B 노드:
;;   자식 노드 0개
;;   메타데이터 항목 3개 (10, 11, 12)
;; C 노드:
;;   자식 노드 1개 (D)
;;   메타데이터 항목 1개 (2)
;; D 노드:
;;   자식 노드 0개
;;   메타데이터 항목 1개 (99)

;; 모든 메타데이터 항목의 합

(def sample-input ["2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"])

(defn cast-numbers
  "입력된 문자열 벡터를 숫자 벡터로 변환한다."
  [data]
  (let [split-strings (string/split (first data) #" ")
        parsed-numbers (map #(parse-long %) split-strings)]
    (vec parsed-numbers)))

(defn parse-node
  "트리 구조를 재귀적으로 파싱하여 메타데이터 합과 다음 위치를 반환한다."
  [data idx]
  (let [max-idx (count data)
        sub-end (min max-idx (+ idx 2))
        [child-count metadata-count] (subvec data idx sub-end)
        idx (+ idx 2)]
    (loop [child-idx idx
           child-sum 0
           remaining-children child-count]
      (if (zero? remaining-children)
        (let [metadata (subvec data child-idx (+ child-idx metadata-count))]
          [(+ child-sum (reduce + metadata))
           (+ child-idx metadata-count)])
        (let [[child-sum' next-idx] (parse-node data child-idx)]
          (recur next-idx
                 (+ child-sum child-sum')
                 (dec remaining-children)))))))

(defn sum-metadata
  "메타데이터의 총합을 계산한다."
  [data]
  (first (parse-node data 0)))

(comment
  (sum-metadata (cast-numbers (read-resource "day8.sample.txt")))
  (sum-metadata (cast-numbers sample-input))
  )


;; 파트 2
;; 루트 노드의 값 계산

;; 자식 노드가 없는 경우, 메타데이터 합을 반환
;; 자식 노드가 있는 경우, 메타데이터 항목이 자식 노드의 인덱스를 가리키며, 해당 값의 합을 반환

;; 예시
;; C 노드
;;  메타데이터: 2
;;  자식 노드: D
;;  2는 존재하지 않는 두 번째 자식 노드를 참조하므로 C 노드의 값은 0
;; A 노드
;;  메타데이터: 1 1 2
;;    1: A 노드의 첫 번째 자식 노드(B)를 참조.
;;    2: A 노드의 두 번째 자식 노드(C)를 참조.
;;  자식 노드: B, C
;;  B 노드의 값은 33 (10 + 11 + 12)
;;  C 노드의 값은 0
;;  A 노드의 값은 66 (33 + 0 + 33)

(defn parse-tree
  "트리 구조를 파싱하여 노드와 다음 위치를 반환한다."
  [nums]
  (let [[child-count metadata-count & rest] nums
        children (loop [n child-count
                        acc []
                        remaining rest]
                   (if (zero? n)
                     [acc remaining]
                     (let [[child new-remaining] (parse-tree remaining)]
                       (recur (dec n) (conj acc child) new-remaining))))
        metadata (take metadata-count (second children))
        remaining (drop metadata-count (second children))]
    [{:children (first children), :metadata metadata} remaining]))

(defn node-value
  "노드의 값을 계산한다."
  [{:keys [children metadata]}]
  (if (empty? children)
    (reduce + metadata)
    (reduce + (map #(-> (nth children (dec %) nil)
                        (node-value))
                   (filter #(<= 1 % (count children)) metadata)))))

(defn root-node-value
  "루트 노드의 값을 계산한다."
  [data]
  (let [[tree _] (parse-tree data)]
    (node-value tree)))

(comment
  (root-node-value (cast-numbers (read-resource "day8.sample.txt")))
  (root-node-value (cast-numbers sample-input))
  )
