(ns aoc2020-8
  (:require
   [clojure.string :as string]
   [utils :refer [read-resource]]))

;; ## 파트 1
;; 일련의 지시가 입력으로 주어진다.
;; - **acc**는 전역 변수를 증가/감소 시키는 역할을 한다. acc +7은 accumulator를 7 증가 시킨다. accumulator는 0에서 시작한다.
;; - **jmp**는 현재 위치에 기반하여 새로운 지시로 넘어간다. jmp +1은 바로 다음의 지시로 넘어가는 것이고, jmp +2는 바로 다음의 지시는 건너뛰고 그 다음의 지시를 실행하는 것이다.
;; - **nop** 는 아무것도 하지 않는다.
;;   아래는 예시이다.
;; ```
;; nop +0
;; acc +1
;; jmp +4
;; acc +3
;; jmp -3
;; acc -99
;; acc +1
;; jmp -4
;; acc +6
;; ```
;; 위의 예시는 아래의 순서로 실행된다.
;; ```
;; nop +0  | 1
;; acc +1  | 2, 8(!)
;; jmp +4  | 3
;; acc +3  | 6
;; jmp -3  | 7
;; acc -99 |
;; acc +1  | 4
;; jmp -4  | 5
;; acc +6  |
;; ```
;; 이 지시들은 무한히 반복된다.

;; 한 지시가 정확히 **두번 실행되는 시점 바로 전**의 acc의 값을 반환하라.
;; 위의 예시에선 acc +1이 8번째 틱에서 정확히 두번 실행되고, 이 때의 acc의 값은 5이다.

;; 1. 실행할 지시의 index를 set으로 보관한다
;; 2. 지시를 실행한다
;; 3. 이미 실행된 지시가 다시 실행되면, acc의 값을 반환한다. 

(def sample-input ["nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3" "acc -99" "acc +1" "jmp -4" "acc +6"])

(defn nop
  "아무것도 하지 않는다."
  [idx]
  {:idx (inc idx)})

(defn acc
  "변수를 증가/감소 시킨다."
  [accumulator, value, idx]
  {:accumulator (+ accumulator value) :idx (inc idx)})

(defn jmp
  "현재 위치에 기반하여 새로운 지시로 넘어간다."
  [value, idx]
  {:idx (+ idx value)})

(defn parse-instruction
  "지시를 파싱한다."
  [instruction]
  (let [[op value] (string/split instruction #" ")]
    {:op op
     :value (parse-long value)}))

(defn run-instruction
  "지시를 실행한다."
  [accumulator, instruction, index]
  (let [{:keys [op value]} (parse-instruction instruction)]
    (case op
      "nop" (nop index)
      "acc" (acc accumulator value index)
      "jmp" (jmp value index)
      ())))

(defn run-instructions
  "입력을 순차적으로 실행한다."
  [instructions]
  (loop [accumulator 0
         idx 0
         run-indexes? #{}]
    (if (or (run-indexes? idx)
            (> idx (count instructions)))
      accumulator
      (let [instrunction (nth instructions idx)
            result (run-instruction accumulator instrunction idx)
            new-idx (:idx result)
            new-accumulator (get result :accumulator accumulator)]
        (recur new-accumulator
               new-idx
               (conj run-indexes? idx))))))

(comment
  (run-instructions (read-resource "day8-2020.sample.txt"))
  (run-instructions sample-input)
  )

;; ## 파트 2
;; 주어진 지시들 중, 정확히 하나의 지시가 잘못된 것을 알게 되었다.
;; 정확히 하나의 jmp가 nop가 되어야하거나, nop가 jmp가 되면 프로그램은 **종료**된다.

;; ```
;; nop +0  | 1
;; acc +1  | 2
;; jmp +4  | 3
;; acc +3  |
;; jmp -3  |
;; acc -99 |
;; acc +1  | 4
;; nop -4  | 5 ;; 여기!
;; acc +6  | 6
;; ```

;; 위의 예시에서, "여기!" 라고 표기된 곳이 jmp에서 nop로 바뀌면, 지시는 무한히 반복하지 않고 마지막에 6을 반환하며 종료된다.
;; 프로그램이 종료되는 시점의 accumulator의 값을 반환하여라.

;; 1. 이미 실행된 지시가 다시 실행되는 시점을 찾는다.
;; 2. 직전 지시를 jmp/nop로 바꾼다.
;; 3. 끝까지 실행 후 accumulator의 값을 반환한다.

(defn swap-instruction
  "nop <> jmp 변경"
  [instruction]
  (let [{:keys [op value]} (parse-instruction instruction)]
    (cond
      (= op "nop") (str "jmp " value)
      (= op "jmp") (str "nop " value)
      :else instruction)))

(defn modify-instruction-at
  "지정된 index의 nop <> jmp 를 변경한 새로운 지시 리스트 반환"
  [instructions idx]
  (map-indexed (fn [i instr]
                 (if (= i idx)
                   (swap-instruction instr)
                   instr))
               instructions))

(defn run-instructions-until-end
  "입력을 실행하고, 정상 종료 시 accumulator 값을 반환"
  [instructions]
  (loop [accumulator 0
         idx 0
         run-indexes? #{}]
    (cond 
      (>= idx (count instructions)) accumulator 
      (run-indexes? idx) nil
      :else
      (let [instruction (nth instructions idx)
            result (run-instruction accumulator instruction idx)
            new-idx (:idx result)
            new-accumulator (get result :accumulator accumulator)]
        (recur new-accumulator
               new-idx
               (conj run-indexes? idx))))))

(defn find-correct-instruction
  "하나씩 jmp <> nop을 변경하며 정상 종료되는 경우 찾기"
  [instructions]
  (some (fn [idx]
          (let [modified-instructions (modify-instruction-at instructions idx)
                result (run-instructions-until-end modified-instructions)]
            (when result result)))
        (range (count instructions))))

(comment
  (find-correct-instruction (read-resource "day8-2020.sample.txt"))
  (find-correct-instruction sample-input)
  )