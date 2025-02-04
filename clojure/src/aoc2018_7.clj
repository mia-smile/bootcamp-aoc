(ns aoc2018-7
  (:require
   [clojure.string :as str]
   [utils :refer [read-resource]]))

;; ## 파트 1

;; 스케줄이 주어질 때, 일이 처리되는 순서를 반환하시오.
;; 알파벳 캐릭터 하나로 대표되는 일(work)이 주어지고, 각 일을 처리하기 위해서 선행되어야 하는 일들이 스케줄 형식으로 주어짐.
;; ```
;; Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.
;; ```
;; 위와 같은 입력의 경우 아래 형태의 그래프가 만들어짐.


;; ```
;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----
;; ```

;; 순서는 아래와 같음.
;; - 처음엔 C만 가능함. C에서 시작. 만약 다른 시작점이 존재한다면 알파벳 순서로 진행.
;; - C 다음으로 A와 F가 가능한데, A가 알파벳 우선순위가 높으므로 A로 감.
;; - A 를 완료하면 B, D, F가 가능한데, 역시 알파벳 우선순위가 높은 B가 수행됨.
;; - 남은 D와 F중에서 D가 수행됨
;; - F가 수행됨
;; - E가 수행됨 (E는 B, D, F 모두가 선행되어야 수행될 수 있음)

;; 결과: `CABDFE`

;; 1. 입력 데이터를 파싱해 그래프 형태로 변환한다.
;; 2. 그래프에서 실행 가능한 초기 작업 리스트를 추출한다.
;; 3. 그래프에서 순차적으로 작업을 실행하며, 실행된 작업은 completed 리스트에 추가한다.
;; 4. 작업을 실행한 뒤 그래프에서 해당 작업의 의존성을 제거한다.
;; 5. 새로운 실행 가능 작업을 업데이트하고 알파벳 순서로 정렬한다.
;; 6. 모든 작업이 실행되거나 실행 가능한 작업이 없으면 종료한다.
;; 7. 실행 순서를 문자열로 반환한다.

(def sample-input ["Step C must be finished before step A can begin."
                   "Step C must be finished before step F can begin."
                   "Step A must be finished before step B can begin."
                   "Step A must be finished before step D can begin."
                   "Step B must be finished before step E can begin."
                   "Step D must be finished before step E can begin."
                   "Step F must be finished before step E can begin."])

(defn build-task-dependencies
  "주어진 작업 스케줄 입력을 파싱하여 그래프 형태의 의존성 관계를 생성한다."
  [lines]
  (reduce
   (fn [graph line]
     (let [[_ prereq step] (re-find #"Step (\w) must be finished before step (\w) can begin\." line)]
       (-> graph
           (update step #(conj (or % #{}) prereq))
           (update prereq #(or % #{})))))
   {}
   lines))

(defn find-runnable-tasks
  "현재 실행할 수 있는 작업들을 찾아 알파벳 순으로 정렬하여 반환한다."
  [graph completed-tasks]
  (->> graph
       (filter (fn [[_ prereqs]] (every? completed-tasks prereqs)))
       (map first)
       sort))

(defn remove-task-dependency
  "주어진 작업을 완료 처리하여, 그래프에서 해당 작업을 모든 선행 조건에서 제거한다."
  [completed-task graph]
  (reduce-kv
   (fn [updated-graph task deps]
     (if (deps completed-task)
       (update updated-graph task disj completed-task)
       updated-graph))
   (dissoc graph completed-task)
   graph))

(defn determine-task-order
  "작업 순서를 결정하여 실행 가능한 작업 순서를 리스트로 반환한다."
  [graph]
  (reduce
   (fn [[completed available graph] _]
     (if (empty? available)
       (reduced completed)
       (let [next-task (first available)
             updated-graph (remove-task-dependency next-task graph)
             new-available (find-runnable-tasks updated-graph (conj (set completed) next-task))]
         [(conj completed next-task) new-available updated-graph])))
   [[] (find-runnable-tasks graph #{}) graph]
   (range (count graph))))

(defn schedule-tasks
  "작업 스케줄을 분석하여 실행 순서를 반환한다."
  [input]
  (-> input
      #dbg(build-task-dependencies)
      #dbg(determine-task-order)
      #dbg(first)
      (str/join)))

(comment
  (schedule-tasks (read-resource "day7.sample.txt"))
  (schedule-tasks sample-input))

;; ## 파트 2

;; 파트 1에서는 일을 워커(worker)가 하나였지만, 파트 2는 5명. 즉, 동시에 5개의 일을 처리할 수 있게 됨.
;; 그리고 각각의 일 (A\~Z)은 처리하는데 (60+1\~60+26)의 시간이 걸림. B는 62초, D는 64초, etc.

;; 이 때, 주어진 모든 일을 처리하는데 걸리는 시간을 구하시오.

;; 예)

;; 아래는 파트 1의 예시에서 워커가 2명이 된 케이스이다.
;; ```
;; Second   Worker 1   Worker 2   Done
;;    0        C          .        
;;    1        C          .        
;;    2        C          .        
;;    3        A          F       C
;;    4        B          F       CA
;;    5        B          F       CA
;;    6        D          F       CAB
;;    7        D          F       CAB
;;    8        D          F       CAB
;;    9        D          .       CABF
;;   10        E          .       CABFD
;;   11        E          .       CABFD
;;   12        E          .       CABFD
;;   13        E          .       CABFD
;;   14        E          .       CABFD
;;   15        .          .       CABFDE
;; ```
;; 15초가 걸리므로 답은 15


;; 1. 입력 데이터를 파싱하여 그래프 형태로 변환한다.
;; 2. 그래프에서 실행 가능한 초기 작업 리스트를 추출한다.
;; 3. 병렬 작업 환경에서 각 초마다 가능한 작업을 워커에 할당한다.
;; 4. 작업을 수행하며, 완료된 작업을 반영하고 새로운 실행 가능한 작업을 추가한다.
;; 5. 모든 작업이 완료될 때까지 반복하며, 최종 소요 시간을 반환한다.

(defn task-duration
  "각 작업(A-Z)의 수행 시간을 계산한다. 기본 60초 + (A=1, B=2, ..., Z=26).
   - 문자일 경우: 'A'~'Z' 기준으로 계산
   - 숫자일 경우: 그대로 반환"
  [task]
  (if (number? task)
    task
    (+ 60 (- (int (first task)) (int \A) -1))))

(defn make-workers
  [count]
  (repeat count {:status :idle, :curr-step nil, :proc-time 0}))

(defn idle?
  "워커 상태가 idle 인지 확인"
  [{status :status}]
  (= :idle status))

(defn assign-next-step
  "워커의 상태가 :idle 이면 다음 작업을 할당한다."
  [idle-worker available-step]
  (if available-step
    (-> idle-worker
        (assoc :status :running)
        (assoc :curr-step available-step))
    idle-worker))

(defn assign-work
  "유휴 상태의 워커에게 실행 가능한 작업을 할당한다."
  [available-tasks workers]
  #dbg(let [idle-workers (filter idle? workers)
            tasks-to-assign (take (count idle-workers) available-tasks)
            remaining-tasks (drop (count idle-workers) available-tasks)
            updated-workers (map (fn [w step] (if (idle? w) (assign-next-step w step) w))
                                  workers
                                  (concat tasks-to-assign (repeat nil)))]
        (println updated-workers)
        {:workers updated-workers
         :remaining-tasks remaining-tasks}))

(defn update-task-timers
  "각 워커의 진행 중인 작업 시간을 1초씩 감소하고 완료된 작업을 찾는다."
  [workers task-timers]
  (let [updated-timers (map #(when % (dec %)) task-timers)
        completed-now (keep-indexed #(when (zero? %2) (:curr-step (nth workers %1))) updated-timers)]
    {:updated-timers updated-timers
     :completed-now completed-now}))

(defn remove-completed-tasks
  "완료된 작업을 그래프에서 제거하고, 새로운 실행 가능한 작업을 찾는다."
  [graph completed-tasks completed]
  (let [updated-graph (reduce remove-task-dependency graph completed)
        new-available (find-runnable-tasks updated-graph (set (concat completed completed-tasks)))]
    {:updated-graph updated-graph
     :new-available new-available}))

(defn update-worker-assignments
  "새로운 작업을 할당하고, 작업 타이머를 갱신한다."
  [available-tasks workers]
  (let [{:keys [workers remaining-tasks]} (assign-work available-tasks workers)
        new-task-timers (map #(when (:curr-step %) (task-duration (:curr-step %))) workers)]
    {:workers workers
     :task-timers new-task-timers
     :remaining-tasks remaining-tasks}))

(defn simulate-work
  "각 초마다 워커들의 작업 상태를 업데이트하고, 완료된 작업을 반영한다."
  [{:keys [workers task-timers completed graph elapsed-time]}]
  (let [{:keys [updated-timers completed-now]} (update-task-timers workers task-timers)
        {:keys [updated-graph new-available]} (remove-completed-tasks graph completed completed-now)
        updated-workers (map (fn [w]
                                (if (some #{(:curr-step w)} completed-now)
                                  (assoc w :status :idle :curr-step nil)
                                  w))
                              workers)
        {:keys [workers task-timers]} (update-worker-assignments new-available updated-workers)]
    {:workers workers
     :task-timers task-timers
     :completed (concat completed completed-now)
     :graph updated-graph
     :elapsed-time (inc elapsed-time)}))

(defn calculate-total-time
  "모든 작업을 완료하는 데 걸리는 총 시간을 계산한다."
  [graph num-workers]
  #dbg(let [initial-available (find-runnable-tasks graph #{})
        {:keys [workers remaining-tasks]} (assign-work initial-available (vec (make-workers num-workers )))
        initial-task-timers (map #(when % (task-duration %)) workers)]
    (->> (iterate simulate-work
                  {:workers workers
                   :task-timers initial-task-timers
                   :completed []
                   :graph graph
                   :elapsed-time 0})
         (drop-while #(not-empty (:graph %)))
         first
         :elapsed-time)))

(defn schedule-tasks-with-workers
  "주어진 작업 스케줄을 분석하여 총 작업 시간을 반환한다."
  [input num-workers]
  (-> input
      (build-task-dependencies)
      (calculate-total-time num-workers)))

(comment
  (schedule-tasks-with-workers (read-resource "day7.sample.txt") 5)
  (schedule-tasks-with-workers sample-input 2))
