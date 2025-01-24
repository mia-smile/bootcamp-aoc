(ns aoc2018-7
  (:require
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

(defn parse-input
  "입력을 파싱해서 그래프 형태로 정리한다"
  [lines]
  (reduce
   (fn [graph line]
     (let [[_ prereq step] (re-find #"Step (\w) must be finished before step (\w) can begin\." line)]
       (-> graph
           (update step #(conj (or % #{}) prereq))
           (update prereq #(or % #{})))))
   {}
   lines))

(defn- get-available-steps
  "그래프에서 선행 조건이 없는 단계를 찾아서 알파벳 순으로 정렬하여 반환한다"
  [graph completed-steps]
   (->> graph
        (filter (fn [[_ reqs]]
                  (every? completed-steps reqs)))
        (map key)
        sort))

(defn- update-graph
  "그래프에서 특정 단계를 완료 처리하여 해당 단계를 모든 선행 조건에서 제거한다"
  [next-step graph]
  (reduce-kv
   (fn [g k v]
     (if (v next-step)
       (update g k disj next-step)
       g))
   graph
   graph))

(defn- get-new-available
  "다음 단계에서 실행 가능한 새로운 단계들을 계산한다
     - 입력:
       - available: 현재 실행 가능한 단계들
       - completed: 완료된 단계들
       - updated-graph: 업데이트된 그래프
     - 처리:
       1. 선행 조건이 없는 단계들 중 아직 완료되지 않은 단계들을 필터링
       2. 현재 available에서 처리된 단계를 제거
       3. 중복 단계를 제거한 후 정렬
     - 반환: 새로운 available 리스트"
  [available completed updated-graph]
  (->> updated-graph
       (filter (fn [[k v]] (and (empty? v) (not (some #{k} completed)))))
       (map key)
       (concat (remove #{(first available)} available))
       distinct
       sort))

(defn find-order
  "그래프에서 순서를 정렬한다"
  [graph]
  (let [initial-available (get-available-steps graph #{})]
    (loop [{:keys [completed available graph]} {:completed [] :available initial-available :graph graph}]
      (if (empty? available)
        (apply str completed)
        (let [next-step (first available)
              updated-graph (update-graph next-step graph)
              new-available (get-new-available available (conj completed next-step) updated-graph)]
          (recur {:completed (conj completed next-step)
                  :available new-available
                  :graph (dissoc updated-graph next-step)}))))))

(defn assembly-sleigh
  "썰매를 조립한다"
  [input]
  (-> input
      parse-input
      find-order))

(comment
  (assembly-sleigh (read-resource "day7.sample.txt")))

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

(defn task-duration
  "작업 이름(문자)에 기반하여 작업 소요 시간을 계산한다."
  [task base-time]
  (+ base-time (- (int (first task)) (int \A) 1)))

(defn assign-workers
  "가능한 작업들을 작업자들에게 할당한다."
  [workers available base-time]
  (reduce
   (fn [worker-state step]
     (if (some #(nil? (:task %)) worker-state)
       (let [worker (first (filter #(nil? (:task %)) worker-state))]
         (conj (remove #(= worker %) worker-state)
               (assoc worker :task step :remaining-time (+ base-time (task-duration step 0)))))
       worker-state))
   workers
   available))

(defn update-workers
  "작업자의 상태를 업데이트하여 완료된 작업을 반환한다."
  [workers]
  (let [updated-workers
        (map (fn [worker]
               (if (:task worker)
                 (update worker :remaining-time dec)
                 worker))
             workers)]
    {:workers (mapv (fn [worker]
                      (if (and (:task worker) (zero? (:remaining-time worker)))
                        (assoc worker :task nil)
                        worker))
                    updated-workers)
     :completed (mapv :task (filter #(and (:task %) (zero? (:remaining-time %))) updated-workers))}))

(defn simulate-with-workers
  "작업자와 작업 시간을 고려하여 작업을 시뮬레이션한다."
  [graph base-time num-workers]
  (loop [graph graph
         workers (vec (repeat num-workers {:task nil :remaining-time 0}))
         completed []
         available (get-available-steps graph #{})
         time 0]
    (if (and (empty? available) (every? #(nil? (:task %)) workers))
      time
      (let [{:keys [workers completed-steps]} (update-workers workers)
            updated-graph (reduce update-graph graph completed-steps)
            new-available (get-available-steps updated-graph (set completed))
            assigned-workers (assign-workers workers new-available base-time)]
        (recur updated-graph
               assigned-workers
               (concat completed completed-steps)
               (distinct new-available)
               (inc time))))))

(defn calculate-total-time
  "전체 작업을 처리하는 데 걸리는 총 시간을 계산한다."
  [input base-time num-workers]
  (let [graph (parse-input input)]
    (simulate-with-workers graph base-time num-workers)))

(comment
  (calculate-total-time (read-resource "day7.sample.txt") 60 5)
  )
