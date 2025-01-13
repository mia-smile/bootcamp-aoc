(ns aoc2018-4
  (:require [utils :refer [read-resource]]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

;; 1. 로그를 파싱해서 year, month, day, hour, minute, action으로 변환한다.
;; 2. 가드 ID를 추출한다.
;; 3. 가드별로 잠든 구간과 총 잠든 시간을 계산한다.
;; 4. 가장 오랜 시간 잠든 가드의 ID와 데이터를 반환한다.
;; 5. 가장 오랜 시간 잠든 가드가 빈번하게 잠든 시간대를 찾는다.
;; 6. 결과를 출력한다.

(def sample-log ["[1518-11-05 00:55] wakes up"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up" 
                 "[1518-11-01 23:58] Guard #99 begins shift" 
                 "[1518-11-02 00:50] wakes up"
                 "[1518-11-03 00:05] Guard #10 begins shift"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-02 00:40] falls asleep"
                 "[1518-11-01 00:55] wakes up" 
                 "[1518-11-04 00:02] Guard #99 begins shift"
                 "[1518-11-04 00:36] falls asleep"
                 "[1518-11-03 00:24] falls asleep"
                 "[1518-11-03 00:29] wakes up"
                 "[1518-11-04 00:46] wakes up"
                 "[1518-11-05 00:03] Guard #99 begins shift"
                 "[1518-11-05 00:45] falls asleep"
                 "[1518-11-01 00:00] Guard #10 begins shift"])

(def log-format #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)")

(defn parse-line
  "주어진 로그 라인을 파싱하여 해시맵으로 리턴한다.
   ({:year 1518, :month 11, :day 1, :hour 0, :minute 0, :action \"Guard #10 begins shift\"}
    {:year 1518, :month 11, :day 1, :hour 0, :minute 5, :action \"falls asleep\"}
    {:year 1518, :month 11, :day 1, :hour 0, :minute 25, :action \"wakes up\"}
    {:year 1518, :month 11, :day 1, :hour 0, :minute 30, :action \"falls asleep\"}
    {:year 1518, :month 11, :day 1, :hour 0, :minute 55, :action \"wakes up\"}
    {:year 1518, :month 11, :day 1, :hour 23, :minute 58, :action \"Guard #99 begins shift\"}
    {:year 1518, :month 11, :day 2, :hour 0, :minute 40, :action \"falls asleep\"}
    {:year 1518, :month 11, :day 2, :hour 0, :minute 50, :action \"wakes up\"}
    {:year 1518, :month 11, :day 3, :hour 0, :minute 5, :action \"Guard #10 begins shift\"}
    {:year 1518, :month 11, :day 3, :hour 0, :minute 24, :action \"falls asleep\"}
    {:year 1518, :month 11, :day 3, :hour 0, :minute 29, :action \"wakes up\"}
    {:year 1518, :month 11, :day 4, :hour 0, :minute 2, :action \"Guard #99 begins shift\"}
    {:year 1518, :month 11, :day 4, :hour 0, :minute 36, :action \"falls asleep\"}
    {:year 1518, :month 11, :day 4, :hour 0, :minute 46, :action \"wakes up\"}
    {:year 1518, :month 11, :day 5, :hour 0, :minute 3, :action \"Guard #99 begins shift\"}
    {:year 1518, :month 11, :day 5, :hour 0, :minute 45, :action \"falls asleep\"}
    {:year 1518, :month 11, :day 5, :hour 0, :minute 55, :action \"wakes up\"})"
  [line]
  (let [[_ year month day hour minute action] (re-find log-format line)]
    {:year (parse-long year)
     :month (parse-long month)
     :day (parse-long day)
     :hour (parse-long hour)
     :minute (parse-long minute)
     :action action}))

(defn parse-log
  "주어진 로그 리스트를 파싱하여 각 라인을 해시맵 리스트로 변환한다."
  [logs]
  (->> logs
       (map parse-line)))

(defn sort-log
  "주어진 로그 리스트를 정렬한다."
  [logs]
  (sort-by (fn [{:keys [year month day hour minute]}]
             [year month day hour minute])
           logs))

(defn extract-guard-id
  "로그의 :action 필드에서 Guard ID를 추출한다."
  [log]
  (when-let [[_ id] (re-find #"Guard #(\d+)" (:action log))]
    (parse-long id)))

(defn compose-logs-by-guard
  "로그 리스트를 가드별로 묶어서 해시맵으로 리턴한다.
   {10 [{:year 1518, :month 11, :day 1, :hour 0, :minute 0, :action \"Guard #10 begins shift\"}
        {:year 1518, :month 11, :day 1, :hour 0, :minute 5, :action \"falls asleep\"}
        {:year 1518, :month 11, :day 1, :hour 0, :minute 25, :action \"wakes up\"}
        {:year 1518, :month 11, :day 1, :hour 0, :minute 30, :action \"falls asleep\"}
        {:year 1518, :month 11, :day 1, :hour 0, :minute 55, :action \"wakes up\"}
        {:year 1518, :month 11, :day 3, :hour 0, :minute 5, :action \"Guard #10 begins shift\"}
        {:year 1518, :month 11, :day 3, :hour 0, :minute 24, :action \"falls asleep\"}
        {:year 1518, :month 11, :day 3, :hour 0, :minute 29, :action \"wakes up\"}],
    99 [{:year 1518, :month 11, :day 1, :hour 23, :minute 58, :action \"Guard #99 begins shift\"}
        {:year 1518, :month 11, :day 2, :hour 0, :minute 40, :action \"falls asleep\"}
        {:year 1518, :month 11, :day 2, :hour 0, :minute 50, :action \"wakes up\"}
        {:year 1518, :month 11, :day 4, :hour 0, :minute 2, :action \"Guard #99 begins shift\"}
        {:year 1518, :month 11, :day 4, :hour 0, :minute 36, :action \"falls asleep\"}
        {:year 1518, :month 11, :day 4, :hour 0, :minute 46, :action \"wakes up\"}
        {:year 1518, :month 11, :day 5, :hour 0, :minute 3, :action \"Guard #99 begins shift\"}
        {:year 1518, :month 11, :day 5, :hour 0, :minute 45, :action \"falls asleep\"}
        {:year 1518, :month 11, :day 5, :hour 0, :minute 55, :action \"wakes up\"}]}"
  [logs]
  (let [add-guard-ids
         (fn [logs]
           (loop [remaining-logs logs
                  current-guard nil
                  result []]
             (if (empty? remaining-logs)
               result
               (let [log (first remaining-logs)
                     new-guard (extract-guard-id log)
                     guard-id (or new-guard current-guard)
                     updated-log (assoc log :guard-id guard-id)]
                 (recur (rest remaining-logs)
                        guard-id
                        (conj result updated-log))))))]
     (->> logs
          add-guard-ids
          (group-by :guard-id))))

(defn calculate-sleep-data
  "한 명의 가드 로그를 받아 수면 데이터를 계산한다.
  {:periods [[5 25] [30 55]], :total-sleep-time 45, :status :w}"
  [guard-logs]
  (reduce
   (fn [result log]
     (let [action (:action log)
           minute (:minute log)
           status (:status result)]
       (case [status action]
         ;; 깨어있는 상태에서 잠드는 경우
         [:w "falls asleep"]
         (assoc result :start minute :status :f)

         ;; 잠든 상태에서 깨어나는 경우
         [:f "wakes up"]
         (if-let [start (:start result)]
           (-> result
               (update :periods conj [start minute])
               (update :total-sleep-time + (- minute start))
               (assoc :status :w)
               (dissoc :start))
           result)

         ;; 그 외의 경우 상태 유지
         result)))
   {:periods [] :total-sleep-time 0 :status :w}
   guard-logs))

(defn calculate-sleep-data-by-guard
  "가드별로 수면 데이터를 계산한다.
   {10 {:periods [[5 25] [30 55] [24 29]], :total-sleep-time 50, :status :w},
    99 {:periods [[40 50] [36 46] [45 55]], :total-sleep-time 30, :status :w}}"
  [logs-by-guard]
  (into {} (map (fn [[guard-id logs]]
                  [guard-id (calculate-sleep-data logs)]) 
                logs-by-guard)))

(defn find-longest-sleeping-guard
  "가장 오랜 시간 잠든 가드의 ID와 데이터를 반환한다."
  [sleep-data]
  (apply max-key (comp :total-sleep-time val) (into {} sleep-data)))

(defn find-most-frequent-sleep-minute
  "가장 빈번하게 잠든 시간대를 찾는다."
  [{:keys [periods]}]
  (->> periods
       (map (fn [[start end]] (range start end)))
       (apply concat)
       (frequencies)
       (apply max-key val)
       key))

(comment
  (println
   (-> "day4.sample.txt"
       (read-resource)  
;    (-> sample-log
       (parse-log)
       (sort-log)
       (compose-logs-by-guard)
       (calculate-sleep-data-by-guard)
       (find-longest-sleeping-guard)
       ((fn [[guard-id guard-data]]
          (->> (find-most-frequent-sleep-minute guard-data)
               (* guard-id)))))))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(defn default-periods
  "기본 수면 데이터를 생성한다."
  [periods]
  (if (empty? periods)
    [[0 1]]
    periods))

(defn- get-frequences [[guard-id {:keys [periods]}]]
  (let [[minute freq] (->> periods 
                           default-periods
                           (map (fn [[start end]] (range start end)))
                           (apply concat)
                           (frequencies)
                           (apply max-key val))]
    [guard-id {:minute minute, :frequency freq}]))

(defn find-most-frequent-minute-per-guard
  "각 가드별로 가장 자주 잠든 분과 해당 빈도를 반환한다.
   {10 {:minute 24, :frequency 3},
    99 {:minute 45, :frequency 2}}"
  [sleep-data-by-guard]
  (let [sleep-frequencies (map get-frequences
                               sleep-data-by-guard)]
  (into {} sleep-frequencies)))

(defn find-guard-most-frequent-minute
  "모든 가드 중 가장 자주 잠든 분(minute)을 찾고, 그 가드의 ID와 분을 반환한다."
  [most-frequent-minutes]
  (apply max-key (comp :frequency val) most-frequent-minutes))

(comment
  (println
   (-> "day4.sample.txt"
       (read-resource) 
       (parse-log)
       (sort-log)
       (compose-logs-by-guard)
       (calculate-sleep-data-by-guard)
       (find-most-frequent-minute-per-guard)
       (find-guard-most-frequent-minute)
       ((fn [[guard-id {:keys [minute]}]]
          (* guard-id minute))))))