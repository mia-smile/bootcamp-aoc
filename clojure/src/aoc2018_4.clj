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

(def sample-log ["[1518-11-01 00:00] Guard #10 begins shift"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-01 00:55] wakes up"
                 "[1518-11-01 23:58] Guard #99 begins shift"
                 "[1518-11-02 00:40] falls asleep"
                 "[1518-11-02 00:50] wakes up"
                 "[1518-11-03 00:05] Guard #10 begins shift"
                 "[1518-11-03 00:24] falls asleep"
                 "[1518-11-03 00:29] wakes up"
                 "[1518-11-04 00:02] Guard #99 begins shift"
                 "[1518-11-04 00:36] falls asleep"
                 "[1518-11-04 00:46] wakes up"
                 "[1518-11-05 00:03] Guard #99 begins shift"
                 "[1518-11-05 00:45] falls asleep"
                 "[1518-11-05 00:55] wakes up"])

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

(defn parse-and-sort-log
  "주어진 로그 리스트를 파싱하여 각 라인을 해시맵 리스트로 변환한다."
  [log]
  (->> log
       (map parse-line)
       (sort-by (fn [{:keys [year month day hour minute]}]
                    [year month day minute hour]))))

(defn extract-guard-id
  "로그의 :action 필드에서 Guard ID를 추출한다."
  [log]
  (when-let [[_ id] (re-find #"Guard #(\d+)" (:action log))]
    (parse-long id)))

(defn calculate-sleep-data
  "가드별로 잠든 구간과 총 잠든 시간을 계산한다."
  [parsed-logs]
  (reduce
   (fn [result log]
     (let [action (:action log)
           minute (:minute log)
           guard-id (or (extract-guard-id log) (:current-guard result))]
       (cond
         ;; Guard begins shift
         (some? (extract-guard-id log))
         (assoc result :current-guard guard-id)

         ;; falls asleep
         (= action "falls asleep")
         (assoc result :start minute)

         ;; wakes up
         (and (= action "wakes up") (:start result))
         (let [start (:start result)]
           (-> result
               ;; 가드 ID에 대한 초기화
               (update-in [:guards guard-id] #(or % {:periods [] :total-sleep-time 0}))
               (update-in [:guards guard-id :periods] conj [start minute])
               (update-in [:guards guard-id :total-sleep-time] + (- minute start))
               (dissoc :start)))

         ;; 기본 처리
         :else result)))
   {:guards {} :current-guard nil}
   parsed-logs))

(defn find-longest-sleeping-guard
  "가장 오랜 시간 잠든 가드의 ID와 데이터를 반환한다."
  [sleep-data]
  (apply max-key (comp :total-sleep-time val) (:guards sleep-data)))

(defn find-most-frequent-sleep_minute
  "가장 빈번하게 잠든 시간대를 찾는다."
  [guard] 
   (->> (:periods guard)
        (map (fn [[start end]] (range start end)))
        (apply concat)
        frequencies
        (apply max-key val)
        key))

(defn print-result
  "결과를 출력한다."
  [[guard-id guard-data]]
  (let [minute (find-most-frequent-sleep_minute guard-data)]
    (* guard-id minute)))

(comment
  (let [parsed-log (parse-and-sort-log sample-log)
        sleep-data (calculate-sleep-data parsed-log)
        longest-sleeping-guard (find-longest-sleeping-guard sleep-data)]
    (println (print-result longest-sleeping-guard))))

(comment
  (println (-> "day4.sample.txt"
               read-resource
               parse-and-sort-log
               calculate-sleep-data
               find-longest-sleeping-guard
               print-result)))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
