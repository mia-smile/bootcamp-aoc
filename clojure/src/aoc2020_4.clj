(ns aoc2020-4
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [utils :refer [read-resource]]))

;; ## 파트 1
;; 여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.
;; - byr (Birth Year)
;; - iyr (Issue Year)
;; - eyr (Expiration Year)
;; - hgt (Height)
;; - hcl (Hair Color)
;; - ecl (Eye Color)
;; - pid (Passport ID)
;; - cid (Country ID)

;; 파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다. 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.

;; ```
;; ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
;; byr:1937 iyr:2017 cid:147 hgt:183cm

;; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
;; hcl:#cfa07d byr:1929

;; hcl:#ae17e1 iyr:2013
;; eyr:2024
;; ecl:brn pid:760753108 byr:1931
;; hgt:179cm

;; hcl:#cfa07d eyr:2025 pid:166559648
;; iyr:2011 ecl:brn hgt:59in
;; ```

;; - 첫번째는 유효한 여권이다. 8개의 필드가 전부 존재한다.
;; - 두번째는 유효하지 않다. hgt가 없기 때문.
;; - 세번째는 cid가 없지만, ** cid는 없어도 되는 ** 것으로 간주한다. 그래서 유효하다.
;; - 네번째는 cid와 byr이 없다. byr은 반드시 있어야하는 필드이므로 유효하지 않다.

(def sample-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(s/def ::byr pos-int?)
(s/def ::iyr pos-int?)
(s/def ::eyr pos-int?)
(s/def ::hgt string?)
(s/def ::hcl string?)
(s/def ::ecl string?)
(s/def ::pid pos-int?)
(s/def ::cid pos-int?)

(s/def ::passport
  (s/and (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] :opt [::cid])
         #(> (::iyr %) (::byr %))
         #(> (::eyr %) (::iyr %))))

(defn split-each-passport-info
  "합쳐진 여권 정보를 blank line 으로 구분하여 각 여권 정보 시퀀스로 반환한다"
  [passport-infos]
  (string/split passport-infos #"\n\n"))

(defn parse-passport-info
  "개별 여권 정보를 파싱하여 맵으로 반환한다"
  [passport-info]
  (->> (string/split passport-info #"\s+")
       (map #(string/split % #":"))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn coerce-passport
  "여권 정보의 타입을 스펙에 맞게 변환한다."
  [passport-info]
  (-> passport-info
      (update :byr #(when-some [v %] (parse-long v)))
      (update :iyr #(when-some [v %] (parse-long v)))
      (update :eyr #(when-some [v %] (parse-long v)))
      (update :pid #(when-some [v %] (parse-long v)))
      (update :cid #(when-some [v %] (parse-long v)))))

(defn validate-passport
  "각 여권 정보를 검증하여 유효한 여권인지 판단한다"
  [passport]
  (let [coerced (coerce-passport passport)]
    #dbg(s/explain ::passport coerced)))

(defn count-valid-passports [input]
  (->> (split-each-passport-info input)
       (map parse-passport-info)
       #dbg(map validate-passport)
       #_#_#dbg(filter true?)
       count))

(comment
  (count-valid-passports sample-input)
  )

;; ## 파트 2
;; 파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.
;; - byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
;; - iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
;; - eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
;; - hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
;; - cm의 경우, 숫자는 최소 150 & 최대 193.
;; - in의 경우, 숫자는 최소 59 & 최대 76.
;; - hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
;; - ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
;; - pid (Passport ID) - 처음 0을 포함하는 9자리 숫자.
;; - cid (Country ID) - 없어도 됨.

;; 아래는 예시들이다.
;; ```
;; byr valid:   2002
;; byr invalid: 2003

;; hgt valid:   60in
;; hgt valid:   190cm
;; hgt invalid: 190in
;; hgt invalid: 190

;; hcl valid:   #123abc
;; hcl invalid: #123abz
;; hcl invalid: 123abc

;; ecl valid:   brn
;; ecl invalid: wat

;; pid valid:   000000001
;; pid invalid: 0123456789
;; ```
;; 모든 필드의 기준에 맞는 여권의 수를 반환하여라.
