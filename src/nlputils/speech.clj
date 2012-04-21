(ns nlputils.speech
  (:require [clojure.contrib.string :as str]))

;; basic soundex codes
(def SOUNDEX_CODES {\b 1 \f 1 \p 1 \v 1
                    \c 2 \g 2 \j 2 \k 2 \q 2 \s 2 \x 2 \z 2
                    \d 3 \t 3
                    \l 4
                    \m 5 \n 5
                    \r 6})

;; extended soundex codes
(def SOUNDEX_EXT_CODES {\b 1 \p 1
                    \f 2 \v 2
                    \c 3 \k 3 \s 3
                    \g 4 \j 4
                    \q 5 \x 5 \z 5
                    \d 6 \t 6
                    \l 7
                    \m 8 \n 8
                    \r 9})

;; letters ignored by soundex code
(def SOUNDEX_IGNORE_1 #{\a \e \i \o \u \y})
(def SOUNDEX_IGNORE_2 #{\h \w})

;; NYSIIS codes
(def NYSIIS_START {"mac" "mcc", "kn" "n", "k" "c",
                   "ph" "ff", "pf" "ff", "sch" "sss"})

(def NYSIIS_END {"ee" "y", "ie" "y", "dt" "d", "rt" "d"
                 "rd" "d", "nt" "d", "nd" "d"})

(def NYSIIS_INNER {"ev" "af", "e" "a", "i" "a", "o" "a", "u" "a",
                   "q" "g", "z" "s", "m" "n", "kn" "n",
                   "k" "c", "sch" "sss", "ph" "ff"})

(def NYSIIS_SEQ ["sch" "ph" "ev" "kn" "m" "z" "q" "k" "e" "i" "o" "u"])

(defn soundex [name]
  "Get soundex-code for given name using codes. Only English.
   Soundex code has always fixed 4-chars-length code (e.g M576)"
  (let [[a & b] (.toLowerCase name)]
    (.toUpperCase (subs (str (apply str a
           (remove #(= 0 %)
                   (loop [[l & ls] (map #(get SOUNDEX_CODES % 0) (remove #(contains? SOUNDEX_IGNORE_2 %) b))
                          prev (get SOUNDEX_CODES a 0) ls-new []]
                     (if l
                       (if (= l prev)
                         (recur ls prev ls-new)
                         (recur ls l (conj ls-new l))) ls-new)))) "000") 0 4))))

(defn soundex-extended [name]
  "Get extended soundex-code for given name using codes. Only English.
   Extended soundex has non-fixed size of code."
  (let [[a & b] (.toLowerCase name)]
    (.toUpperCase (apply str a
         (loop [[l & ls] (map #(get SOUNDEX_EXT_CODES % 0) (remove #(contains? SOUNDEX_IGNORE_2 %) (.toLowerCase name)))
                prev nil ls-new []]
           (if l
             (if (= l prev)
               (recur ls l ls-new)
               (recur ls l (conj ls-new l))) ls-new))))))

(defn- nysiis-start [name]
  "Transform start of the word due to table. Step 1."
  (loop [[p & prefs] (keys NYSIIS_START)]
    (if p
      (if (.startsWith name p)
        (let [strep (get NYSIIS_START p)]
          (str strep (subs name (count strep))))
        (recur prefs)) name)))

(defn- nysiis-end [name]
  "Transform end of the word due to table. Step 2."
  (loop [[p & prefs] (keys NYSIIS_END)]
    (if p
      (if (.endsWith name p)
        (let [strep (get NYSIIS_END p)]
          (str (subs name 0 (- (count name) (count strep) 1)) strep))
        (recur prefs)) name)))

(defn- nysiis-inner [name]
  "Transform all occurrences but first character due to table. Step 3."
  (str (subs name 0 1)
       (loop [[from & to] NYSIIS_SEQ s (subs name 1)]
         (if from
           (recur to (str/replace-str from (get NYSIIS_INNER from) s)) s))))

(defn- vowel? [char]
  "Predicate that detects if character is vowel."
  (contains? #{\a \e \i \o \u} char))

(defn- nysiis-vowels [name]
  "Remove all [h] after vowel. Replace all [w->a] after vowel. Step 4."
  (loop [[l & ls] (partition 2 1 name) news []]
    (if l
      (let [[a b] l]
        (if (vowel? a)
          (cond (= \h b) (recur ls news)
                (= \w b) (recur ls (conj news \a))
                :else (recur ls (conj news b))) (recur ls (conj news b))))
      (apply str (subs name 0 1) news))))

(defn- nysiis-laststep [name]
  "Different small modifications. Step 5."
  (cond (.endsWith name "as") (subs name 0 (- (count name) 2))
        (.endsWith name "s") (subs name 0 (- (count name) 1))
        (.endsWith name "a") (subs name 0 (- (count name) 1))
        (.endsWith name "ay") (str (subs name 0 (- (count name) 2)) "y")
        :else name))
        

(defn nysiis [name]
  "Get code for given name using NYSIIS algorithm."
  (.toUpperCase
   (loop [s (.toLowerCase name) [f & fs]
          [nysiis-start nysiis-end nysiis-inner nysiis-vowels nysiis-laststep]]
     (if f (recur (f s) fs) s))))

(defn similar? [name1 name2 strategy]
  "Check if two names are similar using strategy.
   Strategy is just function name used for similarity (e.g soundex)"
  (= (strategy name1) (strategy name2)))


;; TODO unit tests
;; (soundex "Rubin") => R150
;; (soundex "Robert") => R163
;; (soundex "Rupert") => R163
;; (soundex "Ashcraft") => A261
;; (soundex "Ashcroft") => A261
;; (soundex "Tymczak") => T522
;; (soundex "Pfister") => P236