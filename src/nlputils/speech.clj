(ns nlputils.speech)

;; basic soundex codes
(def SOUNDEX_CODES {\b 1 \f 1 \p 1 \v 1
                    \c 2 \g 2 \j 2 \k 2 \q 2 \s 2 \x 2 \z 2
                    \d 3 \t 3
                    \l 4
                    \m 5 \n 5
                    \r 6})

;; letters ignored by soundex code
(def SOUNDEX_IGNORE_1 #{\a \e \i \o \u \y})
(def SOUNDEX_IGNORE_2 #{\h \w})

(defn soundex [name]
  "Get soundex-code for given name. Only English"
  (let [[a & b] (.toLowerCase name)]
    (.toUpperCase (subs (str (apply str a
           (remove #(= -1 %)
                   (loop [[l & ls] (map #(get SOUNDEX_CODES % -1) (remove #(contains? SOUNDEX_IGNORE_2 %) b))
                          prev (get SOUNDEX_CODES a -1) ls-new []]
                     (if l
                       (if (= l prev)
                         (recur ls prev ls-new)
                         (recur ls l (conj ls-new l))) ls-new)))) "000") 0 4))))

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