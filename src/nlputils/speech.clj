(ns nlputils.speech)

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