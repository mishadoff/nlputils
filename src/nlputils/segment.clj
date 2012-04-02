(ns nlputils.segment)

;; http://www.mieliestronk.com/wordlist.html
(def DICT (into #{} (re-seq #"[\w']+" (.toLowerCase (slurp "res/words.txt")))))
;; file with human audit results
(def GOLD (map #(re-seq #"[\d\w]+" %) (seq (.split (.toLowerCase (slurp "res/twitter-hashtags.txt")) "\n"))))
;; train file
(def TRAIN-WORDS (re-seq #"[\d\w]+" (.toLowerCase (slurp "res/big.txt"))))

(defn- train-ngram [words n]
  "Trains ngram model with list of words.
   Usage: bigram = (train-ngram words 2)"
  (frequencies (partition n 1 words)))

;; models

;; unigram
(def UNIGRAM-MODEL (train-ngram TRAIN-WORDS 1))
(def UNIGRAM-SIZE (reduce + (vals UNIGRAM-MODEL)))
(def UNIGRAM-VOC-SIZE (count (vals UNIGRAM-MODEL)))

;; bigram
(def BIGRAM-MODEL (train-ngram TRAIN-WORDS 2))
(def BIGRAM-SIZE (reduce + (vals BIGRAM-MODEL)))
(def BIGRAM-VOC-SIZE (count (vals BIGRAM-MODEL)))

(defn- log-probability-unigram [words]
  (reduce + (map #(Math/log (double (/ (inc (get UNIGRAM-MODEL % 0))
                                       (+ UNIGRAM-SIZE
                                          UNIGRAM-VOC-SIZE)))) words)))

(defn- log-probability-bigram [words]
  (if (= 1 (count words)) (log-probability-unigram words)
      (reduce + (map #(Math/log (double (/ (inc (get BIGRAM-MODEL % 0))
                                           (+ BIGRAM-SIZE
                                              BIGRAM-VOC-SIZE)))) (partition 2 1 words)))))

(defn- number-string? [w]
  "Checks if string is number."
  (reduce #(and %1 %2) (map #(Character/isDigit %) (seq w))))

(defn segment-min [string dict]
  "Perform string segmentation by dictionary with minimum matching algorithm."
  (loop [s string idx 1 res []]
    (cond (empty? s) res
          (< (count s) idx) []
          :else (let [curstr (subs s 0 idx)]
                  (if (contains? dict curstr)
                    (recur (subs s idx) 1 (conj res curstr))
                    (recur s (inc idx) res))))))

(defn segment-max [string dict]
  "Perform string segmentation by dictionary with maximum matching algorithm."
  (loop [s string idx (count s) res []]
    (cond (empty? s) res
          (< idx 0) []
          :else (let [curstr (subs s 0 idx)]
                  (if (contains? dict curstr)
                    (recur (subs s idx) (- (count s) idx) (conj res curstr))
                    (recur s (dec idx) res))))))

(defn- split-string-at [string idx]
  [(subs string 0 idx)
   (subs string idx)])

(defn- contains-all? [words dict]
  (reduce #(and %1 %2) (map #(contains? dict %) words)))

(defn- word-candidates-all [string dict]
  (if (empty? string) [""]
      (apply concat (for [i (range 1 (inc (count string)))]
                      (let [[a b] (split-string-at string i)]
                        (map #(cons a %) (word-candidates-all b dict)))))))

(defn segment-bruteforce [string dict]
  "Perform string segmentation by dictionary with bruteforce"
  (let [cands (filter #(contains-all? % dict) (word-candidates-all string dict))
        segm (first (first (reverse (sort-by second (map #(list % (log-probability-unigram %)) cands)))))]
    segm))

(defn- word-candidates-dict [string dict]
  (if (empty? string) [""]
      (apply concat (for [i (range 1 (inc (count string)))
                          :let [ss (subs string 0 i)]
                          :when (or (contains? dict ss)
                                    (and
                                     (number-string? ss)
                                     (if (= i (count string)) true
                                       (not (number-string? (subs string 0 (inc i)))))))]
                      (map #(cons ss %) (word-candidates-dict (subs string i) dict))))))

(defn segment-bruteforce-clever [string dict]
  "Perform string segmentation by dictionary with clever bruteforce"
  (let [cands (word-candidates-dict string dict)
    segm (first (first (reverse (sort-by second (map #(list % (log-probability-bigram %)) cands)))))]
    segm))

;; TODO add viterbi segmentation

(defn calculate-accuracy [segf dict gold-set]
  (loop [[g & gs] gold-set correct 0]
    (if g
      (if (= (segf (first g) dict) (rest g))
        (recur gs (inc correct))
        (recur gs correct))
      (double (/ correct (count gold-set))))))

;; TEST RESULTS WITHOUT number processing
;; min 0.155
;; max 0.75
;; clever unigram 0.765 / 0.795
;; clever bigram 0.77 / 0.8 with numbers