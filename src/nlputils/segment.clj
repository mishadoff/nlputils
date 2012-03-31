(ns nlputils.segment)

;; http://www.mieliestronk.com/wordlist.html
(def DICT (into #{} (re-seq #"[\w']+" (.toLowerCase (slurp "res/words.txt")))))

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
  (filter #(contains-all? % dict) (word-candidates-all string dict)))

(defn segment-clever-bruteforce [string dict]
  )