(ns nlputils.test.speech
  (:use [nlputils.speech])
  (:use [clojure.test]))

(deftest soundex-test []
  (is (= "R163" (soundex "Robert")))
  (is (= "R163" (soundex "Rupert")))
  (is (= "R150" (soundex "Rubin")))
  (is (= "A261" (soundex "Ashcraft")))
  (is (= "A261" (soundex "Ashcroft")))
  (is (= "T522" (soundex "Tymczak")))
  (is (= "P236" (soundex "Pfister")))
  (is (= "M240" (soundex "Mykhailo")))
  (is (= "M240" (soundex "Michael"))))

(deftest soundex-ext-test []
  (is (= "N8030802" (soundex-extended "Nasimov")))
  (is (= "N8030802" (soundex-extended "Nassonov")))
  (is (= "N8030802" (soundex-extended "Nikonov")))
  (is (= "N80308108" (soundex-extended "Nisenbaum")))
  (is (= "N80308108" (soundex-extended "Nissenbaum")))
  (is (= "N804810602" (soundex-extended "Nagmbetov")))
  (is (= "N8050802" (soundex-extended "Nazimov")))
  (is (= "N805802" (soundex-extended "Nezhnov")))
  (is (= "N805802" (soundex-extended "Nozhnov")))
  (is (= "K30503" (soundex-extended "Kozik")))
  (is (= "C30303" (soundex-extended "Cossick"))))

(deftest nysiis-test []
  (is (= "CASPARAVACH" (nysiis "Kaspirovich")))
  (is (= "CATNACAV" (nysiis "Katnikov")))
  (is (= "CATNACAV" (nysiis "Citnikov")))
  (is (= "LANCHANC" (nysiis "Lenchenko")))
  (is (= "STADNACAV" (nysiis "Stadnikov")))
  (is (= "PRADSCY" (nysiis "Prudskiy"))))

;; Run all tests in current namespace
(run-tests)