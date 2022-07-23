(ns leet.core
  (:require [clojure.string :as s])
  (:gen-class))

;TWO SUM
;Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
;
;You may assume that each input would have exactly one solution, and you may not use the same element twice.
;
;You can return the answer in any order.

(def nums [3 3])

;; add up all combinations of pairs
;; filter out target sum
;; return indecies
(second (first (filter #(= 6 (first
                              %))(for [x nums
                                       y (rest nums)]
                                   [(+ x y) (map #(.indexOf nums %) [x y])]))))

(defn two-sum [nums target]
  (let [solution (->> (for [x nums
                            y (rest nums)]
                        [(+ x y) (map #(.indexOf nums %) [x y])]) ;; add up all combinations of pairs along with their indecies
                      (filter #(= target (first %)))              ;; filter out target sum
                      first                                       ;; get inner vector
                      second)]
    ;; logic to deal with soltions like (two-sum [3 3] 6) => (0 0)
    (if (apply distinct? solution)
      solution
      (map first (filter #(= (second %) (nth nums (first solution))) (map-indexed vector nums))))))

(apply distinct? '(0 0))
(two-sum nums 5)
(two-sum [3 3] 6)
(two-sum [2 3 4] 6)

(map first (filter #(=(second %) 3) (map-indexed vector nums)))

;;Given a string s, find the length of the longest substring without repeating characters.
(def s "abcabcbb")
(count s)
(map #(apply str %) (partition 3 s))

(apply concat (loop [a 1
                     sols []]
                (if (<= a (.length s))
                  (do
                    (println (partition a s))
                    (recur (inc a) (conj sols (partition a s)))
                    )
                  sols
                  )

                ))

;; filter only distinct substrings
;; filter longest distinct substring
;; return length of longest substring
(count (last (filter #(apply distinct? %)(apply concat
                                                (loop [a 1
                                                       sols []]
                                                  (if (<= a (.length s))
                                                    (do
                                                      (println (partition a s))
                                                      (recur (inc a) (conj sols (partition a s)))
                                                      )
                                                    sols
                                                    )

                                                  )))))
;; convert to function

(defn longest-substring
  [s]
  (->> (loop [a 1
              sols []]
         (if (<= a (.length s))
           (recur (inc a) (conj sols (partition a s)))
           sols))
       (apply concat)
       (filter #(apply distinct? %))
       last
       count))

(longest-substring s)

(longest-substring "bbbbb")
(longest-substring "pwwkew")


;; Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.

;; The overall run time complexity should be O(log (m+n)).

(def nums1 [1 3])
(def nums2 [2])

(def nums3 [1 2])
(def nums4 [3 4])
(def nums5 (take 100 (range 0 100)))
(def nums6 (take 1000 (range 0 1000)))

(defn calc-median
  [a]
  (let [s      (count a)
        sorted (sort a)
        mid-pt (Math/floor (/ s 2))]
    (cond
      (even? s) (/ (+ (nth sorted mid-pt) (nth sorted (dec mid-pt))) 2.0)
      (odd? s)  (nth sorted mid-pt))))

(calc-median (concat nums1 nums2))
(calc-median (concat nums3 nums4))
(calc-median (concat nums5 nums6))


; Longest Palindromic Substring
; Given a string s, return the longest palindromic substring in s.
(subs "Clojure" 0 1)
(subs "Clojure" 0 2)
(->> (for [a (range (inc (count "babad")))
           b (range (inc (count "babad")))]
       (cond
         (<= a b)(subs "babad" a b)
         (<= b a)(subs "babad" b a)
         ))
     (into #{})
     (filter #(= % (apply str (reverse %))))
     (filter #(> (count %) 1))
     first
     )

(defn palindrome-substring [s]
  (let [c (range (inc (count s)))]
    (->> (for [a c
               b c]
           (cond
             (<= a b) (subs s a b)
             (<= b a) (subs s b a)))
         (into #{})
         (filter #(= % (apply str (reverse %))))
         (filter #(> (count %) 1))
         first)))

(palindrome-substring "babad" )


; The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of
; rows like this: (you may want to display this pattern in a fixed font for better legibility)
; P   A   H   N
; A P L S I I G
; Y   I   R

; And then read line by line: "PAHNAPLSIIGYIR"
; Write the code that will take a string and make this conversion given a number of rows:

(def t "PAYPALISHIRING")

(def matrix [[]
             []
             []
             ])

(assoc-in (assoc-in matrix [0 0] 1) [0 1] 2)

(assoc-in matrix [0 1] 1)

(defn init-matrix
  [m s]
  (vec (for [r (range m)]
         (into [] (take (count s) (repeat ""))))))

(defn something
  [s r]
  (loop [i 0
         j 0
         reverse? false
         m (vec (for [rows (range r)]
                  (into [] (take (count s) (repeat "")))))
         s s]
    (cond (= r 1) s
          (empty? s) (apply str (flatten m)) ;; base case
          (> i (dec r)) (recur (- r 2) (inc j) true m s) ;; bottom
          (and (> i 0) reverse?) (recur (dec i) (inc j) true (assoc-in m [i j] (first s)) (rest s))
          (and (zero? i) reverse?) (recur i j false m s) ;; top
          (not reverse?) (recur (inc i) j false (assoc-in m [i j] (first s)) (rest s)))))

(something t 4)
(something t 3)
(something t 1)
(+ 1 1)

;; Given a signed 32-bit integer x, return x with its digits reversed. If reversing
;; x causes the value to go outside the signed 32-bit integer range [-231, 231 - 1], then return 0.

(def x 120)

(reverse (.toString x))

(Integer/parseInt "123")

(->> x
     str
     reverse
     (apply str)
     Integer/parseInt
     )

(defn reverse-integer
  [x]
  (->> x
       str
       reverse
       (apply str)
       Integer/parseInt))

(reverse-integer x)

;; Regular Expression Matching
;; Given an input string s and a pattern p, implement regular expression matching with support for '.' and '*' where:

;; '.' Matches any single character.
;; '*' Matches zero or more of the preceding element.
;; The matching should cover the entire input string (not partial).

(some #(= % \*) "aa*")
(some #(or (= % \.)(= % \*)) (str "aa" "a."))

(str (first "aa") (first "a"))

(let [s "aa"
      p "b"]
  (cond (= p ".*") true
        (or (empty? s) (empty? p)) false
        :default (loop [s s
                        p p]
                        (if (or (not= (first s) (first p) (= (first p) ".")))
                          false
                          (recur (rest s) (rest p))
                          ))
        )
  )

(first "aa")
(apply str (rest "a*"))

(defn matches?
  [s p]
  (loop [s s
         p p]
    (cond (= p ".*")                                    true
          (and (empty? s) (empty? p))                   true
          (= (first p) \*)                              true
          (or (= (first s) (first p)) (= (first p) \.)) (recur (apply str (rest s)) (apply str (rest p)))
          :default                                      false)))

(matches? "aa" "a")
(matches? "aa" "a*")
(matches? "ab" ".*")
