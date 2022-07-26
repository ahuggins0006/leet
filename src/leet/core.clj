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


;; Container with Most Water

;; You are given an integer array height of length n. There are n vertical lines drawn
;; such that the two endpoints of the ith line are (i, 0) and (i, height[i]).

;; Find two lines that together with the x-axis form a container, such that the container contains the most water.
;;
;; Return the maximum amount of water a container can store.
;;
;; Notice that you may not slant the container.

(let [h [1,8,6,2,5,4,8,3,7]]
  (loop [h h
         i 0]
    (if (>= (first h) (second h))
      i
      (recur (drop 2 h) (inc i)))))

(defn find-first-max
  [h]
  (loop [h h
         i 0]
    (if (>= (first h) (second h))
      i
      (recur (drop 2 h) (inc i)))))

(nth [1,8,6,2,5,4,8,3,7] (find-first-max [1,8,6,2,5,4,8,3,7]))
(find-first-max (reverse [1,8,6,2,5,4,8,3,7]))
(count [1,8,6,2,5,4,8,3,7])

(defn max-area
  [h]
  (let [left (find-first-max h)
        leftv (nth h left)
        right (- (dec (count h)) (find-first-max (reverse h)))
        rightv (nth h right)]
    (* (- right left) (apply min [leftv rightv]))))

(max-area [1,8,6,2,5,4,8,3,7])
;; => 49
(max-area [1,1])
;; => 1


;; Given an integer, convert it to a roman numeral.


(def numerals
  {
   1 "I"
   4 "IV"
   5 "V"
   9 "IX"
   10 "X"
   40 "XL"
   50 "L"
   90 "XC"
   100 "C"
   400 "CD"
   500 "D"
   900 "CM"
   1000 "M"
   }
  )

(numerals 1000)

(defn int-to-roman
  ([x] (int-to-roman x []))
  ([x v]
   (cond (>= x 1000) (recur (- x 1000) (conj v (numerals 1000)))
         (>= x 900)  (recur (- x 900) (conj v (numerals 900)))
         (>= x 500)  (recur (- x 500) (conj v (numerals 500)))
         (>= x 400)  (recur (- x 400) (conj v (numerals 400)))
         (>= x 100)  (recur (- x 100) (conj v (numerals 100)))
         (>= x 90)   (recur (- x 90) (conj v (numerals 90)))
         (>= x 50)   (recur (- x 50) (conj v (numerals 50)))
         (>= x 40)   (recur (- x 40) (conj v (numerals 40)))
         (>= x 10)   (recur (- x 10) (conj v (numerals 10)))
         (>= x 9)    (recur (- x 9) (conj v (numerals 9)))
         (>= x 5)    (recur (- x 5) (conj v (numerals 5)))
         (>= x 4)    (recur (- x 4) (conj v (numerals 4)))
         (>= x 1)    (recur (- x 1) (conj v (numerals 1)))
         (= x 0)     (apply str v))))

(int-to-roman 4)
;; => "IV"

(int-to-roman 58)
;; => "LVIII"

(int-to-roman 1994)
;; => "MCMXCIV"


;; Longest Common Prefix
;; Write a function to find the longest common prefix string amongst an array of strings
;; If there is no common prefix, return an empty string.

(def strs ["flowser" "flows" "flogsht"])

;; only partial solution
;; finds substrings as well as other common chars
(comment (apply str (keys (filter #(= (val %) (count strs)) (frequencies (apply interleave strs))))))

;; working out a way to ignore non substring chars
(def t (frequencies (apply interleave strs)))
;; => {\f 3, \l 3, \o 2, \i 1, \w 2, \g 1, \s 3}

(first (partition-by #(= (val %) (count strs)) t))


(defn longest-common-prefix
  [strs]
  (->> strs
       (apply interleave)
       frequencies
       (partition-by #(= (val %) (count strs)))
       first
       (map key)
       (apply str)))

(longest-common-prefix strs)
;; => "flo"


;; 3Sum

;; Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]]
;; such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.

;; Notice that the solution set must not contain duplicate triplets.

(def nums (map-indexed vector [-1 0 1 2 -1 -4]))
nums


(apply distinct? [0 2 2])
(distinct (filter #(apply distinct? (map first %)) (map #(sort-by first %) (filter #(= (apply + (map second %)) 0) (for [x nums
                                                                                                                                                                   y nums
                                                                                                                                                                   z nums] [x y z])))))
;; => (([0 -1] [1 0] [2 1]) ([0 -1] [3 2] [4 -1]) ([1 0] [2 1] [4 -1]))

(map #(map second %) '(([0 -1] [1 0] [2 1]) ([0 -1] [3 2] [4 -1]) ([1 0] [2 1] [4 -1])))
;; => ((-1 0 1) (-1 2 -1) (0 1 -1))

(map #(sort %) '(([0 -1] [1 0] [2 1]) ([0 -1] [6 2] [4 -1]) ([1 0] [2 1] [4 -1])))

(filter #(= (apply + (map second %)) 0) (for [x nums
                                                               y nums
                                                               z nums] [x y z]))

(sort-by first '([0 -1] [1 0] [2 1]))

(->> (for [x nums
           y nums
           z nums] [x y z])
     (filter #(and (apply distinct? (map first %))(= (apply + (map second %)) 0)))
     (map #(sort %))
     distinct
     (map #(map second %))
     (map #(sort %))
     set
     )

;; into a named function
(defn three-sum
  [nums]
  (let [n (map-indexed vector nums)]
    (->> (for [x n
               y n
               z n] [x y z])
         (filter #(and (apply distinct? (map first %))(= (apply + (map second %)) 0)))
         (map #(sort %))
         distinct
         (map #(map second %))
         (map #(sort %))
         set
         )))

(three-sum [-1 0 1 2 -1 -4])
;; => #{(-1 0 1) (-1 -1 2)}

(three-sum  [0,1,1])
;; => #{}

(three-sum [0 0 0])
;; => #{(0 0 0)}
