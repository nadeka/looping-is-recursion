(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                 (cond (= 1 exp ) acc
                       (= 0 exp) 1
                       :else (recur base (dec exp) (* acc base))))]
    (helper base exp base)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq f]
                 (if (empty? a-seq)
                   f
                   (recur (rest a-seq) (first a-seq))))]
    (helper (rest a-seq) (first a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond (and (empty? seq1) (empty? seq1)) true
                       (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
                       :else false))]
    (if (not (= (count seq2) (count seq1)))
      false
      (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [p pred
         s a-seq
         i 0]
    (cond
     (empty? s) nil
     (p (first s)) i
     :else (recur p (rest s) (inc i)))))

(defn avg [a-seq]
  (loop [s a-seq
         length (count a-seq)
         sum 0
         i 0]
    (if (= length i)
      (/ sum length)
      (recur (rest s) length (+ sum (first s)) (inc i)))))

(defn parity [a-seq]
  (loop [m #{}
         s a-seq
         l (count a-seq)]
    (cond
      (= 0 l) m
      (contains? m (first s)) (recur (disj m (first s)) (rest s) (dec l))
      :else (recur (conj m (first s)) (rest s) (dec l)))))

(defn fast-fibo [n]
  (loop [f1 0
         f2 0
         i 0]
    (cond
      (= n i) (+ f1 f2)
      (>= 1 i) (recur 0 1 (inc i))
      :else (recur f2 (+ f1 f2) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [t #{}
         v []
         s a-seq
         i (count a-seq)]
    (if (or (= 0 i) (contains? t (first s)))
      v
      (recur (conj t (first s)) (conj v (first s)) (rest s) (dec i)))))

