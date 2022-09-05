(ns icfpc2022.score
  (:require
    [clojure.math :as math]
    [icfpc2022.core :as core]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn color-similarity [^bytes p1 [r2 g2 b2] [l b r t]]
  (let [l  (long l)
        b  (long b)
        r  (long r)
        t  (long t)
        r2 (long r2)
        g2 (long g2)
        b2 (long b2)] 
    (loop [x   l
           y   (dec t)
           idx (-> (- 399 y) (* 400) (+ x) (* 4))
           res 0.0]
      (cond
        (< y b)
        (math/round (* res 0.005))
       
        (>= x r)
        (let [x' l
              y' (dec y)]
          (recur x' y' (-> (- 399 y') (* 400) (+ x') (* 4)) res))
 
        :else
        (let [r1 (core/byte->long (aget p1 idx))
              g1 (core/byte->long (aget p1 (+ idx 1)))
              b1 (core/byte->long (aget p1 (+ idx 2)))]
          (recur (inc x) y (+ idx 4)
            (+ res
              (math/sqrt
                (+
                  (* (- r1 r2) (- r1 r2))
                  (* (- g1 g2) (- g1 g2))
                  (* (- b1 b2) (- b1 b2)))))))))))

(defn block-similarity [^bytes p1 block]
  (if (core/simple? block)
    (color-similarity p1 (:color block) (:rect block))
    (reduce (fn [acc block]
              (+ acc (block-similarity p1 block))) 0 (:children block))))

(defn similarity
  ([p1 p2]
   (similarity p1 p2 Long/MAX_VALUE))
  ([p1 p2 limit]
   (similarity p1 p2 [0 0 400 400] limit))
  ([^bytes p1 ^bytes p2 [l b r t] limit]
   (let [limit' (/ limit 0.005)
         l      (long l)
         b      (long b)
         r      (long r)
         t      (long t)] 
     (loop [x   l
            y   (dec t)
            idx (-> (- 399 y) (* 400) (+ x) (* 4))
            res 0.0]
       (cond
         (> res limit')
         (math/round (* res 0.005))
       
         (< y b)
         (math/round (* res 0.005))
       
         (>= x r)
         (let [x' l
               y' (dec y)]
           (recur x' y' (-> (- 399 y') (* 400) (+ x') (* 4)) res))
 
         :else
         (let [r1 (core/byte->long (aget p1 idx))
               g1 (core/byte->long (aget p1 (+ idx 1)))
               b1 (core/byte->long (aget p1 (+ idx 2)))
               r2 (core/byte->long (aget p2 idx))
               g2 (core/byte->long (aget p2 (+ idx 1)))
               b2 (core/byte->long (aget p2 (+ idx 2)))]
           (recur (inc x) y (+ idx 4)
             (+ res
               (math/sqrt
                 (+
                   (* (- r1 r2) (- r1 r2))
                   (* (- g1 g2) (- g1 g2))
                   (* (- b1 b2) (- b1 b2))))))))))))

(defn op-cost ^long [type [l b r t]]
  (let [base ({:xcut  2
               :ycut  2
               :pcut  3
               :color 5
               :swap  3
               :merge 1} type)
        area (* (- r l) (- t b))]
    (math/round (/ (* base 400 400) area))))

(defn cost [start-picture log]
  (loop [picture start-picture
         cost    0
         log     log]
    (if-some [op (first log)]
      (let [[type id & _] op
            block         (picture id)
            cost'         (op-cost type (:rect block))
            picture'      (transform/transform picture op)]
        (recur picture' (+ cost cost') (next log)))
      cost)))

(defn score
  ([problem log]
   (score problem log Long/MAX_VALUE))
  ([problem log limit]
   (let [{:problem/keys [bytes picture]} problem
         picture' (transform/transform-all picture log)
         pixels   (with-open [bitmap (core/render-to-bitmap picture')]
                    (.readPixels bitmap))
         cost     (cost picture log)
         sim      (similarity bytes pixels (- limit cost))]
     {:similarity sim
      :cost       cost
      :score      (+ cost sim)})))
