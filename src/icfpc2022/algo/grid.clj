(ns icfpc2022.algo.grid
  (:refer-clojure :exclude [merge])
  (:require
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.algo.merge :as algo.merge]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(def min-delta
  20)

(defn gen-rand [n]
  (let [n (+ 2 (rand-int (- n 1)))]
    (loop [res []]
      (if (>= (count res) n)
        (sort (conj res 400))
        (let [x (+ min-delta (rand-int (- 400 (* 2 min-delta))))]
          (if (every? #(> (abs (- x %)) min-delta) res)
            (recur (conj res x))
            (recur res)))))))

(defn log [bytes ylen xlen xcut ycut]
  (let [ys (gen-rand (dec ylen))
        xs (repeatedly ylen #(gen-rand (dec xlen)))]
    (vec
      (for [[i yprev y] (map vector (range) (cons 0 ys) ys)
            :let [yid   (str/join "." (cons "0" (repeat i "1")))]
            op (concat
                 (when (< y 400)
                   [[ycut yid y]])
                 (for [[j xprev x] (map vector (range) (cons 0 (nth xs i)) (nth xs i))
                       :let  [xid   (str/join "." (concat
                                                    [yid]
                                                    (when (< y 400)
                                                      ["0"])
                                                    (repeat j "1")))
                              rect     [xprev yprev x y]
                              colors   (core/colors bytes rect)
                              color    (min-key
                                         #(score/color-similarity bytes % rect)
                                         (core/most-common colors)
                                         (core/average colors))]
                       op [[:color xid color]
                           (when (< x 400)
                             [xcut xid x])]]
                   op))
            :when op]
        op))))

(defn logs [problem ylen xlen iters]
  (repeatedly iters
    #(log (:problem/bytes problem) ylen xlen :xcut :ycut)))