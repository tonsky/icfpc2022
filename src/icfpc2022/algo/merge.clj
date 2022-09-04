(ns icfpc2022.algo.merge
  (:refer-clojure :exclude [merge])
  (:require
    [clojure.math :as math]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn loop-cols [picture log x y step]
  (if (>= x 400)
    [picture log]
    (let [op [:merge
              (core/block-at picture [(- x step) y])
              (core/block-at picture [x y])]
          picture' (transform/transform picture op)]
      (recur picture' (conj log op) (+ x step) y step))))

(defn loop-rows [picture log y step]
  (if (>= y 400)
    [picture log]
    (let [[picture' log'] (loop-cols picture log (+ step (quot step 2)) y step)]
      (if (< y step)
        ;; first row
        (recur picture' log' (+ y step) step)
        ;; second+ row
        (let [op [:merge
                  (core/block-at picture' [(quot step 2) (- y step)])
                  (core/block-at picture' [(quot step 2) y])]
              picture'' (transform/transform picture' op)]
          (recur picture'' (conj log' op) (+ y step) step))))))

(defn merge [picture]
  (let [w    (long (math/round (math/sqrt (count picture))))
        step (quot 400 w)
        [picture' log'] (loop-rows picture [] (quot step 2) step)]
    [log']))
