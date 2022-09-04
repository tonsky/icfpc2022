(ns icfpc2022.algo.merge
  (:refer-clojure :exclude [merge])
  (:require
    [clojure.math :as math]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn loop-cols [picture log x y [l b r t :as rect] step]
  (if (>= x r)
    [picture log]
    (let [b1 (core/block-at picture [(- x step) y])
          b2 (core/block-at picture [x y])]
      (if (and b1 b2)
        (let [op [:merge b1 b2]
              picture' (transform/transform picture op)]
          (recur picture' (conj log op) (+ x step) y rect step))
        (recur picture log (+ x step) y rect step)))))

(defn loop-rows [picture log y [l b r t :as rect] step]
  (if (>= y t)
    [picture log]
    (let [[picture' log'] (loop-cols picture log (+ l step (quot step 2)) y rect step)]
      (if (< (- y step) b)
        ;; first row
        (recur picture' log' (+ y step) rect step)
        ;; second+ row
        (let [b1 (core/block-at picture' [(+ l (quot step 2)) (- y step)])
              b2 (core/block-at picture' [(+ l (quot step 2)) y])]
          (if (and b1 b2)
            (let [op [:merge b1 b2]
                  picture'' (transform/transform picture' op)]
              (recur picture'' (conj log' op) (+ y step) rect step))
            (recur picture' log' (+ y step) rect step)))))))

(defn merge-rect [picture log [l b r t :as rect] step]
  (let [y (+ b (quot step 2))]
    (loop-rows picture log y rect step)))

(defn merge-clusters [picture cluster]
  (let [w     (long (math/round (math/sqrt (count picture))))
        step  (quot 400 w)
        cw    (* step cluster)
        rects (for [y (range 0 400 cw)
                    x (range 0 400  cw)]
                [x y (+ x cw) (+ y cw)])]
    (reduce
      (fn [[picture log] rect]
        (merge-rect picture log rect step))
      [picture []] rects)))

(defn merge [picture]
  (let [w        (long (math/round (math/sqrt (count picture))))
        step     (quot 400 w)
        [_ log'] (merge-rect picture [] [0 0 400 400] step)]
    [log']))
