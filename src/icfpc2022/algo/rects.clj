(ns icfpc2022.algo.rects
  (:require
    [icfpc2022.algo.merge :as algo.merge]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(def min-delta
  40)

(defn rand-rect []
  (let [x1 (+ min-delta (rand-int (- 400 min-delta min-delta)))
        x2 (if (> x1 200)
             (+ min-delta (rand-int (- x1 min-delta min-delta)))
             (+ x1 min-delta (rand-int (- 400 x1 min-delta min-delta))))
        y1 (+ min-delta (rand-int (- 400 min-delta min-delta)))
        y2 (if (> y1 200)
             (+ min-delta (rand-int (- y1 min-delta min-delta)))
             (+ y1 min-delta (rand-int (- 400 y1 min-delta min-delta))))]
    [(min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)]))

(defn rects [problem iters threshold]
  (let [{:problem/keys [picture bytes]} problem
        bg (core/most-common
             (core/colors bytes [0 0 400 400]))
        [merge-log] (if (> (count picture) 1)
                      (algo.merge/merge picture)
                      [[]])
        merge-cost  (score/cost picture merge-log)
        picture     (transform/transform-all picture merge-log)
        _           (assert (= 1 (count picture)) (str "Expected 1 root block, got " (count picture)))
        root        (first (keys picture))]
    (loop [log     (conj merge-log
                     [:color root bg])
           picture (transform/transform picture [:color root bg])
           pixels  (core/picture-pixels picture)
           sim     (score/similarity bytes pixels)
           cost    (+ merge-cost
                     (score/op-cost :color [0 0 400 400]))
           iter    0]
      (if (> iter iters)
        [log]
        (let [rect      (rand-rect)
              colors    (core/colors bytes rect)
              color     (min-key
                          #(score/color-similarity bytes % rect)
                          (core/most-common colors)
                          (core/average colors))
              [l b r t] rect
              _         (assert (= 1 (count picture)) (str picture))
              ; _         (println "iter:" iter "rects:" (/ (dec (count log)) 9) "score:" (+ sim cost) "rect:" rect "color:" color)
              root      (parse-long (first (keys picture)))
              ops       [[:pcut  (str root) [l t]]
                         [:pcut  (str root ".1") [r b]]
                         [:color (str root ".1.3") color]
                         [:merge (str root ".1.3") (str root ".1.2")]
                         [:merge (str root ".1.0") (str root ".1.1")]
                         [:merge (str (+ root 1)) (str (+ root 2))]
                         [:merge (str root ".0") (str (+ root 3))]
                         [:merge (str root ".2") (str root ".3")]
                         [:merge (str (+ root 5)) (str (+ root 4))]]
              picture'  (transform/transform-all picture ops)
              pixels'   (core/picture-pixels picture')
              log'      (into log ops)
              sim'      (score/similarity bytes pixels')
              cost'     (+ cost (score/cost picture ops))]
          (if (< (+ sim' cost') (+ sim cost (- threshold)))
            (recur log' picture' pixels' sim' (long cost') 0)
            (recur log  picture  pixels  sim  cost         (inc iter))))))))
