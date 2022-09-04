(ns icfpc2022.algo.paint
  (:refer-clojure :exclude [merge])
  (:require
    [clojure.math :as math]
    [icfpc2022.algo.merge :as algo.merge]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn paint [problem]
  (let [{:problem/keys [picture bytes]} problem
        [picture' log'] (algo.merge/merge-clusters picture 3)
        log'' (for [[id block] picture'
                    :let [rect        (:rect block)
                          colors      (core/colors bytes rect)
                          color       (core/average colors)
                          op          [:color id color]
                          sim-before  (score/block-similarity bytes block)
                          sim-after   (score/color-similarity bytes color rect)
                          cost        (score/op-cost :color rect)]
                    :when (> (- sim-before sim-after) cost)]
                op)]
    [(concat log' log'')]))