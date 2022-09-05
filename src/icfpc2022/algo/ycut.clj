(ns icfpc2022.algo.ycut
  (:require
    [icfpc2022.core :as core]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn ycut [^bytes bytes]
  (let [step   50
        sample 3]
    (for [y1 (range step        (- 400 (* 3 step)) step)
          y2 (range (+ y1 step) (- 400 (* 2 step)) step)
          y3 (range (+ y2 step) (- 400 (* 1 step)) step)
          y4 (range (+ y3 step) (- 400 (* 0 step)) step)
          :let [y5 400
                colors1 (for [x (range 0 400 sample)
                              y (core/rrange 0 y1 sample)]
                          (core/get-color bytes x y))
                colors2 (for [x (range 0 400 sample)
                              y (core/rrange y1 y2 sample)]
                          (core/get-color bytes x y))
                colors3 (for [x (range 0 400 sample)
                              y (core/rrange y2 y3 sample)]
                          (core/get-color bytes x y))
                colors4 (for [x (range 0 400 sample)
                              y (core/rrange y3 y4 sample)]
                          (core/get-color bytes x y))
                colors5 (for [x (range 0 400 sample)
                              y (core/rrange y4 y5 sample)]
                          (core/get-color bytes x y))]
          color1 (core/color-variants colors1)
          color2 (core/color-variants colors2)
          color3 (core/color-variants colors3)
          color4 (core/color-variants colors4)
          color5 (core/color-variants colors5)]
      [[:color "0"         color1]
       [:ycut  "0"         y1]
       [:color "0.1"       color2]
       [:ycut  "0.1"       y2]    
       [:color "0.1.1"     color3]
       [:ycut  "0.1.1"     y3]
       [:color "0.1.1.1"   color4]
       [:ycut  "0.1.1.1"   y4]
       [:color "0.1.1.1.1" color5]])))
