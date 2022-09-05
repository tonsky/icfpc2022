(ns icfpc2022.algo.rect
  (:require
    [icfpc2022.core :as core]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn rect [^bytes bytes]
  (let [step 50
        sample 5]
    (for [l (range step (- 400 step) step)
          r (range (+ l step) 400 step)
          b (range step (- 400 step) step)
          t (range (+ b step) 400 step)
          :let [colors-in  (for [x (range l r sample)
                                 y (core/rrange b t sample)]
                             (core/get-color bytes x y))
                colors-out (for [x (range 0 400 sample)
                                 y (core/rrange 0 400 sample)
                                 :when (or (< x l) (> x r) (< y b) (> y t))]
                             (core/get-color bytes x y))]
          color-in  (core/color-variants colors-in)
          color-out (core/color-variants colors-out)]
      [[:color "0"     color-out]
       [:pcut  "0"     [l b]]
       [:pcut  "0.2"   [r t]]
       [:color "0.2.0" color-in]])))
  