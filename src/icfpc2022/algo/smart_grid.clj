(ns icfpc2022.algo.smart-grid
  (:require
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.algo.merge :as algo.merge]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn pow [x]
  x #_(math/pow x 4))

(defn remap [xs]
  (->> xs
    (mapv
      (fn [x]
        (-> x (/ 400) pow (* 400) long)))
    dedupe))

(defn sim-decrease [bytes prev-color color rect]
  (-
    (score/color-similarity bytes prev-color rect)
    (score/color-similarity bytes color rect)))

(defn log [picture bytes rows cols]
  (let [width  (quot 400 cols)
        height (quot 400 rows)
        new-xs (remap (range 0 (- 400 width) width))]
    (loop [log     []
           picture picture
           xs      new-xs
           ys      (remap (range 0 (- 400 height) height))]
      (cond
        (empty? ys)
        log
        
        (empty? xs)
        (recur log picture new-xs (next ys))
        
        :else
        (let [left       (first xs)
              bottom     (first ys)
              right      (or (fnext xs) 400)
              top        (or (fnext ys) 400)
              color      (core/median (core/colors bytes [left bottom right top]))
              prev-color (core/color-at picture [(quot (+ left right) 2) (quot (+ bottom top) 2)])
              _      (assert (= 1 (count picture)) (str "Expected 1 root block, got " (count picture)))
              root   (parse-long (first (keys picture)))
              ops    (cond
                       (and (= left 0) (= bottom 0))
                       [[:color (str root) color]]
                      
                       (= left 0)
                       [[:ycut  (str root)      bottom]
                        [:color (str root ".1") color]
                        [:merge (str root ".0") (str root ".1")]]
                      
                       (= bottom 0)
                       [[:xcut (str root) left]
                        [:color (str root ".1") color]
                        [:merge (str root ".0") (str root ".1")]]
                      
                       :else
                       [[:ycut  (str root)      bottom]
                        [:xcut  (str root ".1") left]
                        [:color (str root ".1.1") color]
                        [:merge (str root ".1.0") (str root ".1.1")]
                        [:merge (str root ".0")   (str (inc root))]])
              cost (score/cost picture ops)]
          (if (or 
                #_(>= (sim-decrease bytes prev-color color [left bottom 400 400]) cost)
                (>= (sim-decrease bytes prev-color color [left bottom right top]) cost))
            (let [picture' (transform/transform-all picture ops)
                  log'     (into log ops)]
              (recur log' picture' (next xs) ys))
            (recur log picture (next xs) ys)))))))

(defn logs [problem]
  (let [{:problem/keys [picture bytes]} problem
        [merge-log] (if (> (count picture) 1)
                      (algo.merge/merge picture)
                      [])
        picture'    (transform/transform-all picture merge-log)
        root        (first (keys picture'))]
    (for [size (range 10 70)]
      (concat
        merge-log
        [[:color root [255 255 255]]]
        (log picture' bytes size size)))))
