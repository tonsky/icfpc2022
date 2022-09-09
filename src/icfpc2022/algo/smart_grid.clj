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

(def grid-threshold
  10)

(def delta-threshold
  1)

(defn remove-close [threshold xs]
  (loop [xs  xs
         res []]
    (if
      (empty? xs)
      res
      
      (let [[i x] (first xs)]
        (if (every? (fn [[j _]] (> (abs (- i j)) threshold)) res)
          (recur (next xs) (conj res [i x]))
          (recur (next xs) res))))))

(defn detect-cols [bytes]
  (let [cols   (for [x (range 0 400)]
                 (core/median
                   (core/colors bytes [x 0 (inc x) 400])))
        deltas (map (fn [i prev color]
                      [i (core/dist prev color)])
                 (range 1 400) cols (next cols))]
    (->> (sort-by second deltas)
      (reverse)
      (take-while (fn [[i d]] (> d delta-threshold)))
      (remove-close grid-threshold)
      (map first)
      (take 20)
      (sort)
      (cons 0))))

(defn detect-rows [bytes]
  (let [rows   (for [y (range 0 400)]
                 (core/median
                   (core/colors bytes [0 y 400 (inc y)])))
        deltas (map (fn [i prev color]
                      [i (core/dist prev color)])
                 (range 1 400) rows (next rows))]
    (->> (sort-by second deltas)
      (reverse)
      (take-while (fn [[i d]] (> d delta-threshold)))
      (remove-close grid-threshold)
      (map first)
      (take 20)
      (sort)
      (cons 0))))

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

(defn log [picture bytes cols rows]
  (loop [log     []
         picture picture
         xs      cols
         ys      rows]
    (cond
      (empty? ys)
      log
      
      (empty? xs)
      (recur log picture cols (next ys))
      
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
          (recur log picture (next xs) ys))))))

(defn equal-grid [cnt]
  (let [size (quot 400 cnt)
        xys  (range 0 (- 400 size) size)]
    [xys xys]))

(defn logs [problem]
  (let [{:problem/keys [picture bytes]} problem
        [merge-log] (if (> (count picture) 1)
                      (algo.merge/merge picture)
                      [])
        picture'    (transform/transform-all picture merge-log)
        root        (first (keys picture'))]
    (for [cnt  (range 10 70)
          :let [[cols rows] (equal-grid cnt)]]
      (concat
        merge-log
        [[:color root [255 255 255]]]
        (log picture' bytes cols rows)))))

(defn adaptive-logs [problem]
  (let [{:problem/keys [picture bytes]} problem
        [merge-log] (if (> (count picture) 1)
                      (algo.merge/merge picture)
                      [])
        picture'    (transform/transform-all picture merge-log)
        root        (first (keys picture'))
        cols        (detect-cols bytes)
        rows        (detect-rows bytes)]
    [(concat
       merge-log
       [[:color root [255 255 255]]]
       (log picture' bytes cols rows))]))
