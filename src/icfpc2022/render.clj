(ns icfpc2022.render
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.skija Bitmap Canvas Color Image]
    [io.github.humbleui.types IPoint IRect Rect]))

(set! *warn-on-reflection* true)

(defrecord SimpleBlock [shape ;; [:rect l b r t]
                        color ;; [r g b a]
                        ])

(defrecord ComplexBlock [shape    ;; [:rect l b r t]
                         children ;; [(SimpleBlock | ComplexBlock) ...]
                         ])

(def start-picture
  (ComplexBlock. [:rect 0 0 400 400]
    [(SimpleBlock. [:rect 0 0 400 400] [255 255 255 255])]))

(def *log 
  (atom []))

(defmulti transform ; => picture'
  (fn [picture op]
    (first op)))

(defn pcut-block [simple-block [x y]]
  (let [[kind l b r t] (:shape simple-block)
        color (:color simple-block)]
    (assert (= kind :rect))
    (assert (and (< x r)
              (< l x)
              (< y t)
              (< b y)))
    [(SimpleBlock. [kind l b x y] color)
     (SimpleBlock. [kind x b r y] color)
     (SimpleBlock. [kind x y r t] color)
     (SimpleBlock. [kind l y x t] color)]))

(defn xcut-block [simple-block x]
  (let [[kind l b r t] (:shape simple-block)
        color (:color simple-block)]
    (assert (= kind :rect))
    (assert (and (< x r)
                 (< l x)))
    [(SimpleBlock. [kind l b x t] color)
     (SimpleBlock. [kind x b r t] color)]))

(defn ycut-block [simple-block y]
  (let [[kind l b r t] (:shape simple-block)
        color (:color simple-block)]
    (assert (= kind :rect))
    (assert (and (< y t)
                 (< b y)))
    [(SimpleBlock. [kind l b r y] color)
     (SimpleBlock. [kind l y r t] color)]))

(defn update-by-id [block id  f]
  (let [go (fn go [block id]
             (let [[child-id & rest] id]
               (if (some? child-id)
                 (do
                   (assert (instance? ComplexBlock block) (str "Expected Complex block for id: " id))
                   (update-in block [:children child-id] (fn [child] (go child rest))))
                 (do
                   (assert (instance? SimpleBlock block) (str "Expected simple block"))
                   (f block)))))]
    (go block id)))

(defmethod transform :pcut [block [_ id [x y]]]
  (update-by-id block id
                (fn [block]
                  (ComplexBlock. (:shape block) (pcut-block block [x y])))))

(defmethod transform :color [block [_ id color]]
  (update-by-id block id
                (fn [block]
                  (assoc block :color color))))

(defmethod transform :xcut [block [_ id x]]
  (update-by-id block id
                (fn [block]
                  (ComplexBlock. (:shape block)
                                 (xcut-block block x)))))

(defmethod transform :ycut [block [_ id y]]
  (update-by-id block id
                (fn [block]
                  (ComplexBlock. (:shape block)
                                 (ycut-block block y)))))

(def *picture
  (atom (reduce transform start-picture @*log)))

(def *coord
  (atom nil))

(def *tool
  (atom [:pcut]))

(defn round-to [x v]
  (-> x
    (/ v)
    (math/round)
    (* v)
    (int)))

(defn coords [ctx event]
  (let [x (int (quot (:x event) (:scale ctx)))
        y (int (- 400 (quot (:y event) (:scale ctx))))]
    (when-some [[x' y'] (cond
                          (< x -420)  nil
                          (< -20 x 0) nil
                          (< 400 x)   nil
                          (< y 0)     nil
                          (< 400 y)   nil
                          (< 0 x 400)    [x y]
                          (< -420 x -20) [(+ x 420) y])]
      (if (= [:pcut] @*tool)
        [(round-to x' 40) (round-to y' 40)]
        [x' y']))))

(defn inside? [[_ l b r t] [x y]]
  (and
    (<= l x r)
    (<= b y t)))

(defn find-leaf [block id [x y]]
  (when (inside? (:shape block) [x y])
    (if (instance? ComplexBlock block)
      (reduce
        (fn [i child]
          (if-some [id' (find-leaf child (conj id i) [x y])]
            (reduced id')
            (inc i)))
        0
        (:children block))
      id)))

(defn event [ctx event]
  (core/eager-or
    (when (= :mouse-move (:event event))
      (when-some [[x y] (coords ctx event)]
        (reset! *coord [x y])
        true))

    (when (and
            (= :mouse-button (:event event))
            (= :primary (:button event))
            (:pressed? event))
      (when-some [[x y] (coords ctx event)]
        (let [tool @*tool
              id   (find-leaf @*picture [] [x y])]
          (when-some [op (case (first tool)
                           :pcut
                           [:pcut id [x y]]

                           :xcut
                           [:xcut id x]

                           :ycut
                           [:ycut id y]

                           :color
                           (let [[_ r g b a] tool]
                             [:color id [r g b a]])
                           
                           nil)]
            (swap! *log conj op)
            (swap! *picture transform op)
            true))))))

(def fill-guides
  (paint/fill 0x40FF0000))

(def stroke-guides
  (paint/stroke 0x40FF0000 2))

(defn draw-block [ctx ^Canvas canvas block id debug?]
  (let [{:keys [scale]} ctx
        [_ l b r t] (:shape block)
        rect (IRect/makeLTRB l (- 400 t) r (- 400 b))]
    (when debug?
      (canvas/draw-rect canvas rect stroke-guides))
    (if (instance? ComplexBlock block)
      (dotimes [i (count (:children block))]
        (draw-block ctx canvas (nth (:children block) i) (str id "." i) debug?))
      (let [[red green blue alpha] (:color block)]
        (with-open [fill (paint/fill (Color/makeARGB alpha red green blue))]
          (canvas/draw-rect canvas rect fill))))
    (when debug?
      (canvas/draw-string canvas id
        (+ l 10)
        (- 400 b 10)
        (:font-ui ctx) (:fill-text ctx)))))

(defn draw [ctx ^Canvas canvas ^IPoint size]
  (let [{:keys [scale]} ctx]
    (with-open [bitmap (Bitmap.)]
      (.allocN32Pixels bitmap 400 400 true)
      (let [canvas' (Canvas. bitmap)]
        (with-open [fill (paint/fill 0xFFFFFFFF)]
          (canvas/draw-rect canvas' (IRect/makeXYWH 0 0 400 400) fill))
        (draw-block ctx canvas' @*picture "" true))
      (let [image (Image/makeFromBitmap bitmap)]
        (.drawImageRect canvas image (Rect/makeXYWH 0 0 (* scale 400) (* scale 400)))))
    (when-some [[x y] @*coord]
      (canvas/draw-line canvas (* scale x) 0 (* scale x) (* scale 400) fill-guides)
      (canvas/draw-line canvas 0 (* scale (- 400 y)) (* scale 400) (* scale (- 400 y)) fill-guides))))

(defn dump []
  (println "--- begin ---")
  (doseq [op @*log]
    (println
      (case (first op)
        :pcut
        (let [[_ id [x y]] op]
          (format "pcut [%s] [%d, %d]" (str/join "." id) x y))

        :xcut
        (let [[_ id x] op]
          (format "lcut [%s] %d" (str/join "." id) x))

        :ycut
        (let [[_ id y] op]
          (format "lcut [%s] %d" (str/join "." id) y))

        :color
        (let [[_ id [r g b a]] op]
          (format "color [%s] [%d, %d, %d, %d]" (str/join "." id) r g b a)))))
  (println "--- end ---")
  
  (with-open [bitmap (Bitmap.)]
    (.allocN32Pixels bitmap 400 400 true)
    (let [canvas' (Canvas. bitmap)]
      (with-open [fill (paint/fill 0xFFFFFFFF)]
        (canvas/draw-rect canvas' (IRect/makeXYWH 0 0 400 400) fill))
      (draw-block nil canvas' @*picture "" false))
    (let [image (Image/makeFromBitmap bitmap)
          bytes (.getBytes (.encodeToData image))]
      (with-open [os (io/output-stream "answers/1.png")]
        (.write os bytes)))))

(def app
  (ui/default-theme
    {:hui.text-field/padding-top    10
     :hui.text-field/padding-bottom 10
     :hui.text-field/padding-left   5
     :hui.text-field/padding-right  5}
    (ui/padding 20
      (ui/row
        (ui/valign 0.5
          (ui/width 400
            (ui/height 400
              (ui/image "resources/1.png"))))
        (ui/gap 20 0)
        (ui/valign 0.5
          (ui/width 400
            (ui/height 400
              (ui/canvas {:on-paint draw
                          :on-event event}))))
        (ui/gap 20 0)
        [:stretch 1
         (ui/column
           (ui/dynamic _ [tool @*tool]
             (ui/label (str "Tool: " tool)))
           (ui/gap 0 10)
           (ui/dynamic _ [coord @*coord]
             (ui/label (str "Mouse: " coord)))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:pcut])
             (ui/label "Point Cut"))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:xcut])
             (ui/label "Point XCut"))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:ycut])
             (ui/label "Point YCut"))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:color 255 255 255 255])
             (ui/label "Fill White"))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:color 0 0 0 255])
             (ui/label "Fill Black"))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:color 0x0 0x4A 0xAD 255])
             (ui/label "Fill Blue"))
           (ui/gap 0 10)
           (ui/button
             #(do
                (reset! *log [])
                (reset! *picture start-picture))
             (ui/label "RESET"))

           (ui/gap 0 20)
           (ui/label "Log:")
           (ui/gap 0 10)
           [:stretch 1
            (ui/vscrollbar
              (ui/vscroll
                (ui/padding 0 10
                  (ui/dynamic _ [log @*log]
                    (ui/column
                      (interpose (ui/gap 0 10)
                        (for [op log]
                          (ui/label op))))))))]
           (ui/gap 0 10)
           (ui/button
             dump
             (ui/label "Dump")))]))))

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(redraw)
