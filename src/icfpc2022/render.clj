(ns icfpc2022.render
  (:require
    [clojure.string :as str]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.skija Canvas Color]
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
  [[:pcut [0] 200 200]
   [:color [0 1] 0xCC 0x33 0x33 0xFF]])

(defmulti transform ; => picture'
  (fn [picture op]
    (first op)))

(defmethod transform :pcut [picture [_ id x y]]
  picture)

(defmethod transform :color [picture [_ id r g b a]]
  picture)

(def *picture
  (atom (reduce transform start-picture *log)))

(defonce *coord
  (atom (IPoint. 0 0)))

(defonce *tool
  (atom :pcut))

(defn event [ctx event]
  (when (= :mouse-move (:event event))
    (let [x (quot (:x event) (:scale ctx))
          y (- 400 (quot (:y event) (:scale ctx)))]
      (cond
        (< x 0)       (reset! *coord nil)
        (< 400 x 420) (reset! *coord nil)
        (< 820 x)     (reset! *coord nil)
        (< y 0)       (reset! *coord nil)
        (< 400 y)     (reset! *coord nil)
        (< x 400)     (reset! *coord (IPoint. x y))
        (< 420 x 820) (reset! *coord (IPoint. (- x 420) y))))))

(defn draw-block [ctx ^Canvas canvas block id]
  (let [{:keys [scale]} ctx
        [_ l b r t] (:shape block)]
    (if (instance? ComplexBlock block)
      (dotimes [i (count (:children block))]
        (draw-block ctx canvas (nth (:children block) i) (str id "." i)))
      (let [[red green blue alpha] (:color block)]
        (with-open [fill (paint/fill (Color/makeARGB alpha red green blue))]
          (canvas/draw-rect canvas
            (IRect/makeLTRB (* scale l) (* scale (- 400 t)) (* scale r) (* scale (- 400 b))) fill))))
    (canvas/draw-string canvas id
      (* scale (+ l 10))
      (* scale (- 400 b 10))
      (:font-ui ctx) (:fill-text ctx))))

(defn draw [ctx ^Canvas canvas ^IPoint size]
  (with-open [fill (paint/fill 0xFFFFFFFF)]
    (canvas/draw-rect canvas (IRect/makeXYWH 0 0 800 800) fill))
  (draw-block ctx canvas @*picture ""))

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
             (ui/label (str "Mouse: " (:x coord) " " (:y coord))))
           (ui/gap 0 10)
           (ui/button
             #(reset! *tool [:pcut])
             (ui/label "Point Cut"))
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
             (ui/label "Fill Blue")))]))))

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(redraw)
