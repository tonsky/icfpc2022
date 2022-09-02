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

(def *preview
  (atom nil))

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
      (case (first @*tool)
        :pcut
        [(round-to x' 40) (round-to y' 40)]
        
        :xcut 
        [(round-to x' 40) y']
        
        :ycut
        [x' (round-to y' 40)]
        
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
            (swap! *picture transform op)
            (swap! *log conj op)
            true))))))

(def fill-guides
  (paint/fill 0x80FF0000))

(def stroke-guides
  (paint/stroke 0x80FF0000 2))

(defn draw-block [^Canvas canvas block id debug?]
  (let [[_ l b r t] (:shape block)
        rect (IRect/makeLTRB l (- 400 t) r (- 400 b))]
    (when debug?
      (canvas/draw-rect canvas rect stroke-guides))
    (if (instance? ComplexBlock block)
      (dotimes [i (count (:children block))]
        (draw-block canvas (nth (:children block) i) (str id "." i) debug?))
      (let [[red green blue alpha] (:color block)]
        (with-open [fill (paint/fill (Color/makeARGB alpha red green blue))]
          (canvas/draw-rect canvas rect fill))))))

(defn ^Bitmap render-to-bitmap [picture debug?]
  (let [bitmap (Bitmap.)]
    (.allocN32Pixels bitmap 400 400 true)
    (let [canvas' (Canvas. bitmap)]
      (with-open [fill (paint/fill 0xFFFFFFFF)]
        (canvas/draw-rect canvas' (IRect/makeXYWH 0 0 400 400) fill))
      (draw-block canvas' picture "" debug?))
    bitmap))

(defn draw [ctx ^Canvas canvas ^IPoint size]
  (let [{:keys [scale]} ctx
        preview @*preview
        picture (if preview
                  (reduce transform start-picture (take preview @*log))
                  @*picture)]
    (with-open [bitmap (render-to-bitmap picture true)
                image  (Image/makeFromBitmap bitmap)]
      (.drawImageRect canvas image (Rect/makeXYWH 0 0 (* scale 400) (* scale 400))))
    (when-some [[x y] @*coord]
      (when (#{:pcut :xcut} (first @*tool))
        (canvas/draw-line canvas (* scale x) 0 (* scale x) (* scale 400) fill-guides))
      (when (#{:pcut :ycut} (first @*tool))
        (canvas/draw-line canvas 0 (* scale (- 400 y)) (* scale 400) (* scale (- 400 y)) fill-guides)))))

(defn dump []
  (println "--- begin ---")
  (doseq [op @*log]
    (println
      (case (first op)
        :pcut
        (let [[_ id [x y]] op]
          (format "cut [%s] [%d, %d]" (str/join "." id) x y))

        :xcut
        (let [[_ id x] op]
          (format "cut [%s] [x] [%d]" (str/join "." id) x))

        :ycut
        (let [[_ id y] op]
          (format "cut [%s] [y] [%d]" (str/join "." id) y))

        :color
        (let [[_ id [r g b a]] op]
          (format "color [%s] [%d, %d, %d, %d]" (str/join "." id) r g b a)))))
  (println "--- end ---"))

(defn tool [tool label]
  (ui/dynamic _ [selected? (= tool @*tool)]
    (ui/button
      #(reset! *tool tool)
      {:bg (if selected? 0xFFFED7B2 0xFFB2D7FE)}
      label)))

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
              (ui/dynamic _ [preview *preview
                             picture *picture]
                (ui/canvas {:on-paint draw
                            :on-event event})))))
        (ui/gap 20 0)
        [:stretch 1
         (ui/column
           (ui/row
             [:stretch 1
              (tool [:pcut]
                (ui/label "╋"))]
             (ui/gap 10 0)
             [:stretch 1
              (tool [:xcut]
                (ui/label "┃"))]
             (ui/gap 10 0)
             [:stretch 1
              (tool [:ycut]
                (ui/label "━"))])
           
           (ui/gap 0 10)
           
           (ui/row
             [:stretch 1
              (tool [:color 255 255 255 255]
                (ui/rect (paint/fill 0xFFFFFFFF)
                  (ui/gap 30 20)))]
             (ui/gap 10 0)
             [:stretch 1
              (tool [:color 0 0 0 255]
                (ui/rect (paint/fill 0xFF000000)
                  (ui/gap 30 20)))]
             (ui/gap 10 0)
             [:stretch 1
              (tool [:color 0x0 0x4A 0xAD 255]
                (ui/rect (paint/fill 0xFF004AAD)
                  (ui/gap 30 20)))])
                                 
           (ui/gap 0 10)
           
           (ui/button
             #(do
                (reset! *log [])
                (reset! *picture start-picture))
             (ui/label "RESET"))

           (ui/gap 0 10)
           (ui/dynamic _ [coord @*coord]
             (ui/label (str "Mouse: " coord)))

           (ui/gap 0 10)
           (ui/label "Log:")
           (ui/gap 0 10)
           
           [:stretch 1
            (ui/dynamic _ [log @*log]
              (ui/vscrollbar
                (ui/vscroll
                  (ui/column
                    (for [[idx op] (reverse (map vector (next (range)) log))]
                      (ui/clickable
                        {:on-click
                         (fn [_]
                           (swap! *log #(vec (take idx %)))
                           (reset! *picture (reduce transform start-picture @*log)))}
                        (ui/hoverable
                          {:on-hover #(reset! *preview idx)
                           :on-out   (fn [] (swap! *preview #(if (= idx %) nil %)))}
                          (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                            (let [label (ui/padding 10 10
                                          (ui/label (str idx ". " op)))]
                              (if hovered?
                                (ui/rect (paint/fill 0xFFEEEEEE)
                                  label)
                                label))))))))))]
           
           (ui/gap 0 10)
           (ui/button dump
             (ui/label "Dump")))]))))

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(redraw)
