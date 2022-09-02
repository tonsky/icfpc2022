(ns icfpc2022.render
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.protocols :as protocols]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.skija Bitmap Canvas Color ColorAlphaType ColorSpace ColorType Image ImageInfo]
    [io.github.humbleui.types IPoint IRect Rect]))

(set! *warn-on-reflection* true)

(defrecord SimpleBlock [shape ;; [:rect l b r t]
                        color ;; [r g b a]
                        ])

(defrecord ComplexBlock [shape    ;; [:rect l b r t]
                         children ;; { id (SimpleBlock | ComplexBlock) ... }
                         ])

(def start-picture
  { "0" (SimpleBlock. [:rect 0 0 400 400] [255 255 255 255]) })

(def *log 
  (atom []))

(def *guides?
  (atom true))

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

(defmethod transform :color [picture [_ id color]]
  (update picture id (fn [block]
                       (assert (instance? SimpleBlock block) "Set color to Complex block")
                       (assoc block :color color))))

(defmethod transform :pcut [picture [_ id [x y]]]
  (let [block (get picture id)
        new-blocks  (map-indexed (fn [ind block]
                                   [(str id "." ind) block])
                      (pcut-block block [x y]))]
    (assert (instance? SimpleBlock block) "Cut Complex block")
    (-> picture
      (dissoc id)
      (merge (into {} new-blocks)))))

(defmethod transform :xcut [picture [_ id x]]
  (let [block (get picture id)
        new-blocks  (map-indexed (fn [ind block]
                                   [(str id "." ind) block])
                      (xcut-block block x))]
    (assert (instance? SimpleBlock block) "Cut Complex block")
    (-> picture
      (dissoc id)
      (merge (into {} new-blocks)))))

(defmethod transform :ycut [picture [_ id y]]
  (let [block (get picture id)
        new-blocks  (map-indexed (fn [ind block]
                                   [(str id "." ind) block])
                      (ycut-block block y))]
    (assert (instance? SimpleBlock block) "Cut Complex block")
    (-> picture
      (dissoc id)
      (merge (into {} new-blocks)))))

(defn shape-width [shape]
  (let [[_ l b r t] shape]
    (- r l)))

(defn shape-height [shape]
  (let [[_ l b r t] shape]
    (- t b)))

(defn same-shape? [shape1 shape2]
  (and (= (first shape1) (first shape2))
    (= (shape-height shape1) (shape-height shape2))
    (= (shape-width shape1) (shape-width shape2))))

(defmethod transform :swap [picture [_ id1 id2]]
  (let [block1 (get picture id1)
        shape1 (:shape block1)
        block2 (get picture id2)
        shape2 (:shape block2)]
    (assert (same-shape? shape1 shape2) "Blocks should be the same shape")
    (-> picture
        (assoc-in [id1 :shape] shape2)
        (assoc-in [id2 :shape] shape1))))

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

(defn find-block-id [picture [x y]]
  (let [[id block] (first (filter (fn [[id block]] (inside? (:shape block) [x y])) picture))]
    id))

(defn event [ctx event]
  (core/eager-or
    (when (= :mouse-move (:event event))
      (reset! *coord (coords ctx event))
      (when @*coord
        (reset! *preview nil))
      true)

    (when (and
            (= :mouse-button (:event event))
            (= :primary (:button event))
            (:pressed? event))
      (when-some [[x y] (coords ctx event)]
        (let [tool @*tool
              id   (find-block-id @*picture [x y])]
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

                           :swap
                           (let [[_ id1] tool]
                             (if (some? id1)
                               (do (swap! *tool (fn [_] [:swap]))
                                 [:swap id1 id])
                               (do (swap! *tool (fn [_] [:swap id]))
                                 nil)))
                           nil)]
            (swap! *picture transform op)
            (swap! *log conj op)
            true))))))

(def fill-guides
  (paint/fill 0xFFFF8080))

(def stroke-guides
  (paint/stroke 0xFFFF8080 2))

(defn draw-block [^Canvas canvas block id]
  (let [[_ l b r t] (:shape block)
        rect (IRect/makeLTRB l (- 400 t) r (- 400 b))]
    (if (instance? ComplexBlock block)
      (assert false "Not implemented")
      (let [[red green blue alpha] (:color block)]
        (with-open [fill (paint/fill (Color/makeARGB alpha red green blue))]
          (canvas/draw-rect canvas rect fill))))))

(defn byte->long [n]
  (assert (<= -128 n 127) (str "Expected -128..127, got: " n))
  (-> n (+ 256) (mod 256)))

(defn long->byte [^long n]
  (assert (<= 0 n 255) (str "Expected 0..255, got: " n))
  (if (>= n 128)
    (- n 256)
    n))

(defn similarity [^bytes p1 ^bytes p2]
  (loop [i   0
         res 0.0]
    (if (>= i (alength p1))
      (math/round (* res 0.005))
      (let [r1 (byte->long (aget p1 (+ i 0)))
            g1 (byte->long (aget p1 (+ i 1)))
            b1 (byte->long (aget p1 (+ i 2)))
            a1 (byte->long (aget p1 (+ i 3)))
            r2 (byte->long (aget p2 (+ i 0)))
            g2 (byte->long (aget p2 (+ i 1)))
            b2 (byte->long (aget p2 (+ i 2)))
            a2 (byte->long (aget p2 (+ i 3)))]
        (recur (+ i 4) (+ res (math/sqrt (+ (* (- r1 r2) (- r1 r2))
                                           (* (- g1 g2) (- g1 g2))
                                           (* (- b1 b2) (- b1 b2))
                                           (* (- a1 a2) (- a1 a2))))))))))

(defn draw-picture [^Canvas canvas picture]
  (doseq [[id block] picture]
    (draw-block canvas block id)))

(defn ^Bitmap render-to-bitmap [picture]
  (let [bitmap     (Bitmap.)
        image-info (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))]
    (.allocPixels bitmap image-info)
    (let [canvas' (Canvas. bitmap)]
      (with-open [fill (paint/fill 0xFFFFFFFF)]
        (canvas/draw-rect canvas' (IRect/makeXYWH 0 0 400 400) fill))
      (draw-picture canvas' picture))
    bitmap))

(defn draw-guides [ctx ^Canvas canvas picture]
  (let [{:keys [scale]} ctx]
    (doseq [[id block] picture
            :let [[_ l b r t] (:shape block)
                  rect (IRect/makeLTRB (* scale l) (* scale (- 400 t)) (* scale r) (* scale (- 400 b)))]]
      (canvas/draw-rect canvas rect stroke-guides))))

(defn draw-cursor [ctx ^Canvas canvas]
  (let [{:keys [scale]} ctx]
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
          (format "cut [%s] [%d, %d]" id x y))

        :xcut
        (let [[_ id x] op]
          (format "cut [%s] [x] [%d]" id x))

        :ycut
        (let [[_ id y] op]
          (format "cut [%s] [y] [%d]" id y))

        :color
        (let [[_ id [r g b a]] op]
          (format "color [%s] [%d, %d, %d, %d]" id r g b a))

        :swap
        (let [[_ id1 id2] op]
          (format "swap [%s] [%s]" id1 id2)))))
  (println "--- end ---"))

(defn tool [tool label]
  (ui/dynamic _ [selected? (= tool @*tool)]
    (ui/button
      #(reset! *tool tool)
      {:bg (if selected? 0xFFFED7B2 0xFFB2D7FE)}
      label)))

(defonce ^Image original-image
  (Image/makeFromEncoded (core/slurp-bytes "resources/1.png")))

(defonce ^Bitmap original-bitmap
  (let [bitmap (Bitmap.)
        image-info (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))]
    (.allocPixels bitmap image-info)
    (.readPixels original-image bitmap)
    bitmap))

(defonce original-bytes
  (.readPixels original-bitmap))

(core/deftype+ Stack [children ^:mut my-rect]
  protocols/IComponent
  (-measure [_ ctx cs]
    (reduce
      (fn [size child]
        (let [{:keys [width height]} (core/measure child ctx cs)]
          (IPoint. (max (:width size) width) (max (:height size) height))))
      (IPoint. 0 0) children))
  
  (-draw [_ ctx ^IRect rect ^Canvas canvas]
    (set! my-rect rect)
    (doseq [child children]
      (core/draw-child child ctx rect canvas)))
  
  (-event [_ ctx event]
    (reduce core/eager-or false
      (for [child children]
        (core/event-child child ctx event))))
  
  (-iterate [this ctx cb]
    (or
      (cb this)
      (some #(protocols/-iterate % ctx cb) children))))

(defn stack [& children]
  (->Stack children nil))

(def app
  (ui/default-theme
    {:hui.text-field/padding-top    10
     :hui.text-field/padding-bottom 10
     :hui.text-field/padding-left   5
     :hui.text-field/padding-right  5}
    (ui/padding 20
      (ui/row
        (ui/valign 0
          (ui/width 400
            (ui/height 400
              (ui/->AnImage original-image))))
        (ui/gap 20 0)
        (ui/dynamic _ [picture (if-some [preview @*preview]
                                 (reduce transform start-picture (take preview @*log))
                                 @*picture)]
          (with-open [bitmap (render-to-bitmap picture)]
            (let [pixels (.readPixels bitmap)]

              (ui/column
                (ui/width 400
                  (ui/height 400
                    (stack
                      (ui/->AnImage (Image/makeFromBitmap bitmap))
                      (ui/canvas {:on-paint (fn [ctx canvas size]
                                              (when @*guides?
                                                (draw-guides ctx canvas picture))
                                              (draw-cursor ctx canvas))
                                  :on-event event}))))
                (ui/gap 0 10)
                (ui/label (str "Similarity: " (similarity original-bytes pixels)))))))
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
                (ui/label "━"))]
             (ui/gap 10 0)
             [:stretch 1
              (tool [:swap]
                (ui/label "↔︎"))])

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
              (tool [:color 0 74 173 255]
                (ui/rect (paint/fill 0xFF004AAD)
                  (ui/gap 30 20)))])

           (ui/gap 0 10)
           
           (ui/checkbox
             *guides?
             (ui/label "Guides"))
           
           (ui/gap 0 10)

           (ui/button
             #(do
                (reset! *log [])
                (reset! *picture start-picture))
             (ui/label "RESET"))

           (ui/gap 0 10)
           (ui/dynamic _ [coord @*coord]
             (ui/label (str "Mouse: " coord)))

           (ui/gap 0 20)
           (ui/label "Log:")
           (ui/gap 0 10)

           [:stretch 1
            (ui/dynamic _ [log @*log]
              (ui/vscrollbar
                (ui/vscroll
                  (ui/column
                    (interpose (ui/gap 0 1)
                      (for [[idx op] (reverse (map vector (next (range)) log))]
                        (ui/clickable
                          {:on-click
                           (fn [_]
                             (swap! *log #(vec (take idx %)))
                             (reset! *picture (reduce transform start-picture @*log)))}
                          (ui/hoverable
                            {:on-hover #(reset! *preview idx)
                             :on-out   (fn []
                                         (swap! *preview #(if (= idx %) nil %)))}
                            (ui/dynamic ctx [hovered? (= idx @*preview)]
                              (let [label (ui/padding 10 10
                                            (ui/label (str idx ". " op)))]
                                (if hovered?
                                  (ui/rect (paint/fill 0xFFEEEEEE)
                                    label)
                                  label)))))))))))]

           (ui/gap 0 10)
           (ui/button dump
             (ui/label "Dump")))]))))

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(redraw)
