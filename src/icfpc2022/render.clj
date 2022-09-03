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
                         children ;; [(SimpleBlock | ComplexBlock) ...]
                         ])

(def start-picture
  {:counter 0
   :blocks  {"0" (SimpleBlock. [:rect 0 0 400 400] [255 255 255 255])}
   })

(def *log
  (atom []))

(def *guides?
  (atom true))

(def *preview
  (atom nil))

(defmulti transform                                         ; => picture'
          (fn [picture op]
            (first op)))

(defn pcut-shape [shape [x y]]
  (let [[kind l b r t] shape]
    (assert (= kind :rect))
    (assert (and (< x r)
                 (< l x)
                 (< y t)
                 (< b y)))
    [[kind l b x y]
     [kind x b r y]
     [kind x y r t]
     [kind l y x t]]))

(defn xcut-shape [shape x]
  (let [[kind l b r t] shape]
    (assert (= kind :rect))
    (assert (and (< x r)
                 (< l x)))
    [[kind l b x t]
     [kind x b r t]]))

(defn ycut-shape [shape y]
  (let [[kind l b r t] shape]
    (assert (= kind :rect))
    (assert (and (< y t)
                 (< b y)))
    [[kind l b r y]
     [kind l y r t]]))

(defn intersect-shapes [[kind1 l1 b1 r1 t1] [kind2 l2 b2 r2 t2]]
   (assert (and (= kind1 :rect) (= kind2 :rect)))
   (if (or (<= r1 l2) (<= r2 l1) (<= t1 b2) (<= t2 b1))
     nil
     [:rect (max l1 l2) (max b1 b2) (min r1 r2) (min t1 t2)]))

(defn cut-block [block shape-cut-fn]
  (let [shapes (shape-cut-fn (:shape block))]
    (cond
      (instance? SimpleBlock block)
      (mapv (fn [shape]
              (SimpleBlock. shape (:color block)))
            shapes)

      (instance? ComplexBlock block)
      (let [children (:children block)]
        (mapv (fn [shape]
                (let [filtered-children (keep (fn [child]
                                                (let [new-child-shape (intersect-shapes (:shape child) shape)]
                                                  (when (some? new-child-shape)
                                                    (SimpleBlock. new-child-shape (:color child)))))
                                              children)]
                  (ComplexBlock. shape filtered-children)))
              shapes))

      :else (assert false (str "Unexpected block type" block)))))

(defn pcut-block [block [x y]]
  (cut-block block (fn [shape] (pcut-shape shape [x y]))))

(defn xcut-block [block x]
  (cut-block block (fn [shape] (xcut-shape shape x))))

(defn ycut-block [block y]
  (cut-block block (fn [shape] (ycut-shape shape y))))

(defmethod transform :color [picture [_ id color]]
  (assert (contains? (:blocks picture) id) (str ":color No block " id))
  (update-in picture [:blocks id] (fn [block]
                                    (SimpleBlock. (:shape block) color))))

(defmethod transform :pcut [picture [_ id [x y]]]
  (assert (contains? (:blocks picture) id) (str ":pcut No block " id))
  (let [block (get-in picture [:blocks id])
        new-blocks  (map-indexed (fn [ind block]
                                   [(str id "." ind) block])
                      (pcut-block block [x y]))]
    (update picture :blocks
      (fn [blocks]
        (-> blocks
          (dissoc id)
          (merge (into {} new-blocks)))))))

(defmethod transform :xcut [picture [_ id x]]
  (assert (contains? (:blocks picture) id) (str ":xcut No block " id))
  (let [block (get-in picture [:blocks id])
        new-blocks  (map-indexed (fn [ind block]
                                   [(str id "." ind) block])
                      (xcut-block block x))]
    (update picture :blocks
      (fn [blocks]
        (-> blocks
          (dissoc id)
          (merge (into {} new-blocks)))))))

(defmethod transform :ycut [picture [_ id y]]
  (assert (contains? (:blocks picture) id) (str ":ycut No block " id))
  (let [block (get-in picture [:blocks id])
        new-blocks  (map-indexed (fn [ind block]
                                   [(str id "." ind) block])
                      (ycut-block block y))]
    (update picture :blocks
      (fn [blocks]
        (-> blocks
          (dissoc id)
          (merge (into {} new-blocks)))))))

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
  (let [block1 (get-in picture [:blocks id1])
        shape1 (:shape block1)
        block2 (get-in picture [:blocks id2])
        shape2 (:shape block2)]
    (assert (contains? (:blocks picture) id1) (str ":swap No block " id1))
    (assert (contains? (:blocks picture) id2) (str ":swap No block " id2))
    (assert (same-shape? shape1 shape2) "Blocks should be the same shape")
    (-> picture
      (assoc-in [:blocks id1 :shape] shape2)
      (assoc-in [:blocks id2 :shape] shape1))))

(defn merge-shapes [shape1 shape2]
  (let [[kind1 l1 b1 r1 t1] shape1
        [kind2 l2 b2 r2 t2] shape2]
    (assert (= kind1 kind2 :rect))
    (cond
      ; shape1
      ; shape2
      (and (= b1 t2) (= l1 l2) (= r1 r2))
      [:rect l1 b2 r1 t1]
      ; shape2
      ; shape1
      (and (= b2 t1) (= l1 l2) (= r1 r2))
      [:rect l1 b1 r1 t2]
      ; shape1 shape2
      (and (= r1 l2) (= b1 b2) (= t1 t2))
      [:rect l1 b1 r2 t1]
      ; shape2 shape1
      (and (= r2 l1) (= b1 b2) (= t1 t2))
      [:rect l2 b1 r1 t1]
      :else
      nil)))

(defn list-simple-blocks [block]
  (cond
    (instance? SimpleBlock block)
    [block]

    (instance? ComplexBlock block)
    (mapcat (fn [block] (list-simple-blocks block)) (:children block))

    :else (assert false (str "Unexpected block type: " block))))

(defmethod transform :merge [picture [_ id1 id2]]
  (let [block1 (get-in picture [:blocks id1])
        shape1 (:shape block1)
        block2 (get-in picture [:blocks id2])
        shape2 (:shape block2)
        merged-shape (merge-shapes shape1 shape2)]
    (assert (some? merged-shape) "Blocks should be the same shape")
    (let [picture (update picture :counter inc)
          new-block (ComplexBlock. merged-shape (into (list-simple-blocks block1)
                                                  (list-simple-blocks block2)))]
      (update picture :blocks
        (fn [blocks]
          (-> blocks
            (assoc (str (:counter picture)) new-block)
            (dissoc id1)
            (dissoc id2)))))))

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

(defn byte->long [n]
  (assert (<= -128 n 127) (str "Expected -128..127, got: " n))
  (-> n (+ 256) (mod 256)))

(defn long->byte [^long n]
  (assert (<= 0 n 255) (str "Expected 0..255, got: " n))
  (if (>= n 128)
    (- n 256)
    n))

(def problem
  "3")

(def snap
  (case problem
    "1" 40
    "3" 25
    5))

(defonce ^Image original-image
  (Image/makeFromEncoded (core/slurp-bytes (str "resources/" problem ".png"))))

(defonce ^Bitmap original-bytes
  (let [bitmap     (Bitmap.)
        image-info (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))]
    (.allocPixels bitmap image-info)
    (.readPixels original-image bitmap)
    (.readPixels bitmap)))

(defn get-color [^bytes bytes x y]
  (let [idx (* 4 (+ x (* 400 (- 399 y))))
        r   (byte->long (aget bytes (+ idx 0)))
        g   (byte->long (aget bytes (+ idx 1)))
        b   (byte->long (aget bytes (+ idx 2)))
        a   (byte->long (aget bytes (+ idx 3)))]
    [r g b a]))

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
        [(round-to x' snap) (round-to y' snap)]
        
        :xcut 
        [(round-to x' snap) y']
        
        :ycut
        [x' (round-to y' snap)]
        
        [x' y']))))

(defn inside? [[_ l b r t] [x y]]
  (and
    (<= l x r)
    (<= b y t)))

(defn find-block-id [picture [x y]]
  (let [[id block] (first (filter (fn [[id block]]
                                    (inside? (:shape block) [x y]))
                            (:blocks picture)))]
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
        (let [tool    @*tool
              picture @*picture
              id      (find-block-id picture [x y])]
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
                           
                           :picker
                           (let [[r g b a] (get-color original-bytes x y)]
                             (reset! *tool [:color r g b a])
                             nil)

                           :swap
                           (let [[_ id1] tool]
                             (if (some? id1)
                               (do
                                 (reset! *tool [:swap])
                                 [:swap id1 id])
                               (do
                                 (reset! *tool [:swap id])
                                 nil)))

                           :merge
                           (let [[_ id1] tool]
                             (if (some? id1)
                               (do (swap! *tool (fn [_] [:merge]))
                                 [:merge id1 id])
                               (do (swap! *tool (fn [_] [:merge id]))
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
      (doseq [nested-block (:children block)]
        (draw-block canvas nested-block nil))
      (let [[red green blue alpha] (:color block)]
        (with-open [fill (paint/fill (Color/makeARGB alpha red green blue))]
          (canvas/draw-rect canvas rect fill))))))

(defn similarity
  ([^bytes p1 ^bytes p2]
   (similarity p1 #(get-color p2 %1 %2) [0 0 400 400]))
  ([^bytes p1 get-color2 [l b r t]]
   (loop [x   0
          y   0
          res 0.0]
     (cond
       (>= x 400)
       (recur 0 (inc y) res)
        
       (>= y 400)
       (math/round (* res 0.005))
        
       :else
       (let [[r1 g1 b1 a1] (get-color p1 x y)
             [r2 g2 b2 a2] (get-color2 x y)]
         (recur (inc x) y
           (+ res (math/sqrt (+ (* (- r1 r2) (- r1 r2))
                               (* (- g1 g2) (- g1 g2))
                               (* (- b1 b2) (- b1 b2))
                               (* (- a1 a2) (- a1 a2)))))))))))

(defn op-cost [type [shape l b r t]]
  (let [base  ({:xcut  7
                :ycut  7
                :pcut  10
                :color 5
                :swap  3
                :merge 1} type)
        area  (* (- r l) (- t b))]
    (math/round (/ (* base 400 400) area))))

(defn cost
  ([log]
   (second
     (reduce
       (fn [[picture acc] op]
         [(transform picture op)
          (+ acc (cost picture op))])
       [start-picture 0]
       log)))
  ([picture op]
   (let [id    (second op)
         block (get-in picture [:blocks id])
         [shape l b r t] (:shape block)]
     (op-cost (first op) [shape l b r t]))))

(defn draw-picture [^Canvas canvas picture]
  (doseq [[id block] (:blocks picture)]
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

(defn score
  ([]
   (score @*log original-bytes))
  ([log original-bytes]
   (let [picture (reduce transform start-picture log)]
     (with-open [bitmap (render-to-bitmap picture)]
       (let [pixels (.readPixels bitmap)
             sim    (similarity original-bytes pixels)
             cost   (cost log)]
         (+ cost sim))))))

(defn draw-guides [ctx ^Canvas canvas picture]
  (let [{:keys [scale]} ctx]
    (doseq [[id block] (:blocks picture)]
      (let [[_ l b r t] (:shape block)
            rect (IRect/makeLTRB (* scale l) (* scale (- 400 t)) (* scale r) (* scale (- 400 b)))]
        (canvas/draw-rect canvas rect stroke-guides)))))

(defn draw-cursor [ctx ^Canvas canvas]
  (let [{:keys [scale]} ctx]
    (when-some [[x y] @*coord]
      (when (#{:pcut :xcut} (first @*tool))
        (canvas/draw-line canvas (* scale x) 0 (* scale x) (* scale 400) fill-guides))
      (when (#{:pcut :ycut} (first @*tool))
        (canvas/draw-line canvas 0 (* scale (- 400 y)) (* scale 400) (* scale (- 400 y)) fill-guides)))))

(defn solution [log]
  (str/join "\n" 
    (for [op log]
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
          (format "swap [%s] [%s]" id1 id2))

        :merge
        (let [[_ id1 id2] op]
          (format "merge [%s] [%s]" id1 id2))))))

(defn dump []
  (println "--- begin ---")
  (let [solution (solution @*log)
        score    (score)
        file     (str "answers/problem " problem "/" score)]
    (println solution)
    (println "--- end ---")
    (println "Score:" score)
    (println "File:" file)
    (.mkdirs (io/file (str "answers/problem " problem)))
    (spit file solution)))

(defn tool [tool label]
  (ui/dynamic _ [selected? (= tool @*tool)]
    (ui/button
      #(reset! *tool tool)
      {:bg (if selected? 0xFFFED7B2 0xFFB2D7FE)}
      label)))

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
        (ui/dynamic _ [log     (if-some [preview @*preview]
                                 (take preview @*log)
                                 @*log)
                       picture (if-some [preview @*preview]
                                 (reduce transform start-picture log)
                                 @*picture)]
          (with-open [bitmap (render-to-bitmap picture)]
            (let [pixels (.readPixels bitmap)
                  sim    (similarity original-bytes pixels)
                  cost   (cost log)]
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
                (ui/row
                  (ui/column
                    (ui/label "Total Score:")
                    (ui/gap 0 10)
                    (ui/label "Similarity:")
                    (ui/gap 0 10)
                    (ui/label "Cost:"))
                  (ui/gap 10 0)
                  (ui/column
                    (ui/label (+ sim cost))
                    (ui/gap 0 10)
                    (ui/label sim)
                    (ui/gap 0 10)
                    (ui/label cost)))))))
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
              (ui/dynamic _ [tool @*tool]
                (ui/button
                  #(reset! *tool [:picker])
                  {:bg (if (#{:picker :color} (first tool))
                         0xFFFED7B2
                         0xFFB2D7FE)}
                  (if (= :color (first tool))
                    (let [[_ r g b a] tool]
                      (ui/rect (paint/fill (Color/makeARGB a r g b))
                        (ui/gap 30 10)))
                    (ui/label "🥢"))))]
             (ui/gap 10 0)   
             [:stretch 1
              (tool [:swap]
                (ui/label "↔︎"))]
             (ui/gap 10 0)
             [:stretch 1
              (tool [:merge]
                (ui/label "M"))])

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
