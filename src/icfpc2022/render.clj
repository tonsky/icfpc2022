(ns icfpc2022.render
  (:require
    [clj-async-profiler.core :as profiler]
    [clj-http.client :as http]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.algo.merge :as algo.merge]
    [icfpc2022.algo.paint :as algo.paint]
    [icfpc2022.algo.rect :as algo.rect]
    [icfpc2022.algo.swap :as algo.swap]
    [icfpc2022.algo.ycut :as algo.ycut]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as hui]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.protocols :as protocols]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.skija Bitmap Canvas Color ColorAlphaType ColorSpace ColorType Image ImageInfo]
    [io.github.humbleui.types IPoint IRect Rect]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(defonce *problem
  (atom
    (core/load-problem
      (if (.exists (io/file "last_open"))
        (parse-long (slurp "last_open"))
        1))))

(add-watch *problem ::clear-cache
  (fn [_ _ _ _]
    (vswap! core/*cache empty)))


(defonce *log
  (atom []))

(defonce *preview
  (atom nil))
  
(defonce *score
  (atom {}))

(defonce *picture
  (atom nil))

(defonce *image
  (atom nil))

(defn update-derived! [problem log preview]
  (let [log'     (if preview
                   (take preview log)
                   log)
        picture' (transform/transform-all (:problem/picture problem) log')
        score'   (score/score problem log')
        image'   (with-open [bitmap (core/render-to-bitmap picture')]
                   (Image/makeFromBitmap bitmap))]
    (reset! *picture picture')
    (reset! *score score')
    (reset! *image image')
    (redraw)))

(update-derived! @*problem @*log @*preview)

(add-watch *problem ::update-picture
  (fn [_ _ old new]
    (update-derived! new @*log @*preview)))

(add-watch *log ::update-picture
  (fn [_ _ old new]
    (update-derived! @*problem new @*preview)))

(add-watch *preview ::update-picture
  (fn [_ _ old new]
    (when (not= old new)
      (update-derived! @*problem @*log new))))

(defonce *problem-text
  (atom
    {:text
     (if (.exists (io/file "last_open"))
       (slurp "last_open")
       "1")}))

(add-watch *problem-text ::load-problem
  (fn [_ _ old new]
    (when (not= (:text old) (:text new))
      (when-some [id (parse-long (:text new))]
        (when (<= 1 id 40)
          (when (not= id (:problem/id @*problem))
            (reset! *problem (core/load-problem id))
            (reset! *log [])
            (spit "last_open" (str id))))))))

(def *guides?
  (atom true))

(def *coord
  (atom nil))

(def *tool
  (atom [:pcut]))

(defonce *status
  (atom "Ready"))

(add-watch *status ::redraw
  (fn [_ _ _ _]
    (redraw)))

(def snap
  5)

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
        [(core/round-to x' snap) (core/round-to y' snap)]
        
        :xcut 
        [(core/round-to x' snap) y']
        
        :ycut
        [x' (core/round-to y' snap)]
        
        [x' y']))))

(defn event [ctx event]
  (hui/eager-or
    (when (= :mouse-move (:event event))
      (let [coord  @*coord
            coord' (coords ctx event)]
        (when (not= coord coord')
          (reset! *coord coord')
          (when coord'
            (reset! *preview nil))
          true)))

    (when (and
            (= :mouse-button (:event event))
            (= :primary (:button event))
            (:pressed? event))
      (when-some [[x y] (coords ctx event)]
        (let [tool @*tool
              id   (core/block-at @*picture [x y])]
          (when-some [op (case (first tool)
                           :pcut
                           [:pcut id [x y]]

                           :xcut
                           [:xcut id x]

                           :ycut
                           [:ycut id y]

                           :color
                           (if (< (:x event) -20)
                             ;; picker
                             (let [[r g b] (core/get-color (:problem/bytes @*problem) x y)]
                               (reset! *tool [:color r g b])
                               nil)  
                             ;; fill
                             (let [[_ r g b] tool]
                               [:color id [r g b]]))

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
                               (do
                                 (swap! *tool (fn [_] [:merge]))
                                 [:merge id1 id])
                               (do
                                 (swap! *tool (fn [_] [:merge id]))
                                 nil)))
                           nil)]
            (swap! *log conj op)
            true))))))

(def fill-guides
  (paint/fill 0xFFFF8080))

(def stroke-guides
  (paint/stroke 0xFF804040 2))

(def stroke-cursor
  (paint/stroke 0xFFFF8080 2))

(defn draw-guides [ctx ^Canvas canvas picture]
  (let [{:keys [scale]} ctx]
    (doseq [[id block] picture]
      (let [[l b r t] (:rect block)
            rect      (IRect/makeLTRB (* scale l) (* scale (- 400 t)) (* scale r) (* scale (- 400 b)))]
        (canvas/draw-rect canvas rect stroke-guides)))))

(defn draw-cursor [ctx ^Canvas canvas]
  (let [{:keys [scale]} ctx]
    (when-some [[x y] @*coord]
      (when (not= :ycut (first @*tool))
        (canvas/draw-line canvas (* scale x) 0 (* scale x) (* scale 400) stroke-cursor))
      (when (not= :xcut (first @*tool))
        (canvas/draw-line canvas 0 (* scale (- 400 y)) (* scale 400) (* scale (- 400 y)) stroke-cursor)))))

(defn tool [tool label]
  (ui/dynamic _ [selected? (= tool @*tool)]
    (ui/button
      #(reset! *tool tool)
      {:bg (if selected? 0xFFFED7B2 0xFFB2D7FE)}
      label)))

(defn try-clj! [logs]
  (when-not (str/starts-with? @*status "⏳")
    (reset! *status "⏳")
    (future
      (try
        (let [problem     @*problem
              t0          (System/currentTimeMillis)
              *best-score (volatile! Long/MAX_VALUE)
              *iter       (volatile! 0)]
          (doseq [log logs]
            (let [picture         (transform/transform-all (:problem/picture problem) log)
                  {:keys [score]} (score/score problem log @*best-score)]
              (when (< score @*best-score)
                (vreset! *best-score score)
                (reset! *log log))
              (reset! *status (format "⏳ %,.1f sec, iter %d" (/ (- (System/currentTimeMillis) t0) 1000.0) @*iter))
              (vswap! *iter inc)))
          (reset! *status (format "✅ %,.1f s" (/ (- (System/currentTimeMillis) t0) 1000.0))))
        (catch Throwable t
          (reset! *status (str "☠️ " (.getMessage t)))
          (.printStackTrace t))))))

(defn try-rust! [problem-id algo & args]
  (when-not (str/starts-with? @*status "⏳")
    (reset! *status "⏳")
    (future
      (try
        (let [t0 (System/currentTimeMillis)]
          (core/run!
            {:on-output
             (fn [line]
               (let [[score & log] (str/split line #"\|")
                     log (mapv core/parse-command log)]
                 (reset! *log log)
                 (reset! *status (format "⏳ %,.1f sec" (/ (- (System/currentTimeMillis) t0) 1000.0)))))}
            (concat ["cargo" "-q" "run" "--release" (str problem-id) algo] args))
          (reset! *status (format "✅ in %,.1f s" (/ (- (System/currentTimeMillis) t0) 1000.0))))
        (catch Throwable t
          (reset! *status (str "☠️ " (.getMessage t)))
          (.printStackTrace t))))))

(def app
  (ui/default-theme
    {:hui.text-field/padding-top    10
     :hui.text-field/padding-bottom 10
     :hui.text-field/padding-left   5
     :hui.text-field/padding-right  5}
    (ui/focus-controller
      (ui/padding 20
        (ui/dynamic _ [problem   @*problem
                       gap       10
                       btn-width 100]
          (ui/row
        
            ;; problem image
            (ui/column
              (ui/width 400
                (ui/height 400
                  (core/stack
                    (ui/->AnImage (:problem/image problem))
                    (ui/dynamic _ [picture @*picture]
                      (ui/canvas {:on-paint (fn [ctx canvas size]
                                              (when @*guides?
                                                (draw-guides ctx canvas picture))
                                              (draw-cursor ctx canvas))})))))
              (ui/gap 0 20)
              (ui/row
                (ui/valign 0.5
                  (ui/label "Problem:"))
                   
                (ui/gap 10 0)
                 
                (ui/button
                  #(when-some [n (parse-long (:text @*problem-text))]
                     (when (> n 1)
                       (swap! *problem-text assoc :text (str (dec n)))))
                  (ui/label "←"))
                (ui/gap 10 0)
                (ui/width 60
                  (ui/text-field {:focused? true} *problem-text))
                (ui/gap 10 0)
                (ui/button
                  #(when-some [n (parse-long (:text @*problem-text))]
                     (when (< n 40)
                       (swap! *problem-text assoc :text (str (inc n)))))
                  (ui/label "→"))))
        
            (ui/gap 20 0)
        
            ;; picture
            (ui/dynamic _ [picture @*picture
                           image   @*image
                           {:keys [cost similarity score]} @*score
                           saved (or (core/saved-score (:problem/id problem)) 1000000000)]
              (ui/column
                (ui/width 400
                  (ui/height 400
                    (core/stack
                      (ui/->AnImage image)
                      (ui/canvas {:on-paint (fn [ctx canvas size]
                                              (when @*guides?
                                                (draw-guides ctx canvas picture))
                                              (draw-cursor ctx canvas))
                                  :on-event event}))))
                (ui/gap 0 20)
                (ui/row
                  (ui/column
                    (ui/label "Status:")
                    (ui/gap 0 10)
                    (ui/label "This Score:")
                    (ui/gap 0 10)
                    (ui/label "Saved Score:")
                    (ui/gap 0 10)
                    (ui/label "Similarity:")
                    (ui/gap 0 10)
                    (ui/label "Cost:")
                    (ui/gap 0 10)
                    (ui/label "Mouse:")
                    (ui/gap 0 10)
                    (ui/checkbox
                      *guides?
                      (ui/label "Guides")))
                  (ui/gap 10 0)
                  (ui/column
                    (ui/dynamic _ [status @*status]
                      (ui/label status))
                    (ui/gap 0 10)
                    (ui/label (str score (if (< score saved) " 🔥" "")))
                    (ui/gap 0 10)
                    (ui/label saved)
                    (ui/gap 0 10)
                    (ui/label similarity)
                    (ui/gap 0 10)
                    (ui/label cost)
                    (ui/gap 0 10)
                    (ui/dynamic _ [coord @*coord]
                      (ui/label coord))))))
        
            (ui/gap 20 0)
        
            ;; tools
            [:stretch 1
             (ui/column
               (ui/label "Tools:")
               (ui/gap 0 10)
             
               ;; cuts
               (ui/row
                 (ui/width btn-width
                   (tool [:pcut] (ui/label "╋")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (tool [:xcut] (ui/label "┃")))
                 (ui/gap 10 0)
                 (ui/width btn-width 
                   (tool [:ycut] (ui/label "━"))))

               (ui/gap 0 10)
               (ui/row
                 (ui/width btn-width
                   (ui/dynamic _ [tool @*tool]
                     (if (= :color (first tool))
                       (ui/button
                         (fn [] :nop)
                         {:bg 0xFFFED7B2}
                         (let [[_ r g b] tool]
                           (ui/rect (paint/fill (Color/makeARGB 255 r g b))
                             (ui/gap 30 10))))
                       (ui/button
                         #(reset! *tool [:color 255 255 255])
                         (let [[_ r g b] tool]
                           (ui/rect (paint/fill 0xFFFFFFFF)
                             (ui/gap 30 10)))))))
             
                 (ui/gap 10 0)   
             
                 (ui/width btn-width 
                   (tool [:swap] (ui/label "Swap")))
             
                 (ui/gap 10 0)
             
                 (ui/width btn-width 
                   (tool [:merge] (ui/label "Merge"))))
                      
               (ui/gap 0 20)
           
               ;; Clojure Algos
               (ui/label "Clojure:")
               (ui/gap 0 10)
               (ui/row
                 (ui/width btn-width
                   (ui/button #(try-clj! (algo.ycut/ycut (:problem/bytes problem)))
                     (ui/label "YCut")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (ui/button #(try-clj! (algo.rect/rect (:problem/bytes problem)))
                     (ui/label "Rect")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (ui/button #(try-clj! (algo.swap/swap problem 50 1000))
                     (ui/label "Swap"))))
               
               (ui/gap 0 10)
               (ui/row
                 (ui/width btn-width
                   (ui/button #(try-clj! (algo.merge/merge (:problem/picture problem)))
                     (ui/label "Merge")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (ui/button #(try-clj! (algo.paint/paint problem))
                     (ui/label "Paint")))
                 (ui/gap 10 0))
           
               (ui/gap 0 20)
           
               ;; Rust Algos
               (ui/label "Rust:")
               (ui/gap 0 10)
               (ui/row
                 (ui/width btn-width
                   (ui/button #(try-rust! (:problem/id problem) "xcut")
                     (ui/label "XCut")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (ui/button #(try-rust! (:problem/id problem) "ycut")
                     (ui/label "YCcut")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (ui/button #(try-rust! (:problem/id problem) "rect")
                     (ui/label "Rect"))))
             
               (ui/gap 0 10)
               (ui/row
                 (ui/width btn-width
                   (ui/button #(try-rust! (:problem/id problem) "x3y2")
                     (ui/label "x3y2")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                   (ui/button #(try-rust! (:problem/id problem) "x3y3")
                     (ui/label "x3y3")))
                 (ui/gap 10 0)
                 (ui/width btn-width
                           (ui/button #(try-rust! (:problem/id problem) "grid")
                                      (ui/label "grid"))))

               (ui/gap 0 20)

               ;; Submit area
               (ui/label "Solution:")
               (ui/gap 0 10)
               (ui/row
                 (ui/width btn-width 
                   (ui/button
                     #(let [{:keys [score]} (score/score problem @*log)
                            id   (:problem/id problem)
                            file (core/save id @*log score)
                            resp (core/submit id file)]
                        (reset! *status (format "💌 Sent! %d %s" (:status resp) (:body resp))))
                     {:bg 0xFFB2FEB2}
                     (ui/label "Submit")))
                 (ui/gap 10 0)
                 (ui/width btn-width 
                   (ui/button
                     #(let [{:keys [score]} (score/score problem @*log)]
                        (core/save (:problem/id problem) @*log score))
                     (ui/label "Save")))
                 (ui/gap 10 0)
                 (ui/width btn-width 
                   (ui/button
                     #(do
                        (reset! *log [])
                        (reset! *status "Ready"))
                     {:bg 0xFFFEB2B2}
                     (ui/label "Reset"))))
           
               (ui/gap 0 15)
           
               ;; Log
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
                               (swap! *log #(vec (take idx %))))}
                            (ui/hoverable
                              {:on-hover #(reset! *preview idx)
                               :on-out   (fn []
                                           (swap! *preview #(if (= idx %) nil %)))}
                              (ui/dynamic ctx [hovered? (= idx @*preview)]
                                (let [label (ui/padding 10 10
                                              (ui/label (str idx "> " op)))]
                                  (if hovered?
                                    (ui/rect (paint/fill 0xFFEEEEEE)
                                      label)
                                    label))))))))))])]))))))


(redraw)
