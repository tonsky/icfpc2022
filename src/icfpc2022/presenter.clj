(ns icfpc2022.presenter
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as hui]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.protocols :as protocols]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui])
  (:import
    [io.github.humbleui.skija Bitmap Canvas Color ColorAlphaType ColorSpace ColorType Image ImageInfo]
    [io.github.humbleui.types IPoint IRect Rect]
    [java.io File]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(def *state
  (atom nil))

(def fill-guides
  (paint/fill 0xFFFF8080))

(def stroke-guides
  (paint/stroke 0xFF804040 2))

(defn draw-guides [ctx ^Canvas canvas picture]
  (let [{:keys [scale]} ctx]
    (doseq [[id block] picture]
      (let [[l b r t] (:rect block)
            rect      (IRect/makeLTRB (* scale l) (* scale (- 400 t)) (* scale r) (* scale (- 400 b)))]
        (canvas/draw-rect canvas rect stroke-guides)))))

(def *guides?
  (atom false))

(defn start! []
  (core/thread
    (doseq [id (range 1 41)]
      (let [problem   (core/load-problem id)
            solutions (some->> (file-seq (io/file (str "answers/problem " id)))
                        (keep #(parse-long (.getName ^File %)))
                        (sort)
                        (reverse))]
        (reset! *state problem)
        (redraw)
        (Thread/sleep 1000)
        
        (doseq [score (butlast solutions)]
          (let [log (->> (slurp (str "answers/problem " id "/" score))
                      (str/split-lines)
                      (mapv core/parse-command))]
            (swap! *state assoc :log log))
          (redraw)
          (Thread/sleep 200))
        
        (reset! *guides? true)
        
        (let [log (->> (slurp (str "answers/problem " id "/" (last solutions)))
                      (str/split-lines)
                      (mapv core/parse-command))
              dt  (quot 2000 (count log))]
          (doseq [i (range 1 (inc (count log)))]
            (swap! *state assoc :log (take i log))
            (redraw)
            (Thread/sleep dt)))
        
        (Thread/sleep 200)

        (reset! *guides? false)
        (redraw)
        
        (Thread/sleep 800)))))

(def app
  (ui/default-theme
    {:hui.text-field/padding-top    10
     :hui.text-field/padding-bottom 10
     :hui.text-field/padding-left   5
     :hui.text-field/padding-right  5}
    (ui/valign 0.5
      (ui/halign 0.5
        (ui/dynamic _ [state @*state]
          (if-some [{:keys [problem/id problem/image problem/picture log]} state]
            (ui/row
              (ui/column
                (ui/width 400
                  (ui/height 400
                    (ui/->AnImage image)))
                (ui/gap 0 20)
                (ui/halign 0
                  (ui/label (str "Problem " id))))
              
              (ui/gap 20 0)
          
              (if log
                (let [picture' (transform/transform-all picture log)
                      score    (score/score state log)
                      image    (with-open [bitmap (core/render-to-bitmap picture')]
                                 (Image/makeFromBitmap bitmap))]
                  (ui/column
                    (ui/width 400
                      (ui/height 400
                        (core/stack
                          (ui/->AnImage image)
                          (ui/canvas {:on-paint (fn [ctx canvas size]
                                                  (when @*guides?
                                                    (draw-guides ctx canvas picture')))}))))
                    (ui/gap 0 20)
                    (ui/halign 1
                      (ui/label (:score score)))))
                #_(ui/rect (paint/fill 0xCCCCCC)
                  (ui/gap 400 400))))
            (ui/button
              start!
              (ui/label "Begin"))))))))

(redraw)
