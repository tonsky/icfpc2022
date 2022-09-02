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
    [io.github.humbleui.skija Canvas]
    [io.github.humbleui.types IPoint IRect Rect]))

(set! *warn-on-reflection* true)

(defonce *coord
  (atom (IPoint. 0 0)))

(defonce *radius
  (atom
    {:value 50
     :min   0
     :max   100}))

(defn event [ctx event]
  (when (= :mouse-move (:event event))
    (reset! *coord (IPoint. (:x event) (:y event)))))

(defn draw [ctx ^Canvas canvas ^IPoint size]
  (let [{:keys [scale]} ctx]
    (with-open [fill   (paint/fill 0xFFFFFFFF)
                stroke (paint/stroke 0xFFCC3333 (* 2 scale))]
      (canvas/draw-rect canvas (IRect/makeXYWH 0 0 (:width size) (:height size)) fill)
      (canvas/draw-circle canvas (:x @*coord) (:y @*coord) (:value @*radius) stroke))))

(def app
  (ui/default-theme
    {:hui.text-field/padding-top    10
     :hui.text-field/padding-bottom 10
     :hui.text-field/padding-left   5
     :hui.text-field/padding-right  5}
    (ui/padding 20
      (ui/row
        [:stretch 5
         (ui/canvas {:on-paint draw
                     :on-event event})]
        (ui/gap 20 0)
        [:stretch 1
         (ui/column
           (ui/row
             (ui/valign 0.5
               (ui/label "Param: "))
             (ui/gap 10 0)
             [:stretch 1
              (ui/text-field {:focused? true}
                (atom {:placeholder "Type here"}))])
           (ui/gap 0 20)
           (ui/checkbox (atom true)
             (ui/label "Optimal?"))
           (ui/gap 0 20)
           (ui/dynamic _ [value (:value @*radius)]
             (ui/label (str "Radius: " value)))
           (ui/gap 0 10)
           (ui/slider *radius)
           (ui/gap 0 20)
           (ui/halign 0
             (ui/button nil
               (ui/label "Calculate"))))]))))

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(redraw)
