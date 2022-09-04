(ns icfpc2022.main
  (:require
    [clojure.string :as str]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.debug :as debug]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.types IRect]))

(set! *warn-on-reflection* true)

(defonce *window
  (atom nil))

(defonce **app
  (atom nil))

(defn ctx [window]
  (when-not (window/closed? window)
    {:window window
     :scale  (window/scale window)}))

(defn on-paint [window canvas]
  (canvas/clear canvas 0xFFF6F6F6)
  (let [bounds (window/content-rect window)]
    (core/draw @@**app (ctx window) (IRect/makeXYWH 0 0 (:width bounds) (:height bounds)) canvas)))

(defn on-event [window event]
  (when-let [result (core/event @@**app (ctx window) event)]
    (window/request-frame window)
    result))

(defn make-window []
  (let [screen  (first (app/screens))
        scale   (:scale screen)
        area    (:work-area screen)
        width   (* (:width area) 0.95)
        height  (* (:height area) 0.95)
        x       (+ (:x area) (/ (- (:width area) width) 2))
        y       (+ (:y area) (/ (- (:height area) height) 2))
        window  (window/make
                  {:on-close #(System/exit 0)
                   :on-paint #'on-paint
                   :on-event #'on-event})]
    ; (window/set-z-order window :floating)
    (reset! debug/*enabled? true)
    (window/set-title window "ICFPC 2022 × Humble UI 🐝")
    (when (= :macos app/platform)
      (window/set-icon window "resources/icon.icns"))
    (window/set-window-size window width height)
    (window/set-window-position window x y)
    (window/set-visible window true)))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (reset! **app
    (case (first args)
      "runner" (requiring-resolve 'icfpc2022.runner/app)
      (requiring-resolve 'icfpc2022.render/app)))
  (app/start #(reset! *window (make-window))))
