(ns icfpc2022.runner
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clj-http.client :as http]
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.core :as core]
    [icfpc2022.render :as render]
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
    [io.github.humbleui.types IPoint IRect Rect]
    [java.io File]
    [java.util.concurrent Executors ExecutorService]))

(set! *warn-on-reflection* true)

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(def algos
  [#_:xcut #_:ycut #_:rect :x3y2 :x3y3])

(defonce ^ExecutorService executor
  (Executors/newFixedThreadPool (.availableProcessors (Runtime/getRuntime))))

(defn saved-image [problem score]
  (let [log     (->> (slurp (io/file (str "answers/problem " problem "/" score)))
                  (str/split-lines)
                  (mapv core/parse-command))
        picture (reduce render/transform render/start-picture log)]
    (with-open [bitmap (render/render-to-bitmap picture)]
      (Image/makeFromBitmap bitmap))))

(def *problems
  (atom
    (into {}
      (for [problem core/problems
            :let [image       (Image/makeFromEncoded (hui/slurp-bytes (str "resources/" problem ".png")))
                  saved-score (core/saved-score problem)]]
        [problem {:problem {:image image
                            :bytes (render/image-bytes image)}
                  :saved   (when saved-score
                             {:score saved-score
                              :image (saved-image problem saved-score)})}]))))

(add-watch *problems ::redraw
  (fn [_ _ _ _]
    (redraw)))

(defn run-problem! [problem algo]
  (try
    (let [t0 (System/currentTimeMillis)]
      (swap! *problems update-in [problem algo] assoc
        :status "⏳")
      (core/run!
        {:on-output
         (fn [line]
           (let [[score & log] (str/split line #"\|")
                 score   (parse-long score)
                 log     (mapv core/parse-command log)
                 picture (reduce render/transform render/start-picture log)
                 image   (with-open [bitmap (render/render-to-bitmap picture)]
                           (Image/makeFromBitmap bitmap))]
             (swap! *problems update-in [problem algo] assoc
               :score score
               :log   log
               :image image)))}
        ["target/release/brutforce" (str problem) (name algo)])
      (swap! *problems update-in [problem algo] assoc
        :status "☑️")
      (core/log
        (format "[ DONE ] %s %s %d in %,.1f sec" problem algo (get-in @*problems [problem algo :score]) (/ (- (System/currentTimeMillis) t0) 1000.0)))
      
      (let [info   (get @*problems problem)
            score  (:score (get info algo))
            scores (->> (dissoc info algo)
                     (vals)
                     (keep :score)
                     (reduce min Integer/MAX_VALUE))]
        (when (< score scores)
          (let [file (render/dump
                       problem
                       (:bytes (:problem info))
                       (:log (get info algo)))]
            #_(render/submit problem file)))))
    (catch Throwable t
      (.printStackTrace t))))

(defn run-all! []
  (core/run! {} ["cargo" "build" "--release"])
  (doseq [[problem algo] (shuffle
                           (for [problem core/problems
                                 algo    algos]
                             [problem algo]))]
    (.submit executor ^Runnable #(run-problem! problem algo))))

(def padding
  5)

(def big-padding
  30)

(def image-size
  70)

(defn an-image [image]
  (ui/halign 0
    (if image
      (ui/width image-size
        (ui/height image-size
          (ui/->AnImage image)))
      (ui/rect (paint/fill 0xFFCCCCCC)
        (ui/gap image-size image-size)))))

(def app
  (ui/default-theme
    {:hui.text-field/padding-top    10
     :hui.text-field/padding-bottom 10
     :hui.text-field/padding-left   5
     :hui.text-field/padding-right  5}
    (ui/with-bounds ::bounds
      (ui/dynamic ctx [problems  (sort-by first @*problems)
                       partition (quot (:height (::bounds ctx)) (+ image-size padding padding 10))]
        (ui/padding 20
          (ui/row
            (interpose (ui/gap big-padding 0)
              (for [problems (partition-all partition problems)]
                (list
                  ;; problem
                  (ui/column
                    (for [[problem info] problems
                          :let [{:keys [image]} (:problem info)]]
                      (list
                        (an-image image)
                        (ui/gap 0 padding)
                        (ui/label (str "# " problem))
                        (ui/gap 0 padding))))
                
                  (ui/gap padding 0)
                
                  ;; best-saved
                  (ui/column
                    (for [[problem info] problems
                          :let [{:keys [score image]} (:saved info)]]
                      (list
                        (an-image image)
                        (ui/gap 0 padding)
                        (ui/label score)
                        (ui/gap 0 padding))))
                
                  (ui/gap padding 0)
                
                  ;; algos
                  (interpose (ui/gap padding 0)
                    (for [algo algos]
                      (ui/column
                        (for [[problem info] problems
                              :let [{:keys [score status image]} (info algo)
                                    best (->> (dissoc info algo)
                                           (vals)
                                           (keep :score)
                                           (reduce min Integer/MAX_VALUE))]]
                          (list
                            (an-image image)
                            (ui/gap 0 padding)
                            (ui/label (when score
                                        (str status (if (< score best) "🔥" "") score)))
                            (ui/gap 0 padding))))))
                  
                  )))
            (ui/gap big-padding 0)
            (ui/column
              (ui/button run-all! (ui/label "RUN"))
              (ui/gap 0 (* 2 padding))
              (ui/label (str "Saved: "
                          (reduce + 0
                            (for [[problem info] problems]
                              (:score (:saved info))))))
              (ui/gap 0 (* 2 padding))
              (ui/label (str "Total: "
                          (reduce + 0
                            (for [[problem info] problems]
                              (->> (vals info) (keep :score) (reduce min Integer/MAX_VALUE))))))
              [:stretch 1 nil])
              ))))))
          
(redraw)