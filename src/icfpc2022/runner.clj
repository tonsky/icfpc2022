(ns icfpc2022.runner
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clj-http.client :as http]
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.algo.grid :as algo.grid]
    [icfpc2022.core :as core]
    [icfpc2022.render :as render]
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
    [io.github.humbleui.types IPoint IRect Rect]
    [java.io File]
    [java.util.concurrent Executors ExecutorService]))

(set! *warn-on-reflection* true)

(defn redraw []
  (some-> (resolve 'icfpc2022.main/*window) deref deref window/request-frame))

(def algos
  [:grid #_:xcut #_:ycut #_:rect #_:x3y2 #_:x3y3])

(defonce ^ExecutorService executor
  (Executors/newFixedThreadPool (.availableProcessors (Runtime/getRuntime))))

(defn saved-image [problem score]
  (let [log     (->> (slurp (io/file (str "answers/problem " (:problem/id problem) "/" score)))
                  (str/split-lines)
                  (mapv core/parse-command))
        picture (transform/transform-all (:problem/picture problem) log)]
    (with-open [bitmap (core/render-to-bitmap picture)]
      (Image/makeFromBitmap bitmap))))

(def *problems
  (atom
    (into {}
      (for [id    core/problems
            ; :when (<= 36 id 40)
            :let  [problem     (core/load-problem id)
                   saved-score (core/saved-score id)]]
        [id {:problem problem
             :saved   (when saved-score
                        {:score saved-score
                         :image (saved-image problem saved-score)})}]))))

(add-watch *problems ::redraw
  (fn [_ _ _ _]
    (redraw)))

(defn run-rust! [problem algo]
  (try
    (let [t0 (System/currentTimeMillis)
          problem-id (:problem/id problem)]
      (swap! *problems update-in [problem-id algo] assoc
        :status "⏳")
      (core/run!
        {:on-output
         (fn [line]
           (let [[score & log] (str/split line #"\|")
                 score   (parse-long score)
                 log     (mapv core/parse-command log)
                 picture (transform/transform-all (:problem/picture problem) log)
                 image   (with-open [bitmap (core/render-to-bitmap picture)]
                           (Image/makeFromBitmap bitmap))]
             (swap! *problems update-in [problem-id algo] assoc
               :score score
               :log   log
               :image image)))}
        ["target/release/brutforce" (str problem-id) (name algo)])
      (swap! *problems update-in [problem-id algo] assoc
        :status "☑️")
      (core/log
        (format "[ DONE ] %s %s %d in %,.1f sec" problem-id algo (get-in @*problems [problem-id algo :score]) (/ (- (System/currentTimeMillis) t0) 1000.0)))
      
      #_(let [info   (get @*problems problem-id)
              score  (:score (get info algo))
              scores (->> (dissoc info algo)
                       (vals)
                       (keep :score)
                       (reduce min Integer/MAX_VALUE))]
          (when (< score scores)
            (let [file (render/dump
                         problem-id
                         (:problem/bytes (:problem info))
                         (:log (get info algo)))]
              (render/submit problem-id file)))))
    (catch Throwable t
      (.printStackTrace t))))

(defn run-clj! [problem algo logs-fn]
  (try
    (let [t0 (System/currentTimeMillis)
          problem-id (:problem/id problem)]
      (swap! *problems update-in [problem-id algo] assoc
        :status "⏳")
      
      (doseq [log (logs-fn)]
        (let [log  (vec log)
              best (get-in @*problems [problem-id algo :score] Long/MAX_VALUE)
              {:keys [score]} (score/score problem log best)]
          (when (< score best)
            (let [picture (transform/transform-all (:problem/picture problem) log)
                  image   (with-open [bitmap (core/render-to-bitmap picture)]
                            (Image/makeFromBitmap bitmap))]
              (swap! *problems update-in [problem-id algo] assoc
                :score score
                :log   log
                :image image)))))
      (swap! *problems update-in [problem-id algo] assoc :status "☑️")
      (let [best (get-in @*problems [problem-id algo :score])
            time (/ (- (System/currentTimeMillis) t0) 1000.0)]
        (core/log (format "[ DONE ] %s %s %d in %,.1f sec" problem-id algo best time)))
      
      (let [info   (get @*problems problem-id)
            score  (:score (get info algo))
            scores (->> (dissoc info algo)
                     (vals)
                     (keep :score)
                     (reduce min Integer/MAX_VALUE))]
        (when (< score scores)
          (let [file (core/save problem-id (:log (get info algo)) score)
                resp (core/submit problem-id file)]
            (core/log (format "💌 Sent! %d with score %d, status: %d, resp: %s" problem-id score (:status resp) (:body resp)))))))
    (catch Throwable t
      (.printStackTrace t))))

(defmulti run-problem!
  (fn [problem algo]
    algo))

(defmethod run-problem! :grid [problem-id _]
  (let [problem (get-in @*problems [problem-id :problem])]
    (run-clj! problem :grid
      #(algo.grid/logs problem 10 10 10000))))

(defn run-all! []
  #_(core/run! {} ["cargo" "build" "--release"])
  (doseq [[problem-id algo] (shuffle
                              (for [problem-id (keys @*problems)
                                    algo       algos]
                                [problem-id algo]))]
    (.submit executor ^Runnable #(run-problem! problem-id algo))))

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
                       partition (quot (- (:height (::bounds ctx)) 40) (+ image-size padding padding 10))]
        (ui/padding 20
          (ui/row
            (interpose (ui/gap big-padding 0)
              (for [problems (partition-all partition problems)]
                (list
                  ;; problem
                  (ui/column
                    (for [[problem-id info] problems
                          :let [{:problem/keys [image]} (:problem info)]]
                      (list
                        (an-image image)
                        (ui/gap 0 padding)
                        (ui/label (str "# " problem-id))
                        (ui/gap 0 padding))))
                
                  (ui/gap padding 0)
                
                  ;; best-saved
                  (ui/column
                    (for [[_ info] problems
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
                        (for [[_ info] problems
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
              (let [saved (reduce + 0
                            (for [[problem info] problems]
                              (:score (:saved info))))
                    total (reduce + 0
                            (for [[problem info] problems]
                              (->> (vals info) (keep :score) (reduce min Integer/MAX_VALUE))))]
                (ui/label (format "Delta: %,d" (- total saved))))
              [:stretch 1 nil])
            ))))))
          
(redraw)