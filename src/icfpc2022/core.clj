(ns icfpc2022.core
  (:refer-clojure :exclude [run!])
  (:require
    [cheshire.core :as json]
    [clj-http.client :as http]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.types :as types]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as hui]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.protocols :as protocols])
  (:import
    [icfpc2022.types SimpleBlock ComplexBlock]
    [io.github.humbleui.skija Bitmap Canvas Color ColorAlphaType ColorSpace ColorType Image ImageInfo]
    [io.github.humbleui.types IPoint IRect]
    [java.io File]
    [java.lang ProcessBuilder$Redirect]
    [java.util List]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(set! *assert* false)

(def sample-rate
  5)

(defonce lock
  (Object.))

(defn log [& msg]
  (locking lock
    (apply println msg)
    (flush)))

(defn round-to [x v]
  (-> x
    (/ v)
    (math/round)
    (* v)
    (int)))

(defmacro byte->long [n]
  `(-> ~n (+ 256) (mod 256)))

(defn inside? [[l b r t] [x y]]
  (and
    (<= l x)
    (< x r)
    (<= b y)
    (< y t)))

(defn rrange
  ([from to]
   (rrange from to 1))
  ([from to step]
   (range (- to step) (dec from) (- step))))

(defn simple? [block]
  (assert (or 
            (instance? SimpleBlock block)
            (instance? ComplexBlock block)))
  (instance? SimpleBlock block))

(def *cache
  (volatile! {}))

(defmacro cached [key & body]
  `(let [key# ~key]
     (or
       (@*cache key#)
       (let [val# (do ~@body)]
         (vswap! *cache assoc key# val#)
         val#))))

(defn run! [opts cmd]
  (log "[ RUN ]" cmd)
  (let [t0 (System/currentTimeMillis)
        {:keys [on-output]} opts
        cmd ^List (vec cmd)
        pb (-> (ProcessBuilder. cmd)
             (.directory (io/file "brutforce"))
             (.redirectError ProcessBuilder$Redirect/INHERIT))
        proc (.start pb)]
    (doseq [line (line-seq (.inputReader proc))]
      (when on-output
        (on-output line)))
    (log (format "[ DONE ] Process %s exited with code %d in %,.1f sec" cmd (.waitFor proc) (/ (- (System/currentTimeMillis) t0) 1000.0)))))

(defn parse-command [cmd]
  (condp re-matches cmd
    #"cut \[([0-9.]+)\] \[(\d+), (\d+)\]"
    :>> (fn [[_ id x y]]
          [:pcut id [(parse-long x) (parse-long y)]])
    
    #"cut \[([0-9.]+)\] \[[xX]\] \[(\d+)\]"
    :>> (fn [[_ id x]]
          [:xcut id (parse-long x)])
    
    #"cut \[([0-9.]+)\] \[[yY]\] \[(\d+)\]"
    :>> (fn [[_ id y]]
          [:ycut id (parse-long y)])
    
    #"color \[([0-9.]+)\] \[(\d+), (\d+), (\d+), (\d+)\]"
    :>> (fn [[_ id r g b a]]
          [:color id [(parse-long r) (parse-long g) (parse-long b)]])
    
    #"swap \[([0-9.]+)\] \[([0-9.]+)\]"
    :>> (fn [[_ id1 id2]]
          [:swap id1 id2])
    
    #"merge \[([0-9.]+)\] \[([0-9.]+)\]"
    :>> (fn [[_ id1 id2]]
          [:merge id1 id2])))

(def problems
  (->> (file-seq (io/file "resources"))
    (keep #(second (re-matches #"(\d+)\.png" (.getName ^File %))))
    (map parse-long)
    (sort)))

(defn saved-score [problem-id]
  (some->> (file-seq (io/file (str "answers/problem " problem-id)))
    (keep #(parse-long (.getName ^File %)))
    (not-empty)
    (reduce min)))

(defn parse-json [file]
  (let [json (json/parse-string file true)]
    (into {}
      (for [block (:blocks json)
            :let [{:keys [blockId bottomLeft topRight color pngBottomLeftPoint]} block
                  [l b] bottomLeft
                  [r t] topRight
                  [red green blue] (if color
                                     color
                                     [255 0 0])]]
        [blockId (SimpleBlock. [l b r t] [red green blue])]))))

(defn image-bytes [^Image image]
  (with-open [bitmap (Bitmap.)]
    (let [image-info (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))]
      (.allocPixels bitmap image-info)
      (.readPixels image bitmap)
      (.readPixels bitmap))))

(defn load-problem [id]
  (let [image   (Image/makeFromEncoded (hui/slurp-bytes (str "resources/" id ".png")))
        json    (io/file (str "resources/" id ".initial.json"))
        picture (if (.exists json)
                  (parse-json (slurp json))
                  {"0" (SimpleBlock. [0 0 400 400] [255 255 255])})]
    {:problem/id      id
     :problem/image   image
     :problem/bytes   (image-bytes image)
     :problem/picture picture}))

(defn block-at [picture [x y]]
  (reduce-kv 
    (fn [acc id block]
      (when (inside? (:rect block) [x y])
        (reduced id)))
    nil picture))

(defn get-color [^bytes bytes x y]
  (let [idx (* 4 (+ x (* 400 (- 399 y))))
        r   (byte->long (aget bytes (+ idx 0)))
        g   (byte->long (aget bytes (+ idx 1)))
        b   (byte->long (aget bytes (+ idx 2)))]
    [r g b]))

(defn colors [^bytes bytes [l b r t]]
  (for [x (range l r sample-rate)
        y (rrange b t sample-rate)]
    (get-color bytes x y)))

(defn average [colors]
  (let [[r g b] (reduce
                  (fn [[r g b] [r' g' b']]
                    [(+ r r') (+ g g') (+ b b')])
                  [0 0 0 0] colors)
        len (count colors)]
    [(int (/ r len))
     (int (/ g len))
     (int (/ b len))]))

(defn most-common [colors]
  (->> (frequencies colors)
    (apply max-key second)
    (first)))

(defn color-variants [colors]
  [(average colors)
   (most-common colors)])

(defn draw-block [^Canvas canvas block]
  (if (instance? ComplexBlock block)
    (doseq [child (:children block)]
      (draw-block canvas child))
    (let [[l b r t] (:rect block)
          rect (IRect/makeLTRB l (- 400 t) r (- 400 b))
          [red green blue] (:color block)]
      (with-open [fill (paint/fill (Color/makeARGB 255 red green blue))]
        (canvas/draw-rect canvas rect fill)))))

(defn ^Bitmap render-to-bitmap [picture]
  (let [bitmap     (Bitmap.)
        image-info (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))]
    (.allocPixels bitmap image-info)
    (let [canvas (Canvas. bitmap)]
      (.clear canvas (unchecked-int 0xFFFFFFFF))
      (doseq [[_ block] picture]
        (draw-block canvas block)))
    bitmap))

(defn ^bytes picture-pixels [picture]
  (with-open [bitmap (render-to-bitmap picture)]
    (.readPixels bitmap)))

(defn serialize [log]
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
        (let [[_ id [r g b]] op]
          (format "color [%s] [%d, %d, %d, %d]" id r g b 255))

        :swap
        (let [[_ id1 id2] op]
          (format "swap [%s] [%s]" id1 id2))

        :merge
        (let [[_ id1 id2] op]
          (format "merge [%s] [%s]" id1 id2))))))

(defn save [problem-id a-log score]
  (let [serialized (serialize a-log)
        dir        (str "answers/problem " problem-id)
        file       (io/file dir (str score))]
    (log "Writing" file)
    (.mkdirs (io/file dir))
    (spit file serialized)
    file))

(defn submit [problem-id content]
  (http/post (str "https://robovinci.xyz/api/submissions/" problem-id "/create")
    {:headers {"Authorization" (str "Bearer " (slurp "api_token"))}
     :multipart [{:name "file" :content content}]}))  

(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (.printStackTrace ^Throwable ex))))

(defmacro thread [& body]
  `(future
     (try
       ~@body
       (catch Throwable t#
         (.printStackTrace t#)))))

(hui/deftype+ Stack [children ^:mut my-rect]
  protocols/IComponent
  (-measure [_ ctx cs]
    (reduce
      (fn [size child]
        (let [{:keys [width height]} (hui/measure child ctx cs)]
          (IPoint. (max (:width size) width) (max (:height size) height))))
      (IPoint. 0 0) children))
  
  (-draw [_ ctx ^IRect rect ^Canvas canvas]
    (set! my-rect rect)
    (doseq [child children]
      (hui/draw-child child ctx rect canvas)))
  
  (-event [_ ctx event]
    (reduce hui/eager-or false
      (for [child children]
        (hui/event-child child ctx event))))
  
  (-iterate [this ctx cb]
    (or
      (cb this)
      (some #(protocols/-iterate % ctx cb) children))))

(defn stack [& children]
  (->Stack children nil))
