(ns icfpc2022.averager
  (:require
    [clj-http.client :as http]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [icfpc2022.render :as render]
    [io.github.humbleui.core :as core])
  (:import
    [io.github.humbleui.skija Bitmap Canvas Color ColorAlphaType ColorSpace ColorType Image ImageInfo]
    [io.github.humbleui.types IPoint IRect Rect]))

(defn submit [problem solution]
  (http/post (str "https://robovinci.xyz/api/submissions/" problem "/create")
    {:headers {"Authorization" (str "Bearer " (slurp "api_token"))}
     :multipart [{:name "file" :content (io/file solution)}]}))

(defn algo-average [^bytes bytes]
  (let [colors (for [x (range 0 400)
                     y (range 0 400)]
                 (render/get-color bytes x y))]
    [[:color "0" (render/average colors)]]))

(defn algo-common [^bytes bytes]
  (let [colors (for [x (range 0 400)
                     y (range 0 400)]
                 (render/get-color bytes x y))]
    [[:color "0" (render/most-common colors)]]))

(defn split [id l b r t]
  (if (> (- r l) 25)
    (let [x (/ (+ l r) 2)
          y (/ (+ t b) 2)]
      (concat
        [[:pcut id [x y]]]
        (split (str id ".0") l b x y)
        (split (str id ".1") x b r y)
        (split (str id ".2") x y r t)
        (split (str id ".3") l y x t)))
    []))

(defn algo-grid [^bytes bytes]
  (let [log (split "0" 0 0 400 400)
        fx  {0 0 1 1 2 1 3 0}
        fy  {0 0 1 0 2 1 3 1}
        colors (for [l1 (range 4)
                     l2 (range 4)
                     l3 (range 4)
                     l4 (range 4)
                     :let [id (str/join "." ["0" l1 l2 l3 l4])
                           l  (+ 
                                (* (fx l1) 200)
                                (* (fx l2) 100)
                                (* (fx l3) 50)
                                (* (fx l4) 25))
                           b  (+ 
                                (* (fy l1) 200)
                                (* (fy l2) 100)
                                (* (fy l3) 50)
                                (* (fy l4) 25))
                           r  (+ l 25)
                           t  (+ b 25)
                           colors (for [x (range l r)
                                        y (range b t)]
                                    (render/get-color bytes x y))]]
                 [:color id (render/average colors)])]
    (concat
      log
      colors)))

(defn min-by [k & args]
  (apply min-key k (filter #(some? (k %)) args)))

(defn algo-divide
  ([^bytes bytes]
   (let [*cache (volatile! {})
         {:keys [ops score]} (algo-divide bytes "0" [:rect 0 0 400 400] [255 255 255 255] false *cache)]
     (println (str/join "\n" ops))
     (println score)
     ops))
  ([^bytes bytes id [shape l b r t] color colored? *cache]
   (render/get-cached *cache [:all id [shape l b r t] color]
     (apply println [:all id [shape l b r t] color])
     (let [colors (render/get-cached *cache [:colors [shape l b r t]]
                    (vec
                      (for [x (range l r)
                            y (range b t)]
                        (render/get-color bytes x y))))]
       (min-by :score
         {:ops   []
          :score (render/similarity bytes (constantly color) [l b r t])}
         (when-not colored?
           (let [color' (render/get-cached *cache [:average [shape l b r t]]
                          (render/average colors))
                 {:keys [ops score]} (algo-divide bytes id [shape l b r t] color' true *cache)]
             {:ops   (cons [:color id color'] ops)
              :score (+ (render/op-cost :color [shape l b r t]) score)}))
         (when-not colored?
           (let [color' (render/get-cached *cache [:common [shape l b r t]]
                          (render/most-common colors))
                 {:keys [ops score]} (algo-divide bytes id [shape l b r t] color' true *cache)]
             {:ops   (cons [:color id color'] ops)
              :score (+ (render/op-cost :color [shape l b r t]) score)}))
         (when (>= (- r l) 100)
           (let [x     (quot (+ r l) 2)
                 left  (algo-divide bytes (str id ".0") [shape l b x t] color false *cache)
                 right (algo-divide bytes (str id ".1") [shape x b r t] color false *cache)]
             {:ops   (concat
                       [[:xcut id x]]
                       (:ops left)
                       (:ops right))
              :score (+ (render/op-cost :xcut [shape l b r t])
                       (:score left)
                       (:score right))}))
         (when (>= (- t b) 100)
           (let [y      (quot (+ t b) 2)
                 bottom (algo-divide bytes (str id ".0") [shape l b r y] color false *cache)
                 top    (algo-divide bytes (str id ".1") [shape l y r t] color false *cache)]
             {:ops   (concat
                       [[:ycut id y]]
                       (:ops bottom)
                       (:ops top))
              :score (+ (render/op-cost :ycut [shape l b r t])
                       (:score bottom)
                       (:score top))})))))))

(comment
  (algo-grid nil))

(defn -main [& args]
  (doseq [problem (->> (file-seq (io/file "resources"))
                    (keep #(second (re-matches #"(\d+)\.png" (.getName %))))
                    (map parse-long)
                    sort
                    (drop 10)
                    (take 1))]
    (with-open [image  (Image/makeFromEncoded (core/slurp-bytes (str "resources/" problem ".png")))
                bitmap (Bitmap.)]
      (let [image-info  (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))
            _           (.allocPixels bitmap image-info)
            _           (.readPixels image bitmap)
            bytes       (.readPixels bitmap)
            algos       [#_algo-average
                         #_algo-common
                         #_algo-grid
                         algo-divide]
            results     (keep #(when-some [log (not-empty (% bytes))]
                                 [log (render/score log bytes)])
                          algos)]
        (when (not-empty results)
          (let [[log score] (apply min-key second results)
                solutions   (->> (file-seq (io/file (str "answers/problem " problem)))
                              (keep #(parse-long (.getName %))))]
            (println "Problem" problem "before:" solutions "now:" (map second results))
            (when (or (empty? solutions)
                    (< score (reduce min solutions)))
              (println "  writing answers/problem " problem "/" score)
              (.mkdirs (io/file (str "answers/problem " problem)))
              (spit (io/file (str "answers/problem " problem "/" score))
                (render/solution log)))))))))

(comment
  (-main)
  
  (submit 2 "answers/problem 2/9695")
  (submit 5 "answers/problem 5/28950")
  (submit 6 "answers/problem 6/17506")
  (submit 7 "answers/problem 7/53951")
  (submit 8 "answers/problem 8/21911")
  (submit 9 "answers/problem 9/31638")
  (submit 10 "answers/problem 10/53728")
  (submit 11 "answers/problem 11/72706")
  (submit 12 "answers/problem 12/24775")
  (submit 13 "answers/problem 13/28462")
  (submit 14 "answers/problem 14/45204")
  (submit 15 "answers/problem 15/62157")
  (submit 16 "answers/problem 16/32828")
  (submit 17 "answers/problem 17/45519")
  (submit 19 "answers/problem 19/75547")
  (submit 20 "answers/problem 20/28682")
  (submit 21 "answers/problem 21/47395")
  (submit 22 "answers/problem 22/54943")
  (submit 23 "answers/problem 23/37399")
  (submit 24 "answers/problem 24/30480")
  (submit 25 "answers/problem 25/43120"))
