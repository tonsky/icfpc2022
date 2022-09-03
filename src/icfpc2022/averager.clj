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

(defn average [colors]
  (let [[r g b a] (reduce
                    (fn [[r g b a] [r' g' b' a']]
                      [(+ r r') (+ g g') (+ b b') (+ a a')])
                    [0 0 0 0] colors)]
    [(int (/ r (count colors)))
     (int (/ g (count colors)))
     (int (/ b (count colors)))
     (int (/ a (count colors)))]))

(defn most-common [colors]
  (first
    (reduce-kv
      (fn [[col cnt] col' cnt']
        (if (> cnt' cnt)
          [col' cnt']
          [col cnt]))
      [[0 0 0 0] -1]
      (frequencies colors))))

(defn algo-average [^bytes bytes]
  (let [colors (for [x (range 0 400)
                     y (range 0 400)]
                 (render/get-color bytes x y))]
    [[:color "0" (average colors)]]))

(defn algo-common [^bytes bytes]
  (let [colors (for [x (range 0 400)
                     y (range 0 400)]
                 (render/get-color bytes x y))]
    [[:color "0" (most-common colors)]]))

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
                 [:color id (average colors)])]
    (concat
      log
      colors)))

(comment
  (algo-grid nil))

(defn -main [& args]
  (doseq [problem (->> (file-seq (io/file "resources"))
                    (keep #(second (re-matches #"(\d+)\.png" (.getName %))))
                    (map parse-long)
                    sort)]
    (with-open [image  (Image/makeFromEncoded (core/slurp-bytes (str "resources/" problem ".png")))
                bitmap (Bitmap.)]
      (let [image-info  (ImageInfo. 400 400 ColorType/RGBA_8888 ColorAlphaType/OPAQUE (ColorSpace/getSRGB))
            _           (.allocPixels bitmap image-info)
            _           (.readPixels image bitmap)
            bytes       (.readPixels bitmap)
            algos       [algo-average
                         algo-common
                         algo-grid]
            results     (map #(let [log (% bytes)] [log (render/score log bytes)]) algos)
            [log score] (apply min-key second results)
            solutions   (->> (file-seq (io/file (str "answers/problem " problem)))
                          (keep #(parse-long (.getName %))))]
        (println "Problem" problem "before:" solutions "now:" (map second results))
        (when (or (empty? solutions)
                (< score (reduce min solutions)))
          (println "  writing answers/problem " problem "/" score)
          (.mkdirs (io/file (str "answers/problem " problem)))
          (spit (io/file (str "answers/problem " problem "/" score))
            (render/solution log)))))))

(comment
  (-main)
  
  (submit 5 "answers/problem 5/42760")
  (submit 6 "answers/problem 6/95781")
  (submit 7 "answers/problem 7/95990")
  (submit 8 "answers/problem 8/21911")
  (submit 10 "answers/problem 10/72254")
  (submit 12 "answers/problem 12/24775")
  (submit 14 "answers/problem 14/45204")
  (submit 15 "answers/problem 15/62157")
  (submit 19 "answers/problem 19/75547")
  (submit 20 "answers/problem 20/75784")
  (submit 22 "answers/problem 22/54943")
  (submit 23 "answers/problem 23/93958")
  (submit 24 "answers/problem 24/46778"))
