(ns icfpc2022.core
  (:refer-clojure :exclude [run!])
  (:require
    [clojure.java.io :as io])
  (:import
    [java.io File]))

(defonce lock
  (Object.))

(defn log [& msg]
  (locking lock
    (apply println msg)
    (flush)))

(defn run! [opts cmd]
  (log "[ RUN ]" cmd)
  (let [t0 (System/currentTimeMillis)
        {:keys [on-output]} opts
        pb (-> (ProcessBuilder. (vec cmd))
             (.directory (io/file "brutforce"))
             (.redirectErrorStream true))
        proc (.start pb)]
    (doseq [line (line-seq (.inputReader proc))]
      (when on-output
        (on-output line)))
    #_(log (format "[ DONE ] Process %s exited with code %d in %,.1f sec" cmd (.waitFor proc) (/ (- (System/currentTimeMillis) t0) 1000.0)))))

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
          [:color id [(parse-long r) (parse-long g) (parse-long b) (parse-long a)]])
    
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

(defn saved-score [problem]
  (some->> (file-seq (io/file (str "answers/problem " problem)))
    (keep #(parse-long (.getName ^File %)))
    (not-empty)
    (reduce min)))

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