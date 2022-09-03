(ns icfpc2022.runner
  (:refer-clojure :exclude [run!])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn run! [opts cmd]
  (let [{:keys [on-output]} opts
        pb (-> (ProcessBuilder. (vec cmd))
             (.directory (io/file "brutforce"))
             (.redirectErrorStream true))
        proc (.start pb)]
    (doseq [line (line-seq (.inputReader proc))]
      (println ">>>" line)
      (on-output line))
    (println "Process" cmd "exited with code" (.waitFor proc))))

(comment
  (run! {:on-output println} "cargo" "run" "--release" "19"))
        
    
