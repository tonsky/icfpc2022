(ns icfpc2022.algo.swap
  (:require
    [icfpc2022.core :as core]
    [icfpc2022.score :as score]
    [icfpc2022.transform :as transform]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defn gen-swap [problem iters]
  (let [{:problem/keys [picture bytes]} problem
        ids (vec (keys picture))]
    (loop [picture' picture
           log      []
           iter     0]
      (if (> iter iters)
        log
        (let [id-from (rand-nth ids)
              id-to   (rand-nth ids)]
          (if (= id-from id-to)
            (recur picture' log (inc iter))
            (let [{color-from :color
                   rect-from :rect} (picture' id-from)
                  {color-to :color
                   rect-to :rect} (picture' id-to)
                  sim-before (+
                               (score/color-similarity bytes color-from rect-from)
                               (score/color-similarity bytes color-to rect-to))
                  sim-after  (+
                               (score/color-similarity bytes color-from rect-to)
                               (score/color-similarity bytes color-to rect-from))]
              (if (> (- sim-before sim-after) (score/op-cost :swap rect-from))
                (let [op [:swap id-from id-to]
                      picture'' (transform/transform picture' op)]
                  (recur picture'' (conj log op) (inc iter)))
                (recur picture' log (inc iter))))))))))

(defn swap [problem logs iters]
  (filter seq
    (repeatedly logs
      #(gen-swap problem iters))))
