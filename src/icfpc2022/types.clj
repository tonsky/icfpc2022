(ns icfpc2022.types)

(defrecord SimpleBlock 
  [rect    ;; [l b r t]
   color]) ;; [r g b a]

(defrecord ComplexBlock
  [rect       ;; [l b r t]
   children]) ;; [SimpleBlock ...]
