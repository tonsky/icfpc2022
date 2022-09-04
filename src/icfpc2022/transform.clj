(ns icfpc2022.transform
  (:require
    [clojure.string :as str]
    [icfpc2022.types :as types])
  (:import
    [icfpc2022.types ComplexBlock SimpleBlock]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(defmulti transform ; => picture'
  (fn [picture op]
    (first op)))

(defn transform-all [picture log]
  (reduce transform picture log))

(defn pcut-rect [[l b r t] [x y]]
  (assert (and (< x r)
            (< l x)
            (< y t)
            (< b y)))
  [[l b x y]
   [x b r y]
   [x y r t]
   [l y x t]])

(defn xcut-rect [[l b r t] x]
  (assert (and (< x r) (< l x)))
  [[l b x t]
   [x b r t]])

(defn ycut-rect [[l b r t] y]
  (assert (and (< y t) (< b y)))
  [[l b r y]
   [l y r t]])

(defn intersect-rects [[l1 b1 r1 t1] [l2 b2 r2 t2]]
  (if (or (<= r1 l2) (<= r2 l1) (<= t1 b2) (<= t2 b1))
    nil
    [(max l1 l2) (max b1 b2) (min r1 r2) (min t1 t2)]))

(defn cut-block [block rect-cut-fn]
  (let [rects (rect-cut-fn (:rect block))]
    (cond
      (instance? SimpleBlock block)
      (mapv #(SimpleBlock. % (:color block)) rects)

      (instance? ComplexBlock block)
      (let [children (:children block)]
        (mapv
          (fn [rect]
            (let [children' (keep (fn [child]
                                    (when-some [rect' (intersect-rects (:rect child) rect)]
                                      (SimpleBlock. rect' (:color child))))
                              children)]
              (ComplexBlock. rect (vec children'))))
          rects))

      :else
      (throw (ex-info (str "Unexpected block type: " block) {:block block})))))

(defn pcut-block [block [x y]]
  (cut-block block #(pcut-rect % [x y])))

(defn xcut-block [block x]
  (cut-block block #(xcut-rect % x)))

(defn ycut-block [block y]
  (cut-block block #(ycut-rect % y)))

(defmethod transform :color [picture [_ id color]]
  (assert (contains? picture id) (str ":color No block " id))
  (let [block (picture id)]
    (assoc picture id (SimpleBlock. (:rect block) color))))

(defmethod transform :pcut [picture [_ id [x y] :as cmd]]
  (assert (contains? picture id) (str ":pcut No block " id))
  (let [block      (picture id)
        new-blocks (->> (pcut-block block [x y])
                     (map-indexed (fn [i block] [(str id "." i) block])))]
    (-> picture
      (dissoc id)
      (into new-blocks))))

(defmethod transform :xcut [picture [_ id x]]
  (assert (contains? picture id) (str ":xcut No block " id))
  (let [block      (picture id)
        new-blocks (->> (xcut-block block x)
                     (map-indexed (fn [i block] [(str id "." i) block])))]
    (-> picture
      (dissoc id)
      (into new-blocks))))

(defmethod transform :ycut [picture [_ id y]]
  (assert (contains? picture id) (str ":ycut No block " id))
  (let [block      (picture id)
        new-blocks (->> (ycut-block block y)
                     (map-indexed (fn [i block] [(str id "." i) block])))]
    (-> picture
      (dissoc id)
      (into new-blocks))))

(defn same-rect? [[l1 b1 r1 t1] [l2 b2 r2 t2]]
  (and
    (= (- r1 l1) (- r2 l2))
    (= (- t1 b1) (- t2 b2))))

(defmethod transform :swap [picture [_ id1 id2]]
  (let [block1 (picture id1)
        rect1  (:rect block1)
        block2 (picture id2)
        rect2  (:rect block2)]
    (assert (contains? picture id1) (str ":swap No block 1 " id1))
    (assert (contains? picture id2) (str ":swap No block 2 " id2))
    (assert (same-rect? rect1 rect2) (str "Blocks should be the same rect, got " rect1 " and " rect2))
    (-> picture
      (update id1 assoc :rect rect2)
      (update id2 assoc :rect rect1))))

(defn merge-rects [rect1 rect2]
  (let [[l1 b1 r1 t1] rect1
        [l2 b2 r2 t2] rect2]
    (cond
      ; rect1
      ; rect2
      (and (= b1 t2) (= l1 l2) (= r1 r2))
      [l1 b2 r1 t1]
      ; rect2
      ; rect1
      (and (= b2 t1) (= l1 l2) (= r1 r2))
      [l1 b1 r1 t2]
      ; rect1 rect2
      (and (= r1 l2) (= b1 b2) (= t1 t2))
      [l1 b1 r2 t1]
      ; rect2 rect1
      (and (= r2 l1) (= b1 b2) (= t1 t2))
      [l2 b1 r1 t1])))

(defn list-simple-blocks [block]
  (if (instance? SimpleBlock block)
    [block]
    (:children block)))

(defn next-id [picture]
  (->
    (reduce-kv
      (fn [acc k _]
        (let [n (parse-long
                  (if-some [i (str/index-of k ".")]
                    (subs k 0 i) 
                    k))]
          (max acc n))) 0 picture)
    inc
    str))

(defmethod transform :merge [picture [_ id1 id2]]
  (let [block1 (picture id1)
        rect1  (:rect block1)
        block2 (picture id2)
        rect2  (:rect block2)
        rect'  (or (merge-rects rect1 rect2)
                 (throw (ex-info (str "Can’t merge " id1 " (" rect1 ") and " id2 " (" rect2 ")") {:rect1 rect1 :rect2 rect2})))
        merged (if (and
                     (instance? SimpleBlock block1)
                     (instance? SimpleBlock block2)
                     (= (:color block1) (:color block2)))
                 (SimpleBlock. rect' (:color block1))
                 (ComplexBlock. rect'
                   (into
                     (list-simple-blocks block1)
                     (list-simple-blocks block2))))]
    (-> picture
      (dissoc id1 id2)
      (assoc (next-id picture) merged))))
