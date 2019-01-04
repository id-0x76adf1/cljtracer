(ns cljtracer.vec3
  (:require [clojure.math.numeric-tower :as math]))

(defn vec3 [e0 e1 e2] (vector e0 e1 e2))

(defn x [v] (first v))
(defn y [v] (second v))
(defn z [v] (nth v 2))

(defn r [v] (first v))
(defn g [v] (second v))
(defn b [v] (nth v 2))

(defn add [& args] (apply map + args))
(defn sub [& args] (apply map - args))
(defn mul [v1 v2] (map * v1 v2))
(defn div
  [v1 v2]
  (let [v1 (map float v1)]
    (map / v1 v2)))

(defn mul-scalar
  [v s]
  (map (partial * s) v))

(defn div-scalar
  [v s]
  (map #(/ % (float s)) v))

(defn length
  [v]
  (math/sqrt (reduce + (map #(* % %) v))))

(defn squared-length
  [v]
  (reduce + (map #(* % %) v)))

(defn normalize
  [v]
  (div-scalar v (length v)))

(defn dot
  [v1 v2]
  (reduce + (mul v1 v2)))

(defn cross
  [v1 v2]
  (let [new-x (- (* (y v1) (z v2)) (* (z v1) (y v2)))
        new-y (- (- (* (x v1) (z v2)) (* (z v1) (x v2))))
        new-z (- (* (x v1) (y v2)) (* (y v1) (x v2)))]
    (vec3 new-x new-y new-z)))

(defn vec3->pixel
  [v]
  {:r (r v), :g (g v), :b (b v)})

(defn reflect
  [v n]
  (sub v (mul-scalar n (* 2 (dot v n)))))

(defn refract
  [v n ni-over-nt]
  (let [uv (normalize v)
        dt (dot uv n)
        discriminant (- 1.0 (* ni-over-nt ni-over-nt (- 1 (* dt dt))))]
    (when (> discriminant 0)
      (sub (mul-scalar (sub uv (mul-scalar n dt)) ni-over-nt)
           (mul-scalar n (math/sqrt discriminant))))))

(def zero (vec3 0.0 0.0 0.0))
(def vup (vec3 0.0 1.0 0.0))
