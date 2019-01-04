(ns cljtracer.util
  (:require [cljtracer.vec3 :as vec3])
  (:require [clojure.math.numeric-tower :as math]))

(defn- random-in-unit-cube
  []
  (let [p (vec3/vec3 (rand) (rand) (rand))]
    (vec3/sub (vec3/mul-scalar p 2.0) (vec3/vec3 1 1 1))))

(defn- random-in-unit-square
  []
  (let [p (vec3/vec3 (rand) (rand) 0.0)]
    (vec3/sub (vec3/mul-scalar p 2.0) (vec3/vec3 1 1 0))))

(defn- random-in-unit
  [gen]
  (let [ps (repeatedly gen)
        in-unit? #(when (< (vec3/squared-length %) 1.0) %)]
    (some in-unit? ps)))

(defn random-in-unit-sphere
  []
  (random-in-unit random-in-unit-cube))

(defn random-in-unit-disk
  []
  (random-in-unit random-in-unit-square))

(defn save-ppm
  [name n-columns n-rows pixels]
  (let [rows (partition n-columns pixels)
        format-pixel (fn [pixel]
                       (let [r (:r pixel)
                             g (:g pixel)
                             b (:b pixel)]
                         (format "%d %d %d" r g b)))
        format-row #(apply str (interpose " " (map format-pixel %)))
        pixels (apply str (interpose "\n" (map format-row rows)))]
    (spit name (format "P3\n%d %d\n255\n%s\n" n-columns n-rows pixels))))

(defn schlick
  [cosine ref-idx]
  (let [r0 (math/expt (/ (- 1.0 ref-idx) (+ 1.0 ref-idx)) 2)]
    (+ r0 (* (- 1.0 r0) (math/expt (- 1.0 cosine) 5)))))
