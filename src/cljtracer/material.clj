(ns cljtracer.material
  (:require [cljtracer.ray :as ray]
            [cljtracer.util :as util]
            [cljtracer.vec3 :as vec3]
            [clojure.math.numeric-tower :as math]))

(defprotocol Material
  (scatter? [this ray-in hit-record]))

(defrecord Lambertian [albedo]
  Material
  (scatter? [this ray-in hit-record]
    (let [hit-point (:p hit-record)
          normal (:normal hit-record)
          random-point (util/random-in-unit-sphere)
          target (vec3/add hit-point normal random-point)
          scattered (ray/->Ray hit-point (vec3/sub target hit-point))]
      {:scattered scattered, :attenuation albedo})))

(defrecord Metal [albedo fuzz]
  Material
  (scatter? [this ray-in hit-record]
    (let [f (if (< fuzz 1) fuzz 1)
          hit-point (:p hit-record)
          normal (:normal hit-record)
          reflected (vec3/reflect (vec3/normalize (:direction ray-in)) normal)
          random-point (util/random-in-unit-sphere)
          direction (vec3/add reflected (vec3/mul-scalar random-point f))
          scattered (ray/->Ray hit-point direction)]
      (when (> (vec3/dot direction normal) 0)
        {:scattered scattered, :attenuation albedo}))))

(defrecord Dielectric [ref-idx]
  Material
  (scatter? [this ray-in hit-record]
    (let [hit-point (:p hit-record)
          in-direction (:direction ray-in)
          in-direction-len (vec3/length in-direction)
          normal (:normal hit-record)
          dot (vec3/dot in-direction normal)
          acute? (> dot 0)
          outward-normal (if acute? (vec3/mul-scalar normal -1) normal)
          ni-over-nt (if acute? ref-idx (/ 1.0 ref-idx))
          cosine (if acute?
                   (let [c (/ dot in-direction-len)]
                     (math/sqrt (- 1.0 (* ref-idx ref-idx (- 1.0 (* c c))))))
                   (/ (- dot) in-direction-len))
          refracted (vec3/refract in-direction outward-normal ni-over-nt)
          reflected (vec3/reflect in-direction normal)
          reflect-prob (if refracted (util/schlick cosine ref-idx) 1.0)
          one (vec3/vec3 1.0 1.0 1.0)]
      (if (< (rand) reflect-prob)
        {:scattered (ray/->Ray hit-point reflected), :attenuation one}
        {:scattered (ray/->Ray hit-point refracted), :attenuation one}))))
