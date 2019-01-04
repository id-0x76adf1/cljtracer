(ns cljtracer.hitable
  (:require [clojure.math.numeric-tower :as math]
            [cljtracer.ray :as ray]
            [cljtracer.vec3 :as vec3]))

(defprotocol Hitable
  (hit? [this ray t-min t-max]))

(defrecord HitRecord [t p normal mat])

(defrecord Sphere [center radius mat]
  Hitable
  (hit? [this ray t-min t-max]
    (let [oc (vec3/sub (ray/origin ray) center)
          d (ray/direction ray)
          a (vec3/dot d d)
          b (vec3/dot oc d)
          c (- (vec3/dot oc oc) (* radius radius))
          discriminant (- (* b b) (* a c))]
      (when (> discriminant 0)
        (let [s (math/sqrt discriminant)
              ts [(/ (- (- b) s) a)
                  (/ (+ (- b) s) a)]
              hit-record? (fn [t]
                             (when (and (< t t-max) (> t t-min))
                               (let [p (ray/point-at-parameter ray t)
                                     normal (vec3/div-scalar (vec3/sub p center) radius)]
                                 (->HitRecord t p normal mat))))]
          (some hit-record? ts))))))

(defrecord HitableSeq [s]
  Hitable
  (hit? [this ray t-min t-max]
    (loop [closest-so-far t-max
           closest nil
           s s]
      (if (empty? s)
        closest
        (let [r (hit? (first s) ray t-min closest-so-far)]
          (recur
            (if r (:t r) closest-so-far)
            (if r r closest)
            (rest s)))))))
