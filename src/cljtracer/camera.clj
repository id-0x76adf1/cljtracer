(ns cljtracer.camera
  (:require [cljtracer.ray :as ray]
            [cljtracer.vec3 :as vec3]
            [cljtracer.util :as util]))

(defn camera
  [look-from look-at vup vfov aspect aperture focus-dist]
  (let [lens-radius (/ aperture 2.0)
        theta (/ (* vfov Math/PI) 180.0)
        half-height (Math/tan (/ theta 2))
        half-width (* half-height aspect)
        origin look-from
        w (vec3/normalize (vec3/sub look-from look-at))
        u (vec3/normalize (vec3/cross vup w))
        v (vec3/cross w u)
        lower-left-corner (vec3/sub origin
                                    (vec3/mul-scalar u (* half-width focus-dist))
                                    (vec3/mul-scalar v (* half-height focus-dist))
                                    (vec3/mul-scalar w focus-dist))
        horizontal (vec3/mul-scalar u (* 2.0 half-width focus-dist))
        vertical (vec3/mul-scalar v (* 2.0 half-height focus-dist))]
    {:origin origin
     :lower-left-corner lower-left-corner
     :horizontal horizontal
     :vertical vertical
     :u u :v v :w w
     :lens-radius lens-radius}))

(defn ray
  [c s t]
  (let [lens-radius (:lens-radius c)
        rd (vec3/mul-scalar (util/random-in-unit-disk) lens-radius)
        offset (vec3/add (vec3/mul-scalar (:u c) (vec3/x rd))
                         (vec3/mul-scalar (:v c) (vec3/y rd)))
        origin (vec3/add (:origin c) offset)
        lower-left-corner (:lower-left-corner c)
        u-d (vec3/mul-scalar (:horizontal c) s)
        v-d (vec3/mul-scalar (:vertical c) t)
        direction (vec3/add lower-left-corner u-d v-d (vec3/mul-scalar origin -1))]
    (ray/->Ray origin direction)))
