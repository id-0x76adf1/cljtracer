(ns cljtracer.ray
  (:require [cljtracer.vec3 :as vec3]))

(defrecord Ray [origin direction])

(defn origin [ray] (:origin ray))
(defn direction [ray] (:direction ray))

(defn point-at-parameter
  [ray t]
  (vec3/add (origin ray) (vec3/mul-scalar (direction ray) t)))
