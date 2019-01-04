(ns cljtracer.core
  (:require [cljtracer.camera :as camera]
            [cljtracer.hitable :as hitable]
            [cljtracer.material :as material]
            [cljtracer.ray :as ray]
            [cljtracer.util :as util]
            [cljtracer.vec3 :as vec3])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.tools.cli :as cli]))

(defn color
  [ray world depth]
  (let [hit-record (hitable/hit? world ray 0.001 1000000)]
    (if hit-record
      (if (< depth 50)
        (let [m (:mat hit-record)
              s (material/scatter? m ray hit-record)]
          (if s
            (let [p (color (:scattered s) world (+ depth 1))]
              (vec3/mul (:attenuation s) p))
            vec3/zero))
        vec3/zero)
      (let [direction (vec3/normalize (ray/direction ray))
            t (* (+ (vec3/y direction) 1.0) 0.5)
            white (vec3/vec3 1.0 1.0 1.0)
            blue (vec3/vec3 0.5 0.7 1.0)]
        (vec3/add (vec3/mul-scalar white (- 1.0 t)) (vec3/mul-scalar blue t))))))

(defn random-scene
  []
  (let [static-spheres (list (hitable/->Sphere
                               (vec3/vec3 0.0 -1000.0 0.0)
                               1000.0
                               (material/->Lambertian (vec3/vec3 0.5 0.5 0.5)))
                             (hitable/->Sphere
                               (vec3/vec3 0.0 1.0 0.0)
                               1.0
                               (material/->Dielectric 1.5))
                             (hitable/->Sphere
                               (vec3/vec3 -4.0 1.0 0.0)
                               1.0
                               (material/->Lambertian (vec3/vec3 0.4 0.2 0.1)))
                             (hitable/->Sphere
                               (vec3/vec3 4.0 1.0 0.0)
                               1.0
                               (material/->Metal (vec3/vec3 0.7 0.6 0.5) 0.0)))
        indexes (map #(vector (- (quot % 23) 11) (- (rem % 23) 11)) (range 0 (* 23 23)))
        generate-sphere (fn [[a b]]
                          (let [choose-nat (rand)
                                center (vec3/vec3 (+ a (* 0.9 (rand))) 0.2 (+ b (* 0.9 (rand))))
                                offset (vec3/sub center (vec3/vec3 4.0 0.2 0))
                                offset-len (vec3/length offset)]
                            (when (> offset-len 0.9)
                              (cond
                                (< choose-nat 0.8) (let [r #(* (rand) (rand))]
                                                     (hitable/->Sphere
                                                       center
                                                       0.2
                                                       (material/->Lambertian (vec3/vec3 (r) (r) (r)))))
                                (< choose-nat 0.95) (let [r #(* 0.5 (+ 1 (rand)))]
                                                      (hitable/->Sphere
                                                        center
                                                        0.2
                                                        (material/->Metal (vec3/vec3 (r) (r) (r)) (* 0.5 (rand)))))
                                :else (hitable/->Sphere
                                        center
                                        0.2
                                        (material/->Dielectric 1.5))))))
        random-spheres (filter (comp not nil?) (map generate-sphere indexes))]
    (hitable/->HitableSeq (concat static-spheres random-spheres))))

(defn sample
  [scene camera row column n-rows n-columns]
  (let [u (/ (float (+ column (rand))) n-columns)
        v (/ (float (+ row (rand))) n-rows)
        r (camera/ray camera u v)]
    (color r scene 0)))

(def cli-options
  [["-w" "--width WIDTH" "Image width"
    :default 200
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--height HEIGHT" "Image height"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-s" "--sample SAMPLE" "Samples per pixel"
    :default 16
    :parse-fn #(Integer/parseInt %)]
   ["-n" "--name NAME" "Name of output image"
    :default "out.ppm"]])

(defn -main
  [& args]
  (let [options (:options (cli/parse-opts args cli-options))
        width (:width options)
        height (:height options)
        n-samples (:sample options)
        look-from (vec3/vec3 13.0 2.0 3.0)
        look-at vec3/zero
        vup vec3/vup
        dist-to-focus 10.0
        aperture 0.1
        camera (camera/camera look-from look-at vup 20.0 (/ (float width) height) aperture dist-to-focus)
        index->position #(vector (- height (quot % width) 1) (rem % width))
        positions (map index->position (range 0 (* width height)))
        scene (random-scene)
        generate-pixel (fn [pos]
                         (let [row (first pos)
                               column (second pos)
                               sample #(sample scene camera row column height width)
                               samples (take n-samples (repeatedly sample))
                               raw-p (vec3/div-scalar (apply vec3/add samples) n-samples)
                               p (map (comp int (partial * 255)) (map math/sqrt raw-p))]
                           (vec3/vec3->pixel p)))
        pixels (map generate-pixel positions)]
    (util/save-ppm (:name options) width height pixels)))
