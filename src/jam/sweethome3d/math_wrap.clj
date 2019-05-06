(ns jam.sweethome3d.math-wrap
  (:require [jam.sweethome3d.math :as math]))

;; Wrapper namespace math functions.
;; Functions here:
;; - converts coordinates from sweethome3d format to Java format
;; - call the wrapped function
;; - convert the result back to sweethome3d format
;;
;; Sweethome3d math differences:
;; - y axis is flipped (uses screen coordinates)
;; - angles are measured in clockwise direction
;; - the angle 0 is offset by - PI/2

(defn- flip-y
  "Flips the y axis to convert to/from sh3d/java format."
  [{:keys [y] :as point}]
  ;; flip the y axis
  (assoc point :y (* -1 y)))

(defn- flip-wall-points
  "Flips the y axis of a walls start and end points."
  [{:keys [start end] :as wall}]
  (assoc  wall
          :start (flip-y start)
          :end (flip-y end)))

(defn points-angle
  "Returns the angle in radians between p1 and p2."
  [p1 p2]
  (math/points-angle (flip-y p1) (flip-y p2)))

(defn move-point
  [{:keys [x y] :as point} angle magnitude]
  (-> point
      flip-y
      (math/move-point angle magnitude)
      flip-y))

(defn dimension-points
  "Calculate the end points of a dimension line between two walls."
  [wall1 wall2 {:keys [align] :as opts}]
  (let [f       flip-wall-points
        [p1 p2 extension] (math/dimension-points (f wall1) (f wall2) opts)]
    [(flip-y p1) (flip-y p2) extension]))
