(ns jam.sweethome3d.math
  (:require [clojure.test :refer [deftest are]]))

;; Pure math functions that use coordinates of Java math.

(defn slope
  "Returns the slope of a line or nil if the slope is infinite."
  [{:keys [start end] :as line}]
  (let [dx (- (:x end) (:x start))
        dy (- (:y end) (:y start))]
    (if-not (zero? dx)
      (/ dy dx))))

(defn y-intercept
  "Returns the y intercept a line or nil of there is none."
  [{:keys [start end] :as line}]
  (if-let [m (slope line)]
    (- (:y start) (* m (:x start)))))

(defn subtract-point
  "Subtracts points."
  [& points]
  (apply merge-with (fn [v1 v2]
                      (if (number? v1)
                        (- v1 v2)
                        v2))
         points))

(defn points-angle
  "Returns the angle in radians between p1 and p2."
  [p1 p2]
  (let [p (subtract-point p2 p1)]
    ;; Note: atan2 takes y as first arg
    (Math/atan2 (:y p) (:x p))))

(deftest points-angle-test
  (are [expected p1 p2] (= expected (points-angle p1 p2))
    (* Math/PI 3/4)  {:x 1 :y 0} {:x 0 :y 1}
    (* Math/PI -1/4) {:x 0 :y 1} {:x 1 :y 0}
    Math/PI          {:x 1 :y 0} {:x -1 :y 0}
    0.0              {:x 1 :y 0} {:x 2 :y 0}
    Math/PI          {:x 1 :y 1} {:x -1 :y 1}
    ))

(defn- round-off
  [f]
  (-> f (* 100) Math/round (/ 100) float))

(defn move-point
  [{:keys [x y] :as point} angle magnitude]
  (let [x1 (Math/cos angle)
        y1 (Math/sin angle)]
    (assoc point
           :x (round-off (+ x (* x1 magnitude)))
           :y (round-off (+ y (* y1 magnitude))))))

(deftest move-point-test
  (are [x1 y1 x y angle] (= {:x (float x1) :y (float y1)} (move-point {:x x :y y} angle 1))
    1 0,  0 0, 0
    0 1, 0 0, (* Math/PI 1/2)
    -1 0, 0 0, Math/PI
    0 -1, 0 0, (* Math/PI 3/2)
    ))

(defn dimension-points
  "Calculate the end points of a dimension line between two walls."
  [wall1 wall2 {:keys [align offset from] :as opts}]
  (let [m (slope wall1)
        _ (assert (= m (slope wall2)) "lines are not parallel")
        _ (assert (align #{:inside :center :outside}) "invalid align value")

        ;; y intercepts
        c1 (y-intercept wall1)
        c2 (y-intercept wall2)

        from (or from :start)
        to   (if (= from :start)
               :end
               :start)
        x1   (-> wall1 from :x)
        x2   (-> wall2 from :x)
        ;; distance between lines
        d    (Math/abs (if (nil? c1)
                         (- x2 x1)
                         (/ (- c1 c2)
                            (Math/sqrt (+ 1 (* m m))))))

        angle (points-angle (wall1 from) (wall1 to))

        ;; amount to offset points by (excluding extension lines)
        offset (or offset
                   (case align
                     :inside  30
                     :center  0
                     :outside 0))

        PI          Math/PI
        s1          (from wall1)
        s2          (from wall2)
        angle-s1-s2 (points-angle s1 s2)
        tangent     (let [t (+ angle (/ PI 2))]
                      (if (< (Math/abs (- t angle-s1-s2)) (/ PI 2))
                        t
                        (- t PI)))
        
        ;; adjust first point for offset
        p1 (move-point s1 angle offset)
        tf (case align                  ; get thickness factor
             :inside  1/2
             :center  0
             :outside -1/2)
        ;; adjust first point for wall thickness
        p1 (move-point p1 tangent (* tf (:thickness wall1)))
        ;; find distance to move along tangent to second point
        d  (- d
              (* (:thickness wall1) tf)
              (* (:thickness wall2) tf))
        p2 (move-point p1 tangent d)

        ;; size of extension lines
        ext-angle (- angle (* Math/PI 1/2))
        ext-l (case align
                :inside  0
                :center  30
                :outside 60)
        extension (if (= ext-angle tangent)
                    ext-l
                    (* -1 ext-l))]
   [p1 p2 extension]))

(deftest dimension-points-test
  (are [expected w1 w2 align] (= expected (dimension-points w1 w2 {:align align}))
  
    ;; horizontal lines; centered
    [{:x 0. :y 0.} {:x 0. :y 300.} -30]
    {:start {:x 0 :y 0} :end {:x 8000 :y 0} :thickness 20}
    {:start {:x 0 :y 300} :end {:x 8000 :y 300} :thickness 20}
    :center

    ;; vertical lines; centered
    [{:x 0. :y 0.} {:x 300. :y 0.} 30]
    {:start {:x 0 :y 0} :end {:x 0 :y 8000} :thickness 20}
    {:start {:x 300 :y 0} :end {:x 300 :y 8000} :thickness 20}
    :center

    ;; horizontal lines; inside
    [{:x 30. :y 10.} {:x 30. :y 293.} 0]
    {:start {:x 0 :y 0} :end {:x 8000 :y 0} :thickness 20}
    {:start {:x 0 :y 300} :end {:x 8000 :y 300} :thickness 14}
    :inside

    ;; horizontal lines; outside
    [{:x 0. :y -10.} {:x 0. :y 307.} -60]
    {:start {:x 0 :y 0} :end {:x 8000 :y 0} :thickness 20}
    {:start {:x 0 :y 300} :end {:x 8000 :y 300} :thickness 14}
    :outside

    ;; vertical lines; inside
    [{:x 10. :y 30.} {:x 293. :y 30.} 0]
    {:start {:x 0 :y 0} :end {:x 0 :y 8000} :thickness 20}
    {:start {:x 300 :y 0} :end {:x 300 :y 8000} :thickness 14}
    :inside

    ;; vertical lines; outside
    [{:x -10. :y 0.} {:x 307. :y 0.} 60]
    {:start {:x 0 :y 0} :end {:x 0 :y 8000} :thickness 20}
    {:start {:x 300 :y 0} :end {:x 300 :y 8000} :thickness 14}
    :outside

    ))
