;; Developers Guid: http://www.sweethome3d.com/pluginDeveloperGuide.jsp

(ns jam.sweethome3d
  (:require [clojure.walk :refer [postwalk]]
            [clojure.core.incubator :refer [dissoc-in]]
            [clojure.test :refer [deftest is are]]
            [jam.sweethome3d.math-wrap :as math])
  (:import [com.eteks.sweethome3d.model Level Wall Room DimensionLine HomeTexture CatalogTexture
            HomePieceOfFurniture]
           jam.sweethome3d.NReplPlugin
           java.util.concurrent.Callable
           com.eteks.sweethome3d.viewcontroller.ThreadedTaskController
           com.eteks.sweethome3d.viewcontroller.ThreadedTaskController$ExceptionHandler
           com.eteks.sweethome3d.swing.SwingViewFactory
           com.eteks.sweethome3d.io.HomeXMLHandler
           ))

;; Notes:
;; - units are in cm
;; - values are floats

(defn connected?
  "Returns true if connected to sweethome3d app."
  []
  (-> NReplPlugin/plugin nil? not))

(defn init!
  [state]
  (let [plugin NReplPlugin/plugin #_ (NReplPlugin/plugin)
        homeController (.getHomeController plugin)]
    (reset! state
            {:plugin plugin
             :home (.getHome plugin)
             :homeController homeController
             :planController (.getPlanController homeController)
             :homeController3D (.getHomeController3D homeController)  ;; display levels, set cameras
             :userPreferences (.getUserPreferences plugin)
             :walls [] ; map of walls created
             })))

(def state
  (let [a (atom {:walls []})]
    (if (connected?)
      (init! a))
    a))

(defn degrees-to-radians
  [angle]
  (-> angle (/ 180) (* Math/PI)))

(defn radians-to-degrees
  [angle]
  (-> angle (* 180) (/ Math/PI)))

(defn set-compass
  [angle]
  (-> @state :home .getCompass (.setNorthDirection angle)))

(defn get-compass
  []
  (-> @state :home .getCompass .getNorthDirection))


;; 3D View Control
;; (com.eteks.sweethome3d.viewcontroller.Home3DAttributesController.)
;; plugin getHomeController getHomeController3D
(defn set-walls-alpha
  [alpha]
  (-> @state :home .getEnvironment (.setWallsAlpha alpha)))
#_ (set-walls-alpha 0.35)
#_ (set-walls-alpha 1.0)
#_ (invoke-ui #(set-walls-alpha 0.0))
#_ (-> @state :home .setSelectedLevel(level))

(defn set-level
  [name]
  (let [home (@state :home)
        level (-> @state :levels (get name))]
    (.setSelectedLevel home level)))

;; TexturesCatalog - has list of TexturesCatagory
;; TexturesCatagory - has list of CatalogTexture
(defn find-texture
  [category-name id & opts]
  (let [{:keys [angle] :or {angle 0}} (apply hash-map opts)

        catalog  (-> @state :userPreferences .getTexturesCatalog)
        category (->> catalog .getCategories (filter #(= category-name (.getName %))) first)
        texture  (->> category .getTextures (filter #(= id (.getId %))) first)
        angle    (-> angle degrees-to-radians float)
        ]
    (HomeTexture. texture angle)))

;; Categories are:
"Fabric" "Floor" "Miscellaneous" "Roof" "Rug" "Sky" "Wall" "Wallpaper" "Wood"

;; ROOM
(defn float-array2
  "Convert a 2 dimensional array of clojure numbers java floats."
  [points]
  (->> points
       (postwalk #(if (number? %)
                    (float %)
                    %))
       (map float-array)
       into-array))

(defn private-method
  [obj method-name & args]
  (let [m (first (filter (fn [x] (.. x getName (equals method-name)))
                         (.. obj getClass getDeclaredMethods)))]
    (. m (setAccessible true))
    (. m (invoke obj (into-array Object args)))))

(defn private-field [obj fn-name-string]
  (let [m (.. obj getClass (getDeclaredField fn-name-string))]
    (. m (setAccessible true))
    (. m (get obj))))

;; Note: Need to call private object and method to auto create room around a point.
(defn add-point
  "Add points together."
  [& points]
  (apply merge-with + points))


;; select keys :x :y
;; if v is keyword
;;   find wall (or throw exception)
(defn get-wall
  ([k] (get-wall k true))
  ([k assert_]
   (or (-> @state :walls-idx k)
       (if assert_
         (throw (Exception. (str "Can't find wall " k)))))))
#_ (get-wall :house-S-0)

(defn wall-to-clj
  "Converts a wall to a clojure map based data structure."
  [wall]
  {:start {:x (.getXStart wall) :y (.getYStart wall)}
   :end   {:x (.getXEnd wall)   :y (.getYEnd wall)}
   :thickness (.getThickness wall)
   })

(defn coerce-wall
  "Coerces value to a wall if a matching wall exists."
  [v]
  (if-let [wall (and (keyword? v) (get-wall v))]
    (-> wall :sh wall-to-clj)
    (if (instance? Wall v)
      (wall-to-clj v)
      v)))
#_ (coerce-wall :foo)
#_ (coerce-wall :house-E-0)

(defn midval
  [x1 x2]
  (/ (+ x1 x2) 2))
#_ (midval 1 3)

(defn midpoint
  "Returns the midpoint of p1 p2 or wall."
  ([p1 p2]
   {:x (midval (:x p1) (:x p2))
    :y (midval (:y p1) (:y p2))})
  ([wall]
   (let [wall (cond
                (keyword? wall)       (-> wall get-wall :sh wall-to-clj)
                (instance? Wall wall) (-> wall wall-to-clj)
                :default              wall)]
     (midpoint (wall :start) (wall :end)))))
#_ (midpoint {:x 0 :y 0} {:x 5 :y 5})
#_ (midpoint (get-wall :house-N-0))
#_ (midpoint :house-N-0)
#_ (instance? Wall (-> :house-N-0 get-wall))
#_ (-> :house-N-0 get-wall wall-to-clj)


(defn direction-to-offset
  "Use the compass setting to convert a direction to an x y offset."
  ([direction magnitude]
   (direction-to-offset (get-compass) direction magnitude))
  ([north direction magnitude]
   (let [target-angle (case direction
                        :N 0
                        :E (/ Math/PI 2)
                        :S Math/PI
                        :W (* Math/PI 3/2))

         ;; calculate angle from compass and direction
         angle (+ north target-angle)

         ;; adjust angle to match java Math
         ;; rotation is opposite and starts at a 90 degree offset
         angle (-> angle (* -1) (+ (/ Math/PI 2)))

         ;; adjust magnitude, flip y axis
         x (-> angle Math/cos Math/round (* magnitude))
         y (-> angle Math/sin Math/round (* -1 magnitude))]
     {:x x :y y})))

(deftest direction-to-offset-test
  (are [expected direction] (= expected (direction-to-offset (-> Math/PI (* 3/2)) direction 1))
    {:x -1, :y  0}  :N 
    {:x  1, :y  0}  :S
    {:x  0, :y -1}  :E 
    {:x  0, :y  1}  :W 
    ))

(defn room-point
  "Returns a point in a room based on wall and direction."
  [wall direction]
  (let [wall   (-> wall get-wall :sh)
        mp     (midpoint wall)
        magnitude (-> wall .getThickness (/ 2) (+ 1)) ; move 1 unit beyond wall into froom
        offset (direction-to-offset direction magnitude)]
    (add-point mp offset)
    ))
#_ (room-point :bedroom-sep-0 :N)

;; Note: the room points do not include the wall widths
(defn create-room-from-points
  [points]
  (let [room (-> points
                 float-array2
                 Room.)]
    (-> @state :home (.addRoom room))
    room))

(defn create-room-around
  [{:keys [around floor]}]
  (let [point            (apply room-point around)
        controller       (-> @state :planController)
        roomDrawingState (private-field controller "roomDrawingState")
        room             (private-method roomDrawingState "createRoomAt" (-> point :x float) (-> point :y float))]
    room))
#_ (create-room :around [:bedroom-sep-0 :N]
             :floor {:texture ["Floor" "eTeksScopia#english-parquet-1"]})

(defn create-room
  [& args]
  (when true
    (let [points? (-> args count odd?)
          {:keys [around
                  floor
                  name] :as opts} (apply hash-map
                                         (if points?
                                           (rest args)
                                           args))
          room (if points?
                 (create-room-from-points (first args))
                 (create-room-around opts))]
      (when name
        (.setName room name))
      (when floor
        (->> floor :texture (apply find-texture) (.setFloorTexture room)))
      room)))
#_ (.getPropertyNames (get-wall :house-S-1))


(defn dimension-line
  "Adds a dimension line between wall1 and wall2."
  [wall1 wall2 {:keys [align] :as opts}]
  (let [[p1 p2 extension] (math/dimension-points
                           (coerce-wall wall1)
                           (coerce-wall wall2) opts)
        line              (DimensionLine. (p1 :x) (p1 :y)
                                          (p2 :x) (p2 :y)
                                          extension)]
    (-> @state :home (.addDimensionLine line))))

;; Inspect selected element
(defn inspect-selected
  "Inspects the selected item."
  []
  (let [items (-> @state :home .getSelectedItems)
        item (first items)]
    (prn
     (cond
       (instance? Room item) (let [t (-> item .getFloorTexture)]
                               {:name (.getName t)
                                :catalogId (.getCatalogId t)})
       :default item))
    item))
#_ (inspect-selected)

(defn invoke-ui
  [f]
  (let [;; Pass in caller's ns for eval to be able to resolve symbols
        ;; when run in gui thread.
        ns_      *ns*
        runnable (reify Runnable
                   (run [this]
                     (prn "invoke-ui, runnable: entering")
                     (binding [*ns* ns_]
                       (f))))
        homeView (-> @state :homeController .getView)
        callable (reify Callable
                   (call [this]
                     (prn "invoke-ui, callable: entering")
                     (.invokeLater homeView runnable)))

        exHandler (reify ThreadedTaskController$ExceptionHandler
                    (handleException [this ex]
                      (.printStackTrace ex)))

        taskController (ThreadedTaskController. callable
                                                "casum repairus"
                                                exHandler
                                                (.getUserPreferences (@state :plugin))
                                                (SwingViewFactory.))]
    (.executeTask taskController homeView)))

(defn clean-home
  "Delete everything in the home."
  []
  (let [home (:home @state)]
    (doseq [wall (.getWalls home)] (.deleteWall home wall))
    (doseq [label (.getLabels home)] (.deleteLabel home label))
    (doseq [dimensionLine (.getDimensionLines home)] (.deleteDimensionLine home dimensionLine))
    (doseq [room (.getRooms home)] (.deleteRoom home room))
    (doseq [level (.getLevels home)] (.deleteLevel home level))
    (doseq [piece (.getFurniture home)] (.deletePieceOfFurniture home piece))
    (doseq [polyline (.getPolylines home)] (.deletePolyline home polyline))))
#_ (clean-home)

(defn to-float
  [val]
  (if (number? val)
    (float val)
    val))
#_ (to-float 1)

(defn vals-to-float
  [coll]
  (->> coll
       (map (fn [[k v]]
              [k (to-float v)]))
       (into {})))
#_ (vals-to-float {:x 1 :y 2})


(defn pairs
  "Converts coll to a sequence of pairs of items."
  [coll]
  (let [f (fn [val item]
            (let [val (if (contains? val :last)
                        (update-in val [:coll] concat [[(:last val) item]])
                        )]
              (assoc val :last item)))
        result (reduce f {:coll []} coll)]
    (:coll result)))

(defn zip [& colls]
  (partition (count colls) (apply interleave colls)))


;; notes:
;; - by default, adding a level sets its elevation at the highest point above the previous level
;; - there is a default level
;; - the floor thickness is part of the level
;; - walls start on top of the floor thickness
;; - height is empty volume above floor
;; - total level height is floor thickness + height
;; - can set selected level from plan controller setSelectedLevel
;; - need to add first level via Home.addLevel before adding walls
;;   adding objects before creating a level will result in creating a
;;   default level that needs to be configured after the fact
(defn add-level
  "Adds a level to the home."
  [name props]
  (let [{:keys [elevation thickness height]} (merge (-> @state :defaults :level) props)
        level (Level. name (float elevation) (float thickness) (float height))]
    (swap! state assoc-in [:levels name] level)
    (-> @state :home (.addLevel level))))


(def ^:dynamic -eval-key)
(defn dim
  [wall]
  (or (-> wall :start -eval-key) ; extract from wall
      (-> wall -eval-key)))      ; extract from point
#_ (binding [-eval-key :x]
     (let [wall (-> :house-S-0 get-wall wall-to-clj)]
       (eval `(dim ~wall))))

(defn eval-form
  "Resolves wall references and evals form."
  [form k]
  (binding [-eval-key k]
    (let [result (eval (postwalk coerce-wall form))
          ;; auto get active dimension from final form result
          result (cond
                   (map? result) (dim result)
                   :default result)]
      result)))
#_ (eval-form '(midpoint :house-N-0) :y)
#_ (eval-form '(dim :house-S-0) :x)
#_ (eval-form '(identity (dim :house-S-0)) :x)
#_ (eval-form '(/ (dim :house-S-0) 2) :x)
#_ (eval-form '(identity :house-S-0) :x)
#_ (eval-form '(identity :house-S-0) :y)

;; -? can a cons be evaled?
#_ (let [i 42] (type `'(~i))) ; Cons
#_ (let [i 42] (type '(~i))) ; PersistentList
#_ (let [i 42] (eval `(identity ~i))) ; 42
#_ (let [i 42] (eval `(identity ~i)))

#_ (let [i 42] (type `(identity ~i)))
#_ (let [i 42] (instance? clojure.lang.Cons `(identity ~i)))
#_ (let [i 42] (instance? Cons `(identity ~i)))


(defn resolve-walls
  [point last-point]
  (let [f (fn [m k v]
            ;; (prn "resolv-walls: m, k, v" m k v)
            (let [v (cond
                      (keyword? v)                    (-> v coerce-wall :start k)
                      (list? v)                       (eval-form v k)
                      (instance? clojure.lang.Cons v) (eval-form v k)
                      (fn? v)                         (v (k last-point))
                      (map? v)                        (k v)
                      :default                        v)]
              (assoc m k v)))
        m (select-keys point [:x :y :height]) ; list of keys to check
        m (reduce-kv f {} m)]
    (merge point m)))
#_ (resolve-walls {:x :house-W-0})
#_ (resolve-walls {:x '(identity :house-W-0)})

(defn direction-to-offset2
  "Use the compass setting to convert a direction to an x y offset."
  ([direction magnitude]
   (direction-to-offset (get-compass) direction magnitude))
  ([north direction magnitude]
   (let [target-angle (case direction
                        :N 0
                        :E (/ Math/PI 2)
                        :S Math/PI
                        :W (* Math/PI 3/2))

         ;; calculate angle from compass and direction
         angle (+ north target-angle)

         ;; adjust angle to match java Math
         ;; rotation is opposite and starts at a 90 degree offset
         angle (-> angle (* -1) (+ (/ Math/PI 2)))

         ;; adjust magnitude, flip y axis
         x (-> angle Math/cos Math/round (* magnitude))
         y (-> angle Math/sin Math/round (* -1 magnitude))]
     {:x x :y y})))

#_ (defn align-wall
  [{:keys [x1 y1 thickness align] :as p1} {:keys [x y] :as p2}]
  (if (or (nil? align) (= align :center))
    [p1 p2]
    (let [p1     (flip-y p1)
          p2     (flip-y p2)
          angle  (points-angle p1 p2)
          angle  (+ angle (case align
                            :left  (/ Math/PI 2)
                            :right (/ Math/PI -2)
                            :else  (assert false)))

          ;; calculate and apply offset
          offset {:x (-> angle Math/cos (* thickness 1/2))
                  :y (-> angle Math/sin (* thickness 1/2))}

          p1 (-> p1 (add-point offset) flip-y)
          p2 (-> p2 (add-point offset) flip-y)]
      [p1 p2])))

#_ (deftest align-wall-test
  (are [expected p1 p2] (= expected (align-wall p1 p2))
     [{:x 100 :y 100} {:x 200 :y 100}] {:x 100 :y 100 } {:x 200 :y 100}
     ;; [{:x 100.0 :y 90.0 :align :left :thickness 10} {:x 200.0 :y 90.0}] {:x 100 :y 100 :align :left :thickness 10} {:x 200 :y 100}
    ;; [{:x 100.0 :y 110.0 :align :right :thickness 10} {:x 200.0 :y 110.0}] {:x 100 :y 100 :align :right :thickness 10} {:x 200 :y 100}
    ;; [{:x 110.0 :y 100.0 :align :left :thickness 10} {:x 110.0 :y 200.0}] {:x 100 :y 100 :align :left :thickness 10} {:x 100 :y 200}
    ))

;; Notes:
;; - Walls are created using height (relative to current level) instead of z.
;; - Wall thickness is applied 1/2 to each side of center.
;; - Walls can have an associated level but don't by default.
;; - height should default to the height of the last one unless specified
(defn add-walls
  "Add a sequence of connected walls."
  ([wall-type points]
   (add-walls wall-type {} points))
  ([wall-type {:keys [closed? start-at] :as properties} points]
   (let [normalize (fn [points a]
                     (let [;; convert to map, extract name if present
                           point (if (odd? (count a))
                                   (assoc (apply hash-map (butlast a))
                                          :name (last a))
                                   (apply hash-map a))
                           ;; merge defaults with supplied values
                           point (merge
                                  (-> @state :defaults :level (select-keys [:height]))   ;; global defaults
                                  (-> @state :defaults :wall wall-type (select-keys [:height])) ;; wall type
                                  (-> points last (select-keys [:x :y :height])) ;; last point
                                  point)

                           ;; resolve references to other walls
                           ;; ? should/can this join?
                           point (resolve-walls point (-> points last))

                           ;; convert all number values to float
                           point (vals-to-float point)]

                       (concat points [point])))
         points (reduce normalize [] points)

         points    (if closed?
                     (concat points [(first points)])
                     points)
         make-wall (fn [[p1 p2]]
                     (let [thickness (or (:thickness p1)
                                         (-> @state :defaults :wall wall-type :thickness))
                           ;;[p1 p2] (align-wall (assoc p1 :thickness thickness) p2)
                           wall    (Wall. (:x p1) (:y p1)
                                          (:x p2) (:y p2)
                                          thickness
                                          (:height p1))]
                       (.setHeightAtEnd wall (:height p2))
                       wall))
         walls     (map make-wall (pairs points))]

     ;; Wrap sh wall and save.
     (doseq [[{:keys [name] :as point} wall] (zip points walls)]
       (let [wall-clj {:sh wall :type wall-type}
             wall-clj (if name
                        (assoc wall-clj :name name)
                        wall-clj)]
         ;; list of all walls
         (swap! state update-in [:walls] conj wall-clj)
         ;; indexed by name
         (when name
           (swap! state assoc-in [:walls-idx name] wall-clj))))

     ;; Join walls.
     (when start-at
       (prn "joining wall" start-at)
       (let [start-wall (-> @state :walls start-at)]
         (if start-wall
           (.setWallAtStart (first walls) start-wall)
           (prn "ERROR: failed to find wall:" start-at)
           )))
     (doseq [[wall1 wall2] (pairs walls)]
       (.setWallAtEnd wall1 wall2)
       (.setWallAtStart wall2 wall1))
     ;; Close the final corner.
     (when closed?
       (.setWallAtStart (first walls) (last walls))
       (.setWallAtEnd (last walls) (first walls)))

     ;; Add the walls to the home.
     (-> @state :planController (.addWalls walls))
     walls)))

(defn delete-wall-clj
  [{:keys [name] :as wall}]
  ;; (prn "delte-wall-clj: deleting wall" wall)
   (when name
     (swap! state dissoc-in [:walls-idx name]))
  (swap! state update-in [:walls] #(filterv (fn [w] (= w wall)) %))
  (-> @state :home (.deleteWall (:sh wall))))

(defn delete-wall
  [kw]
  (-> kw get-wall delete-wall-clj))
#_ (invoke-ui (fn [] (delete-wall :mud)))

(defn delete-walls
  [f]
  (doseq [wall (-> @state :walls)]
    (when (f wall)
      (delete-wall-clj wall))))
#_ (invoke-ui (fn [] (delete-walls #(= :virtual (:type %)))))
#_ (get-wall :mud)
#_ (-> @state :walls)

;; Stair length
(let [total-rise     300
      tread          28
      standard-riser (* 2.54 7)
      steps          (-> (/ total-rise standard-riser)
                         float
                         Math/round)
      rise           (->(/ total-rise steps) float)
      length         (* tread steps)
      angle          (-> (Math/atan (/ rise tread)) (* 180) (/ Math/PI))
      ;; stringer length
      ;; step heights
      ]
  {:total-rise total-rise
   :tread      tread
   :steps      steps
   :rise       rise
   :length     length
   :angle      angle
   })

#_ (inspect-selected)
#_ (-> (inspect-selected) .getCatalog prn)

(defn load-furniture
  "Loads the furniture object with the given category-name and id."
  [category-name id]
  (let [catalog    (-> @state :userPreferences .getFurnitureCatalog)
        categories (.getCategories catalog)
        category   (->> categories (filter #(= category-name (.getName %))) first)
        furniture  (->> category .getFurniture (filter #(= id (.getId %))) first)]
    furniture))
;; (doseq [category categories] (prn (.getName category)))

;; Categories
"Bathroom"
"Bedroom"
"Doors and windows"
"Exterior"
"Kitchen"
"Lights"
"Living room"
"Lumber"
"Miscellaneous"
"Office"
"Roof"
"Staircases"
"Vehicles"

(defn center
  "Returns the center of two points"
  [p1 p2]
  (let [avg (fn [a b] (/ (+ a b) 2))
        x   (avg (:x p1) (:x p2))
        y   (avg (:y p1) (:y p2))]
    {:x x :y y}))

(defn add-door
  ([furniture wall distance]
   (add-door furniture wall distance {}))
  ([furniture wall distance {:keys [width] :as opts}]
   (let [{:keys [start end thickness] :as wall} (coerce-wall wall)
         angle                                  (math/points-angle start end)
         {:keys [x y] :as center} (if (= distance :center)
                                    (center start end)
                                    (let [point (if (> distance 0) start end)]
                                      (math/move-point point angle distance)))
         door                                   (HomePieceOfFurniture. furniture)]
     (when width
       (.setWidth door width))
     (doto door
       (.setDepth thickness)
       (.setAngle angle)
       (.setX x)
       (.setY y))
     (-> @state :home (.addPieceOfFurniture door))
     door)))

(defn add-furniture
  [furniture {:keys [x y angle height depth width mirrored] :as item}]
  (let [f             (HomePieceOfFurniture. furniture)
        {:keys [x y]} (resolve-walls item {})
        ;;x             (eval-form x :x)
        ;;y             (eval-form x :y)
        ]
    (doto f
      (.setX x)
      (.setY y))
    (when angle (.setAngle f angle))
    (when height (.setHeight f height))
    (when depth (.setDepth f depth))
    (when width (.setWidth f width))
    (when mirrored (.setModelMirrored f mirrored))
    (-> @state :home (.addPieceOfFurniture f))
    ))

(when false
  (invoke-ui
   (fn []
     (let [furn (load-furniture "Staircases" "OlaKristianHoff#stair_straight_open_stringer")
           furn (HomePieceOfFurniture. furn)]
       #_ (doto furn
            
            )
       (-> @state :home (.addPieceOfFurniture furn))
       )))
  )
