;; Developers Guid: http://www.sweethome3d.com/pluginDeveloperGuide.jsp

(ns jam.sweethome3d
  (:require [clojure.walk :refer [postwalk]])
  (:import [com.eteks.sweethome3d.model Level Wall Room DimensionLine HomeTexture CatalogTexture]
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

(def state
  "Application state is accessed through the Plugin."
  (let [plugin (NReplPlugin/plugin)
        homeController (.getHomeController plugin)]
    (atom {:plugin plugin
           :home (.getHome plugin)
           :homeController homeController
           :planController (.getPlanController homeController)
           :homeController3D (.getHomeController3D homeController)  ;; display levels, set cameras
           :userPreferences (.getUserPreferences plugin)
           :walls {} ; map of walls created
           })))

;; 3D View Control
;; (com.eteks.sweethome3d.viewcontroller.Home3DAttributesController.)
;; plugin getHomeController getHomeController3D
(defn set-walls-alpha
  [alpha]
  (-> @state :home .getEnvironment (.setWallsAlpha alpha)))
#_ (set-walls-alpha 0.0)
(set-walls-alpha 0.35)
#_ (set-walls-alpha 1.0)
#_ (invoke-ui #(set-walls-alpha 0.0))

;; TexturesCatalog - has list of TexturesCatagory
;; TexturesCatagory - has list of CatalogTexture
(defn find-texture
  [category-name id]
  (let [catalog  (-> @state :userPreferences .getTexturesCatalog)
        category (->> catalog .getCategories (filter #(= category-name (.getName %))) first)
        texture (->> category .getTextures (filter #(= id (.getId %))) first)]
    (HomeTexture. texture)))

;; Categories are:
"Fabric" "Floor" "Miscellaneous" "Roof" "Rug" "Sky" "Wall" "Wallpaper" "Wood"

;; ROOM
;; - points
;; - level
;; - ceiling color/texture
;; - floor color/texture
;; - name
;; - area visible, style
;; -? how to add?
;;    -? home?
;;    -? plan controller?
(defn float-array2
  [points]
  (->> points
       (postwalk #(if (number? %)
                    (float %)
                    %))
       (map float-array)
       into-array))

;; Make room for north east bedroom.
;; Note: the room points do not include the wall widths
#_ (let [room (-> [[7.5 7.5]
                   [595 7.5]
                   [595 315]
                   [7.5 315]]
                  float-array2
                  Room.
                  )]
     (-> @state :home (.addRoom room))
     (.setFloorTexture room (find-texture "Floor" "eTeksScopia#english-parquet-1"))
)

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
(defn create-room
  [point]
  (let [controller       (-> @state :planController)
        roomDrawingState (private-field controller "roomDrawingState")
        room             (private-method roomDrawingState "createRoomAt" (-> point :x float) (-> point :y float))]
    room))

;; Create room from point and add floor texture.
#_ (invoke-ui (fn []
                (let [room (create-room {:x 100 :y 100})]
                  (.setFloorTexture room (find-texture "Floor" "eTeksScopia#english-parquet-1"))
                  room
                  )))

;; <doorOrWindow level='level0' catalogId='eTeks#doorFrame' name='Door frame' creator='eTeks' model='1' icon='0' x='599.9834' y='271.24023' angle='1.5707964' width='91.4' depth='12.700748' height='208.3' movable='false' dropOnTopElevation='-0.004801229' wallThickness='0.7874016' wallDistance='0.10498688' cutOutShape='M0,0 v1 h1 v-1 z'/>


;; DIMENSION
;; DimensionLine(float xStart, float yStart, float xEnd, float yEnd, float offset)
#_ (let [[x1 y1 x2 y2 offset] [7.5 7.5, 595.0 7.5, 20.0]
         line (DimensionLine. x1 y1 x2 y2 offset )]
     (-> @state :home (.addDimensionLine line)))

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

(def find1
  (comp first filter))

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


;; lenghts in cm
(def defaults
  {:level {:total-height 300 ; total height
           :thickness 25 ; floor
           :height (- 300 25); empty space
           }
   :width 640
   :wall {:exterior {:thickness 15}
          :interior {:thickness 10}}})

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
  (let [{:keys [elevation thickness height]} (merge (:level defaults) props)
        level (Level. name (float elevation) (float thickness) (float height))]
    (-> @state :home (.addLevel level))))

;; select keys :x :y
;; if v is keyword
;;   find wall (or throw exception)
(defn get-wall
  ([k] (get-wall k true))
  ([k assert_]
   (or (-> @state :walls k)
       (if assert_
         (throw (Exception. (str "Can't find wall " k)))))))
#_ (get-wall :house-S-0)

(defn wall-to-clj
  "Converts a wall to a clojure map based data structure."
  [wall]
  {:start {:x (.getXStart wall) :y (.getYStart wall)}
   :end   {:x (.getXEnd wall)   :y (.getYEnd wall)}})

(def ^:dynamic -eval-key)
(defn dim
  [wall]
  (prn "dim: wall=" wall)
  (or (-> wall :start -eval-key) ; extract from wall
      (-> wall -eval-key)))      ; extract from point
#_ (binding [-eval-key :x]
     (let [wall (-> :house-S-0 get-wall wall-to-clj)]
       (eval `(dim ~wall))))

(defn coerce-wall
  "Coerces value to a wall if a matching wall exists."
  [v]
  (if-let [wall (and (keyword? v) (get-wall v false))]
    (wall-to-clj wall)
    (if (instance? Wall v)
      (wall-to-clj v)
      v)))
#_ (coerce-wall :foo)
#_ (coerce-wall :house-E-0)

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
            (let [v (cond
                      (keyword? v)                    (-> v get-wall coerce-wall :start k)
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

(defn midval
  [x1 x2]
  (/ (+ x1 x2) 2))
#_ (midval 1 3)

(defn midpoint
  ([p1 p2]
   {:x (midval (:x p1) (:x p2))
    :y (midval (:y p1) (:y p2))})
  ([wall]
   (let [wall (if (keyword? wall)
                (-> wall get-wall wall-to-clj)
                wall)]
     (midpoint (wall :start) (wall :end)))))
#_ (midpoint {:x 0 :y 0} {:x 5 :y 5})
#_ (midpoint :house-N-0)

#_ {:house-N-0 #(midpoint)}
#_ '(/ :house-N-0 2)
#_ '(midpoint :house-N-0)
;; ? how does it know to take the y coordinate?
;;   - it knows it is resolving a coordinate


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
                                  (-> defaults :level (select-keys [:height]))   ;; global defaults
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
                     (prn p1 p2)
                     (let [wall (Wall. (:x p1) (:y p1)
                                       (:x p2) (:y p2)
                                       (-> defaults :wall wall-type :thickness)
                                       (:height p1))]
                       (.setHeightAtEnd wall (:height p2))
                       wall))
         walls     (map make-wall (pairs points))]

     ;; Add names
     (doseq [[{:keys [name] :as point} wall] (zip points walls)]
       (when name
         (swap! state assoc-in [:walls name] wall)))

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


(do
  (defn build
    []
    (clean-home)

    (let [hw (-> defaults :width)]
      ;; ========== BOTTOM LEVEL ==========
      (add-level "bottom" {:elevation 0})

      ;; exterior walls
      (add-walls :exterior {:closed? true}
                 [[:x 0    :y 0  :house-E-0]
                  [:x 1400       :house-S-0]
                  [        :y hw :house-W-0]
                  [:x 0          :house-N-0]])

      ;; separate bedroom from main room
      (add-walls :interior
                 [[:x 600 :y :house-W-0 :bedroom-0]
                  [       :y :house-E-0           ]])

      ;; separate bedrooms from each other
      (add-walls :interior
                 [[:x :house-N-0 :y '(midpoint :house-N-0) :bedroom-sep-0]
                  [:x :bedroom-0                                         ]])

      ;; utility room
      (add-walls :interior
                 [[:x '(- (dim :house-S-0) 300) :y 0          :utility-0]
                  [                             :y :house-W-0           ]])

      ;; bath room
      (add-walls :interior
                 [[:x :bedroom-0 :y 150]
                  [:x :utility-0       ]])

      ;; closets
      (let [entry-width 100]
        (doseq [i [1 -1]]
          (add-walls :interior
                     [[:x '(- (dim :bedroom-0) 75) :y `(+ (dim :bedroom-sep-0) (* ~i ~entry-width))]
                      [:x :bedroom-0                                                               ]])))

      ;; ========== MAIN LEVEL ==========
      (add-level "main" {:elevation 300})

      ;; exterior walls
      (add-walls :exterior {:closed? true}
                 [[:x 0    :y 0  :house-E-1]
                  [:x 2300       :house-S-1]
                  [        :y hw :house-W-1]
                  [:x 0          :house-N-1]])

      ;; greenhouse
      (add-walls :interior
                 [[:x 375 :y 0 ]
                  [       :y hw]])

      ;; entry
      (let [bodega-width 475
            hall-width   125
            bath-width   125
            bath-length  (/ bodega-width 2)
            mud-width    bath-width
            mud-length   bath-length]
        (add-walls :interior
                   [[:x `(- (dim :house-S-1) ~bodega-width) :y 0                            :bodega-N-1]
                    [                                       :y (- hw hall-width bath-width) :bodega-W-1]
                    [:x :house-S-1                                                                     ]])

        ;; bath
        (add-walls :interior
                   [[:x :bodega-N-1                       :y hw               ]
                    [                                     :y (- hw bath-width)]
                    [:x `(- (dim :house-S-1) ~mud-length)                     ]
                    [                                     :y hw               ]]))

      ;; kitchen
      (let [stair-width 100
            fridge-width 80]
        (add-walls :interior
                   [[:x `(- (dim :bodega-N-1) (* 2 ~stair-width)) :y 0          ]
                    [                                             :y :bodega-W-1]
                    [:x #(- % fridge-width)                                     ]]
                   ))

      ;; garage
      (let [garage-width 675]
        (add-walls :exterior
                   [[:x :house-S-1          :y hw]
                    [:x #(+ % garage-width)        :garage-S-1]
                    [                       :y 0 ]]))

      ;; ========== TOP LEVEL ==========
      (add-level "top" {:elevation 600})

      (let [peak {:x 1900
                  :height 450}]
        (add-walls :exterior {:closed? true}
                   [[:x 0           :y 0  :height 0   ]
                    [:x peak              :height peak]
                    [:x :garage-S-1       :height 0   ]
                    [               :y hw             ]
                    [:x peak              :height peak]
                    [:x 0                 :height 0   ]
                    ]))

      ;; loft
      (add-walls :interior
                      ;; loft start x
                 [[:x 1110 :y 0  :height 50 :loft-N]
                  [        :y hw                   ]])
      ;; stair
      (add-walls :interior
                                        ;; loft width
                 [[:x '(+ (dim :loft-N) 370) :y 0        :stair-N-2]
                  [                          :y (/ hw 2)           ]])

      ;; closet
      (add-walls :interior
                 [[:x :stair-N-2 :y hw]
                            ;; closet width
                  [:y (- hw 190)]
                            ;; stair width
                                ;; closet width
                  [:x #(+ % 264 230) :closet-S-2]
                  [:y hw]
                  ])
      ;; bath
      (add-walls :interior
                                           ;; stair width
                 [[:x '(+ (dim :stair-N-2) 264) :y 0]
                  [                             :y (/ hw 2)]
                  [:x :closet-S-2]
                  [:y 0]])

      ;; bedroom
      (add-walls :interior
                                            ;; bedroom width
                 [[:x '(+ (dim :closet-S-2) 320) :y 0]
                  [:y hw]])

      ;; note: double stairs are 2.64 (1.32 each)
      ;;       seems too wide, how wide do stairs need to be for a king bed?
      ;;       can always lift large items over loft

      ;; kitchen/loft start at 1110 = (+ 449 457 204)
      ;; ? what is slope of wall?
      ;; ? how high is peak? appears to be at 4.5m (max is 5m)
      ;; ? what is x coordinate of peak?
      ;;   basically over the start of the master bedroom
      ;; ? how high is ceiling at:
      ;;   - greenhouse wall
      ;;   - outside of loft
      ;;   - inside edge of loft
      ;;   - inside edge of master bedroom

      ))

  (invoke-ui build))
;; - ? auto join? ? do we need to check for intersection?

;; todo: if anchoring at perpendicular wall, only need to supply one start coordinate
;;       also can adjust start point to match joined wall width
