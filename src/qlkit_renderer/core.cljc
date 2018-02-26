(ns qlkit-renderer.core
  (:require #?@(:cljs [[react-dom :refer [render]]
                       [react :refer [createElement]]
                       [create-react-class :refer [createReactClass]]])
            [qlkit-renderer.dom :as dom]
            [qlkit.core :as ql]
            [clojure.string :as st]))

#?(:clj
   (defmacro defcomponent [nam & bodies]
     "This macro overrides the qlkit defcomponent macro to remove explicit 'this'"
     `(qlkit.core/defcomponent*
        ~nam
        ~@(for [[k & v :as body] bodies]
            (if (#{'render 'component-did-mount 'component-will-unmount 'component-will-receive-props} k) ;; TODO update as qlkit supports more React hooks
              `(~k [this# ~@(first v)]
                (binding [*this* this#]
                  ~@(rest v)))
              body)))))

(defn- camel-case [s]
  "Convert a shishkabob string to camelcase"
  (let [words (st/split s #"-")]
    (apply str
           (first words)
           (for [word (rest words)]
             (str (st/upper-case (first word)) (subs word 1))))))

(defn- camel-case-keys [kv-map]
  "CamelCases all the keys, but only if they are keywords."
  (into {}
        (for [[k v] kv-map]
          [(if (keyword? k)
             (keyword (camel-case (name k)))
             k)
           v])))

(def ^:dynamic *this* nil)

(defn transact! [& query]
  (apply ql/transact!* *this* query))

(defn- fix-event-references [this props]
  "This function decouples events from using the traditional javascript 'this' context into something that can be managed in a more clojure-y way."
  (into {}
        (for [[k v] props]
          (if (fn? v)
            [k (fn [& args]
                 (binding [*this* this]
                   (apply v args)))]
            [k v]))))

(defn- fix-classname [props]
  "React doesn't permit the standard html 'class' property, this function reenables it when using qlkit."
  (if (contains? props :class)
    (-> props
        (dissoc :class)
        (assoc :className (:class props)))
    props))

(defn- gather-style-props [props]
  "Gathers legal DOM style elements in style tag. Can override this behavior by using string key instead of keyword key."
  (let [{root-props false styles true} (group-by (fn [[k v]]
                                                   (some? (dom/style-attributes k)))
                                                 props)]
    (cond-> (into {}
                  root-props)
      (seq styles) (assoc :style
                          (into {}
                                (camel-case-keys styles))))))


#?(:cljs (do (declare create-element)
             
             (defn- fix-inline-react [this props]
               "These are idiosyncratic properties that may emit raw react components that need to be created."
               (cond-> props
                 (contains? props :actions) (update :actions
                                                    (fn [actions]
                                                      (apply array (map (partial create-element this) actions))))
                 (contains? props :right-icon) (update :right-icon (partial create-element this))))

             (defn- ensure-element-type [typ]
               (or (cond (keyword? typ) (or (@ql/component-registry typ)
                                            (when (dom/valid-dom-elements typ)
                                              (name typ)))
                         (string? typ)  (when (dom/valid-dom-elements (keyword typ))
                                          typ))
                   (throw (ex-info "Not a valid dom element" {:type typ}))))

             (defn- create-element [this el]
               "This function takes an edn structure describing dom elements and instantiates them with them via React."
               (if (or (string? el) (number? el))
                 el
                 (let [[typ & more]     el
                       [props children] (if (map? (first more))
                                          [(first more) (rest more)]
                                          [{} more])
                       children         (vec (map (partial create-element this) (ql/splice-in-seqs children)))]
                   (if (and (keyword? typ) (namespace typ))
                     (ql/create-instance typ props)
                     (apply createElement
                            (ensure-element-type typ)
                            (->> props
                                 gather-style-props
                                 (fix-event-references this)
                                 fix-classname
                                 (fix-inline-react this)
                                 camel-case-keys
                                 clj->js)
                            children)))))

             (swap! ql/rendering-middleware conj create-element)
             
             (defn update-state! [fun & args]
               "Update the component-local state with the given function"
               (apply ql/update-state!* *this* fun args)))) 
