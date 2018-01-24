(ns qlkit.qlkit-test
  (:refer-clojure :rename {read core-read sync core-sync})
  (:require [clojure.test :refer [deftest is]]
            [qlkit-renderer.core :as qlr]
            [qlkit.core :as ql]))

(defmulti read   (fn [a & args] (first a)))
(defmulti mutate (fn [a & args] (first a)))
(defmulti remote (fn [a & args] (first a)))
(defmulti sync   (fn [a & args] (first a)))

(deftest camel-case-test []
  (is (= (#'qlr/camel-case "foo-bar")
         "fooBar")))

(deftest camel-case-keys-test []
  (is (= (#'qlr/camel-case-keys {:foo 1
                              "bar" 2
                              :foo-bar 3
                              :fooDerp 4
                              33 5})
         {:foo 1, "bar" 2, :fooBar 3, :fooDerp 4, 33 5})))

(deftest fix-event-references-test []
  ;;If we bind 4 to the *this* dynvar, we can override this value with a value handed to the fix-event-references function
  (binding [qlr/*this* 4]
    (let [result      (atom nil)
          props       {:foo (fn []
                              (reset! result qlr/*this*))}
          fixed-props (#'qlr/fix-event-references 5 props)]
      ((:foo fixed-props))
      (is (= @result 5)))))

(deftest fix-classname-test []
  (is (= (#'qlr/fix-classname {:class "foo"})
         {:className "foo"}))
  (is (= (#'qlr/fix-classname {:foo "foo"})
         {:foo "foo"})))

(deftest gather-style-props-test []
  ;;Official dom style elements are gathered into style map
  (is (= (#'qlr/gather-style-props {:color :blue :foo 5})
         {:foo 5
          :style {:color :blue}}))
  ;;Can override this behavior by using string keys
  (is (= (#'qlr/gather-style-props {"color" :blue :foo 5})
         {:foo 5
          "color" :blue})))

(deftest transact!-test []
  (let [state (atom {})]
    (remove-all-methods sync)
    (remove-all-methods read)
    (remove-all-methods mutate)
    (remove-all-methods remote)
    (defmethod read :foo
      [query-term env state]
      42)
    (defmethod remote :foo
      [query-term state]
      query-term)
    (defmethod sync :foo
      [query-term result env state-atom]
      (swap! state-atom assoc :foo result))
    (ql/mount {:remote-handler (fn [query callback]
                                 (callback [:yup]))
               :parsers        {:sync sync
                                :read read
                                :mutate mutate
                                :remote remote}
               :state          state})
    (qlr/transact! [:foo])
    (is (= @state {:foo :yup}))))
