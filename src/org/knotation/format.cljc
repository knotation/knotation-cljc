(ns org.knotation.format
  (:require [org.knotation.state :as st]))

(def formats (atom {}))

(def example-formats
  {"https://knotation.org/format/knotation"
   {::name "https://knotation.org/format/knotation"
    ::description "Foo"
    ::read identity
    ::render identity}})

(defn register!
  [{:keys [::name] :as format}]
  (swap! formats assoc name format))

(defn read-mode-function
  [mode format lines]
  (let [format (or format :kn)
        func (get-in @formats [format ::read])]
    (if func
      (fn [states]
        (->> lines
             (func
              (merge
               (last states)
               (when mode {::st/mode mode})))
             (concat states)))
      (fn [states]
        (concat
         states
         [{::st/error
           {::st/error-type :unknown-read-format
            ::st/error-message
            (str "Unknown read format: " format)}}])))))

(defn read-function
  [format lines]
  (read-mode-function nil format lines))

; TODO: improve on this implementation
(defn read-env-function
  [format lines]
  (read-mode-function :env format lines))

; TODO: improve on this implementation
(defn read-data-function
  [format lines]
  (read-mode-function :data format lines))

(defn render-function
  [format]
  (get-in
   @formats
   [(or format :nq) ::render]
   (fn [states]
     (map
      #(assoc
        %
        ::st/error
        {::st/error-type :unknown-render-format
         ::st/error-message
         (str "Unknown render format: " format)})
      states))))
