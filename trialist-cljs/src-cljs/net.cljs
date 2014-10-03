(ns trialist.net
  (:require [cljs.reader :as reader]
            [goog.net.XhrIo :as xhr]
            [goog.net.Cookies :as cook]
            [goog.events :as events]))


(defn edn->params [edn]
    (apply str
     (interpose "&"
      (for [pair edn]
       (let [k (first pair)
             v (second pair)]
         (str (name k) "=" v))))))

(defn POST
  "Callback-fn has on arg, thus:
   (fn [response])"
  [uri data callback-fn]
  (.send goog.net.XhrIo
         uri
         (fn [res]
           (callback-fn
            (js->clj
             (.getResponseJson (aget res "target"))
             :keywordize-keys true)))
         "POST"
         (edn->params data)))

(defn GET
  [uri callback-fn]
  (.send goog.net.XhrIo
         uri
         callback-fn
         "GET"))
