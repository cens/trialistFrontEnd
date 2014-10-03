(ns trialist.core
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.util.response :as response]))

(defn ping [] true)

(defn echo [params] params)

(defroutes app-routes
  (route/resources "/")
  (route/not-found "Not Found"))

(def handler (handler/site app-routes))
