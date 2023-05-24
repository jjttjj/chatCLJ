(ns ^:dev/always chatclj.main ; Electric currently needs to rebuild everything when any file changes. Will fix
  (:require [chatclj.app :as app]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))




(def electric-main
  (e/boot ; Electric macroexpansion - Clojure to signals compiler
    (binding [dom/node js/document.body]
      (app/Main.))))

(defonce reactor nil)

(defn ^:dev/after-load ^:export start! []
  (assert (nil? reactor) "reactor already running")
  (set! reactor (electric-main
                  #(js/console.log "Reactor success:" %)
                  #(js/console.error "Reactor failure:" %))))

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))
