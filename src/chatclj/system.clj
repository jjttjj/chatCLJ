(ns chatclj.system
  (:require [shadow.cljs.devtools.server :as shadow-server]
            [shadow.cljs.devtools.api :as shadow-api]

            [datomic.api :as d]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.browse :as browse]

            [chatclj.server :as server]
            [taoensso.timbre :as log]))

(def default-config
  {:datomic/uri           "datomic:mem://chatCLJ"
   :server/host           "0.0.0.0"
   :server/port           8888
   :server/resources-path "public"
   :dev?                  true})

(defn start-logging []
  (log/merge-config!
    {:min-level
     [[#{"datomic.*"} :warn]
      [#{"org.eclipse.*"} :warn]
      [#{"*"} :debug]]}))


(defonce latest-db (atom nil))
(defonce tx-queue nil)
(defonce conn nil)
(defonce server nil)

(defn reset-db-atom-loop [conn queue db-atom]
  (let [stop-prom (promise)
        stop-fn   (fn [] (deliver stop-prom true))
        schema    (edn/read-string (slurp (io/resource "chatclj/schema.edn")))]
    (when-let [f (:stop-fn (meta db-atom))] (f))
    (alter-meta! db-atom assoc :stop-fn stop-fn)
    (future
      (log/info "starting db-atom loop")
      (loop []
        (if (deref stop-prom 0 nil)
          (log/info "db-atom loop stopped")
          (do
            (log/debug "resetting db-atom")
            (reset! db-atom
              (:db-after (.take ^java.util.concurrent.LinkedBlockingQueue queue)))
            (recur)))))
    (d/transact conn schema)
    db-atom))

(defn start-datomic [config]
  (if conn
    :already-started
    (let [uri   (:datomic/uri config)
          _     (log/info "connecting to datomic at" uri)
          _     (d/create-database uri)
          conn  (d/connect uri)
          queue (d/tx-report-queue conn)]

      (alter-var-root #'conn (constantly conn))
      (alter-var-root #'tx-queue (constantly tx-queue))
      (reset-db-atom-loop conn queue latest-db)
      :started)))


(defn start-server [config] (server/start-server! config))
(defn stop-server [server] (.stop server))

(defn setup-dev []
  (shadow-server/start!) ; serves index.html as well
  (shadow-api/watch :dev) ; depends on shadow server
  )

(defn setup-prod []
  (shadow-api/compile :prod))

(defn start [& {:as config}]
  (let [config (merge default-config config)]

    (start-logging)

    (if (:dev? config)
      (setup-dev)
      (setup-prod))

    (start-datomic config)


    (alter-var-root #'server (fn [x] (when-not x (start-server config))))

    (when (:dev? config)
      (browse/browse-url (str "http://localhost:" (:server/port config))))
    :ok))

(comment

  (start)

  (stop-server server)
  (start-server default-config)
  )
