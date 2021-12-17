(ns zd.core
  (:require
   [zen.core :as zen]
   [zd.db]
   [zd.pages]
   [zd.web]
   [clojure.walk]
   [edamame.core]))


(defn reload [ztx _opts]
  (swap! ztx dissoc :zdb)
  (let [dirs (:zd/paths @ztx)]
    (zd.db/load-dirs ztx dirs))
  :ok)

(defn dispatch [ztx {uri :uri}]
  (reload ztx {})
  (let [sym (symbol (subs uri 1))]
    (if-let [page (zd.db/get-page ztx sym)]
      {:status 200
       :body  (zd.pages/render-page ztx page)}
      {:status 404
       :body  (zd.pages/render-not-found ztx sym)})))

(defn start [ztx opts]
  (reload ztx opts)
  (zen.core/read-ns ztx 'zd)
  (zd.web/start ztx opts dispatch))

(defn stop [ztx]
  (zd.web/stop ztx))

(comment

  (System/getProperty "user.dir")

  (def pth "docs")

  (def pth "/Users/ghrp/prog/sansara/box/docs")

  (def pth "/Users/ganshinv/hs/sansara/box/docs")
  (def pth "/Users/niquola/sansara/box/docs")

  (def ztx (zen/new-context {:zd/paths [pth] :paths [pth]}))

  (start ztx {:port 3030})

  (stop ztx)

  )
