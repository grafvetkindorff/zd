(ns zd.methods)

(defmulti render-key     (fn [ztx {pth :path}] pth))
(defmulti render-block   (fn [ztx {{blk :block} :annotations}] (keyword blk)))
(defmulti render-content (fn [ztx {{cnt :content} :annotations}] (keyword cnt)))
(defmulti annotation     (fn [nm params] (keyword nm)))
(defmulti inline-method (fn [ztx m arg] (keyword m)))
(defmulti inline-function (fn [ztx m arg] (keyword m)))
(defmulti process-block (fn [ztx tp args cnt] tp))
