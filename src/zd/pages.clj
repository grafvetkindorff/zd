(ns zd.pages
  (:require
   [zd.zentext]
   [zd.db]
   [zd.methods]
   [zd.impl]
   [hiccup.core]
   [hiccup.page]
   [hiccup.util]
   [markdown.core]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [stylo.rule :refer [rule join-rules]]
   [garden.core]
   [cheshire.core :as json]
   [clojure.java.io :as io]))

(defmethod rule :grid-template-areas
  [_ & xs]
  [[:& {:grid-template-areas (apply pr-str xs)}]])


(defn to-html [x] (hiccup.core/html x))

(def closed-node-style (c [:bg :red-500]))

(defn c* [& args]
  (join-rules args))

(def common-style
  [:body {:font-family "sohne, \"Helvetica Neue\", Helvetica, Arial, sans-serif;" :padding "0" :margin "0"}
   [:h1 (c* :border-b {:font-size "32px" :margin-top "10px" :margin-bottom "16px" :font-weight "600"})]
   [:h2 (c* :border-b {:font-size "24px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "30px"})]
   [:h3 (c* :border-b {:font-size "20px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "25px"})]
   [:h4 (c* {:font-size "16px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "20px"})]
   [:h5 (c* {:font-size "14px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "16px"})]
   [:ul (c* [:ml 4] [:mb 4])
    {:list-style "inside"
     :line-height "24px"}
    [:li {:display "list-item"
          :list-style "outside"}]
    [:ul (c* [:mb 0])]]
   [:ol (c* [:ml 4]
            {:list-style "disk inside"
             :line-height "24px"})
    [:li (c* [:my 1]
             {:display "list-item"
              :list-style "outside decimal"})]
    [:ol (c* [:ml 4])]]

   [:p (c* [:mb 4] {:line-height "1.5rem"})]

   [:.hljs (c* [:bg :gray-100] :shadow-sm
               :border)]
   [:.active-nav {:border-bottom "2px solid #666" :color "black"}]
   [:pre {:margin-top "1rem" :margin-bottom "1rem"}]
   [:.closed {:display "none"}]
   [:.bolder (c* :font-bold)]
   [:.nlSaver {:white-space "pre-wrap"}]
   [:.searchResultContainer (c* [:px 6] [:py 3] :flex :flex-col
                                [:hover :cursor-pointer [:bg :gray-200]])]
   [:.searchResultContainerRow (c* :flex)]
   [:.searchResultContainerSummaryRow (c* :border-t)]
   [:.searchResultContainerVBar (c* [:h "30px"] [:w "2px"]
                                    :rounded [:mr 2] [:bg :blue-500])]
   [:.visible {:visibility "visible"}]
   [:.pl-4  {:padding-left "1rem"}]
   [:.toggler {:padding-left "4px"
               :padding-right "4px"
               :padding-top "2px"
               :padding-bottom "2px"}]
   [:.rotateToggler {:transform "rotate(-90deg)"}]
   [:.searchContainer {:position "fixed"
                       :width "90%"
                       :height "100%"
                       :top 0
                       :transition "transform 0.3s 0.3s"}]


   [:.mindmap
    [:.node
     [:circle {:fill "#aaa"}]
     [:text {}]]
    [:.node--internal [:circle {:fill "#999"}]]
    [:.link {:fill "none"
             :stroke "#aaa"
             :stroke-opacity "0.4"
             :stroke-width "1.5px"}]]

   [:.zd-toggle [:.zd-content {:height "0"
                               :transform "scaleY(0) "
                               :transform-origin "top"
                               :transition "all 0.26s ease"}]]
   [:.zd-toggle.zd-open
    [:.zd-content {:transform "scaleY(1)"
                   :height "auto"}]
    [:.zd-block-title [:.fas {:transform "rotate(90deg)"
                              :transition "all 0.26s"}]]]])


(defn layout [ztx content]
  (zd.db/index-refs ztx)
  [:html
   [:head
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:style (garden.core/css common-style)]
    [:meta {:charset "UTF-8"}]
    [:link {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/default.min.css", :rel "stylesheet"}]
    [:link {:href "//cdn.jsdelivr.net/npm/diff2html/bundles/css/diff2html.min.css", :rel "stylesheet"}]
    [:link  {:href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"  :rel "stylesheet"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/clojure.min.js"}]
    [:script {:src "//cdn.jsdelivr.net/npm/diff2html/bundles/js/diff2html-ui.min.js"}]
    [:script {:src "/js/d3.js"}]
    [:script {:src "/js/mindmap.js"}]
    [:script {:src "/js/zendoc.js"}]
    [:script "hljs.highlightAll()"]
    [:script
     "document.addEventListener('DOMContentLoaded', function () {
        var diffs = document.getElementsByClassName('diff-element');

        var configuration = {
          drawFileList: true,
          fileListToggle: true,
          fileListStartVisible: false,
          fileContentToggle: true,
          matching: 'lines',
          outputFormat: 'side-by-side',
          synchronisedScroll: true,
          highlight: true,
          renderNothingWhenEmpty: false,
        };

        for(var counter = 0; counter < diffs.length; counter++) {
          var diff2htmlUi = new Diff2HtmlUI(diffs[counter], diffs[counter].textContent, configuration);
          diff2htmlUi.draw();
          diff2htmlUi.highlightCode();
        }
      });

      function toggle(element_id) {
        var element = document.getElementById(element_id);

        if (element.style.display == 'block') {
          element.style.display = 'none';
        } else {
          element.style.display = 'block';
        }
      }"]]
   [:body {:class (c {:background-color "#F4F7F9" :overflow "hidden" :height "100vh"}  :w-max-full)}
    [:div#overlay
     {:class (c :fixed [:top 0] [:left 0] :h-min-full :w-min-full :overflow-y-hidden
                {:z-index 1} {:background-color "rgba(0, 0, 0, 0.4)"} {:visibility "hidden"})}]
    content
    [:script (format "\nconst searchData = %s;\n%s"
                     (json/encode (zd.db/index-refs ztx))
                     (slurp (io/resource "js/tree.js")))]

    [:script {:src "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"}]
    [:script "mermaid.initialize({startOnLoad:true});"]]])

(defn build-tree [ztx doc]
  (->>
   (sort-by first (:zdb @ztx))
   (filter #(not (contains? #{'logo} (get % 0))))
   (reduce (fn [acc [nm doc]]
             (let [parts (interpose :items (str/split (name nm) #"\."))]
               (assoc-in acc parts {:title (or (get-in doc [:resource :title])
                                               (last parts))
                                    :avatar    (or (get-in doc [:resource :avatar]) (get-in doc [:resource :logo]))
                                    :icon      (get-in doc [:resource :icon])
                                    :menu-order (get-in doc [:resource :menu-order] 10)
                                    :name      (last parts)
                                    :href (str nm)}))) {})))

(defn build-menu* [ztx {ref :ref :as item} doc]
  (let [res  (zd.db/get-resource ztx ref)
        title (or (:title item) (:title res)  (last (str/split (str ref) #"\.")))]
    (merge item
           {:title title
            :broken (nil? res)
            :href (str ref)
            :items (when (and (:menu res) (sequential? (:menu res)))
                     (->> (:menu res)
                          (map-indexed (fn [i x] [i x]))
                          (reduce (fn [acc [i x]]
                                    (assoc acc i (build-menu* ztx x doc)))
                                  {})))})))


(defn menu-item-sort [[_ x]] (format "%07d %s" (get x :menu-order 10) (:title x)))

(defn render-items [item & [k]]
  [:div {:id  (str/lower-case k) :class "closable"}
   [:a {:href (when-not (:broken item) (:href item))
        :class (->> [(c :inline-block :flex :items-center
                        [:pl 2]
                        [:py 2]
                        :rounded [:hover :cursor-pointer [:bg :white] [:text :gray-700]])
                     (when (:broken item) (c [:text :red-500]))]
                    (filterv identity)
                    (mapv name)
                    (str/join " "))}
    (when (:items item)
      [:span {:class (c [:w 6] [:hover :rounded  :cursor-pointer [:bg :gray-300]] :text-lg :flex :justify-center {:margin-left "-2px"})}
       [:i.fas.fa-caret-down.toggler.rotateToggler]])
    (if-let [ava (:avatar item)]
      [:img {:class (c [:w 4] [:h 4] [:mr 1] {:border-radius "100%"}) :src ava}]
      (let [ico (or (:icon item) [:fa-regular :fa-file])]
        [:span {:class (c [:w 4] [:h 4] :flex :items-center :justify-center :text-xs [:text :gray-500] [:mr 1])}
         [:i {:class (str/join " " (map name ico))}]]))

    [:span {:class (c [:ml 0.5])} (or (:title item) (:href item) k)
     (when-let [e (:errors item)] [:span {:class (->> [(c [:text :red-500] :text-xs [:px 1])]
                                                      (str/join " "))} e])]]
   (into [:div {:class (->> ["closed" "closableContent" (name (c :border-l [:ml 3]))]
                            (str/join " "))}
          (let [node-content
                (for [[k it] (->> (:items item)
                                  (sort-by menu-item-sort))]
                  (render-items it k true))]
            node-content)])])

(defn navigation [ztx doc]
  [:aside {:class (c [:text :gray-600] [:pl 6] [:py 8] {:max-height "calc(100vh - 80px)" :overflow-y "auto" :grid-area "navigation"} :text-sm)}
   [:div {:id "nav-files"}
    (for [[k it] (->> (build-tree ztx doc)
                      (sort-by menu-item-sort))]
      (render-items it k))]])


(defn breadcrumb [_ztx name]
  (let [parts (str/split (str name) #"\.")]
    (->> (range (count parts))
         (mapv (fn [x]
                 (let [pth (into [] (take (inc x) parts))
                       nm  (str/join "." pth)]
                   [:a {:href (str "/" nm)
                        :class (c [:text :blue-500] [:px 2] {:border-right "1px solid #ddd"})}
                    (last pth)])))
         (into [:div {:class (c :flex :flex-1)}]))))


(defn links [ztx link-groups]
  [:div {:class (c [:text :gray-600])}
   (when (seq link-groups)
     (->> link-groups
          (reduce-kv (fn [acc k v]
                       (update acc (first k) (fnil into []) v))
                     {})
          (map (fn [[attr links]]
                 [:div {:class (c [:py 2] :text-sm)}
                  [:div {:class (c [:text :gray-600] :border-b [:mb 2] {:font-weight "600"})}
                   attr]
                  [:ul
                   (for [l (sort links)]
                     [:li (zd.impl/symbol-link ztx l)]
                     #_[:a {:href l :class (c :block [:py 0.5] [:text :gray-700] [:hover [:text :gray-800]])} l])]]))
          (into [:div {:class (c  [:py 2] [:px 4])}
                 [:span {:class (c [:text :black] :font-bold)} "Referenced By"]])))])


(defn page [ztx {doc :doc _res :resource :as page}]
  [:div {:class (c {:grid-area "content"} [:pb 8] :grid {:overflow-y "auto"
                                                         :max-height "calc(100vh - 80px)"
                                                         :overflow-x "hidden"
                                                         :grid-template-columns "1fr 230px"})}
   [:div
    [:div {:class (c :flex [:py 1])}
     (breadcrumb ztx (:zd/name page))
     [:div {:class (c :text-sm [:text :gray-600])}
      (:zd/name page)]]
    [:div {:class (c [:bg :white] [:py 4] [:px 8] :shadow-md
                     {:color "#3b454e"})}
     [:div {:class (c [:mb 4])}
      (->>
       (for [block doc]
         (let [block (assoc block :page page)]
           (or (zd.methods/render-key ztx block)
               (zd.methods/render-block ztx block)))))]]]
   (links ztx (:backrefs page))])


(defn search []
  [:div {:class (c :text-sm
                   [:text :gray-600]  [:hover [:bg :white]])}
   [:div#searchButton {:class (c :flex [:space-x 2] :items-baseline
                                 {:transition "color 0.2s ease"}
                                 [:hover :cursor-pointer [:text :black]])}
    [:span [:i.fas.fa-search]]
    [:span "Search... (alt + k)"]]])


(defn search-container [_ztx _doc]
  [:div#searchContainer
   {:class (c :fixed {:z-index 2} [:w "30%"] :h-min-screen [:bg :gray-100] [:top 0]
              [:text :gray-700]
              [:right 0] {:transition "transform 0.3s 0.3s" :visibility "hidden"} [:my 0])}
   [:div {:class (c :flex :flex-col)}
    [:div {:class (c :flex :items-center [:bg :white] [:pb 2] [:px 8] [:py 4])}
     [:span {:class (c [:mr 2] [:text :gray-500])} [:i.fas.fa-search]]
     [:input#searchInput
      {:placeholder "Search..."
       :class (c [:h "30px"] :flex-1 :text-xl
                 [:focus {:outline "none"}]
                 [:placeholder :text-lg :gray-500 :text-xl])}]
     [:span#searchContainerClose
      {:class (c [:text :gray-500] [:mr 2] {:transition "color 0.2s ease"}
                 [:hover :cursor-pointer [:text :black]])} "✕"]]
    [:div#searchResults
     {:class (c :flex :flex-col :h-max-screen :overflow-y-scroll)}]]])


(defn topbar
  [ztx doc]
  (let [src (get (:zdb @ztx) 'logo)
        logo (get-in src [:resource :logo])
        title (get-in src [:resource :title])]
    [:div {:class (c [:bg :white] {:height "80px"
                                   :position "fixed"
                                   :top 0
                                   :width "100%"
                                   :box-shadow "0px 4px 10px rgb(0 0 0 / 5%)"
                                   :border-bottom "1px"
                                   :border-bottom-color "rgba(211,220,228,1.00)"}
                     :grid
                     {:grid-template-columns  "300px 1fr 230px"})}
     [:div {:class (c  [:py 5])}
      [:a {:href "/readme"}
       [:div {:class (c [:px 6]
                        :flex
                        :items-center
                        [:border-r 2])}
        [:div {:class (c {:height "40px" :width "40px"} [:mr 4])}
         (when logo
           [:image {:src logo}])]
        [:div {:class (c :font-bold)}
         (when title
           title)]]]]
     [:div]
     [:div {:class (c  [:py 5])}
      [:div {:class (c :h-full [:px 6] [:py 1] [:border-l 2] :items-center :justify-start :flex)}
       (search)]]]))

(defn generate-page [ztx doc]
  (let [link-groups (zd.db/group-refs-by-attr ztx (:zd/name doc))]
    [:div {:class (c :h-max-full  :flex :flex-col)}
     (topbar ztx doc)
     [:div {:class (c :grid [:grid-template-areas "navigation content"]
                      {:grid-template-columns  "300px 1fr"
                       :margin-top "80px"})}
      (navigation ztx doc)
      (page ztx (assoc doc :backrefs link-groups))]
     (search-container ztx doc)]))

(defn zen-page [_ztx doc]
  [:div {:class (c [:w 260] [:bg :white] [:py 4] [:px 8] :shadow-md)}
   (when-let [t (:zen/name doc)]
     [:div {:class (c :flex :border-b [:pt 2] [:pb 1])}
      [:div {:class (c :flex-1 [:text :gray-800] :font-bold  {:font-size "1.5rem"})} t]])
   [:pre (pr-str doc)]])


(defn generate-zen-page [ztx doc]
  [:div {:class (c [:p 4] :flex [:space-x 4])}
   (navigation ztx doc)
   (zen-page ztx doc)])

(defn render-page [ztx doc]
  (->> (generate-page ztx doc)
       (layout ztx)
       (to-html)))

(defn render-not-found [ztx sym]
  (->> [:div {:class (c [:p 4] :flex [:space-x 4])}
        (navigation ztx nil)
        [:div {:class (c [:w 260]  [:py 4] [:px 8] [:text :gray-600])}
         (str "No page for '" sym)]]
       (layout ztx)
       to-html))

(defn render-zen [ztx doc]
  (->> (generate-zen-page ztx doc)
       (layout ztx)
       (to-html)))


