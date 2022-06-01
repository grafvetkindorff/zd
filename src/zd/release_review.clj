(ns zd.release-review
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clj-http.client :as http]))


(defn days-ago
  "Returns the current date minus [days].
   Warning: passing negative arg takes you to the future!"
  [days]
  (let [month-ago (doto (java.util.Calendar/getInstance)
                    (.add java.util.Calendar/DATE (- days)))
        formatter (java.text.SimpleDateFormat. "yyyy-MM-dd")]
    (.setCalendar formatter month-ago)
    (.format formatter (.getTime month-ago))))


(defn get-request-body
  [link parse? & headers] ;; TODO: using token
  (try
    (let [auth-header {"Authorization" "token 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"}
          body        (:body (http/get link
                                       (if (nil? headers)
                                         {:headers auth-header}
                                         (assoc {} :headers
                                                (conj auth-header (reduce conj headers))))))]
      (if parse?
        (json/parse-string body true)
        body))
    (catch Exception e (println "Some exception occured: " (.getMessage e))
           ())))


(defn get-api-link
  "Makes a API link from the given url.
   For example:
   https://github.com/<author>/<project> will be converted
   to https://api.github.com/repos/<author>/<project>/commits"
  [link part]
  (let [author-project
          (take-last 2 (re-matches #"(https://)?(www\.)?github.com/(.+)/(.+)/?" link))
        author (first author-project)
        project (second author-project)]
    (str "https://api.github.com/repos/" author "/" project "/" part)))


(defn gather-pages
  "Get all entries since given date using pagination.
   Take pages until string parsing gives non-() page."
  [api-link parse? & headers]
  (let [detailed-api-link (str api-link (if (str/includes? api-link "?")
                                          "&page="
                                          "?page="))]
    (reduce concat
            (take-while
             #(not= () %)
             (map #(get-request-body (str detailed-api-link "&page=" %) parse? headers)
                  (iterate inc 1))))))


(defn get-commits-diff
  [api-link sha1 sha2]
  (get-request-body (str api-link "/" sha1 "..." sha2)
                    false
                    {"Accept" "application/vnd.github.VERSION.diff"}))


(defn get-issue-number
  "Get a number from the braces [<project name>#number].
   TODO: incapsulate getting of issue numbers."
  [commit]
  (some->> (get-in commit [:commit :message])
           (re-matches #"(\[.*\])\s?.*")
           last
           (re-matches #"\[(.*\#)(.*)\]")
           last))


(defn list-files
  "Makes a map with info about each file changed in the given commit.
   Each map corresponds to the file in commit."
  [commit]
  (let [files (try
                (:files (get-request-body (:url commit) true))
                (catch Exception e (println "Some exception occured: " (.getMessage e))
                       []))]
    (assoc commit :changed-files
           (into []
                 (map #(assoc {}
                              :filename (:filename %)
                              :patch (:patch %)
                              :sha (:sha commit)
                              :message (get-in commit [:commit :message])
                              :date (get-in commit [:commit :committer :date])
                              :parent (:sha (first (:parents commit))))
                      files)))))


(defn make-diff
  [filename patch]
  (str "diff --git a/" filename " b/" filename
       "\n"
       patch))


(defn make-total-patches-block
  "Makes a block with total changes by file.
   Sorting patches by date is necessary to get a right range between commits (by shas)."
  [filename patches api-link]
  (let [sorted           (sort-by :date patches)
        first-sha-parent (:parent (first sorted))
        first-sha        (:sha (first sorted))
        last-sha         (:sha (last sorted))]
    (if (= first-sha last-sha) ;; it is only one commit
      [:div {:class "diff-element"}
       (make-diff filename (:patch (first patches)))]
      (let [diff          (get-commits-diff (str api-link "/compare") first-sha-parent last-sha)
            diff-by-files (str/split diff #"diff --git ")
            filtered      (filter #(str/includes? % filename) diff-by-files)]
        (into [:div {:class "diff-element"}]
              (into [] (map #(str "diff --git " %) filtered)))))))


(defn make-patches-block
  "A block of changes from other commits (for files related to this one)."
  [filename patches exclude-commit-sha]
  (into [:div]
        (map #(if (or (nil? (:patch %))
                      (= (:sha %) exclude-commit-sha))
                [:div]
                [:div
                 [:p "from commit: " (:message %)]
                 [:div {:class "diff-element"}
                  (make-diff filename (:patch %))]])) patches))


(defn make-diff-from-patches
  [patches]
  (str/join "\n" (map #(make-diff (:filename %) (:patch %)) patches)))


(defn draw-commit
  [commit other-changes?]
  (let [commit-sha           (:sha commit)
        commit-message       (get-in commit [:commit :message])
        all-files            (:all-changed-files commit)
        groupped-by-filename (group-by :filename all-files)
        commit-files         (get (group-by :sha all-files) commit-sha)]
    [:div
     [:h3 commit-message]
     [:div {:class "diff-element"} (make-diff-from-patches commit-files)]
     (if other-changes?
       [:div
        [:h4 "See other changes: "]
        [:button {:onclick (str "toggle(\"" commit-sha "\")")}
         (str "show files")]
        [:div {:id    commit-sha
               :style "display:none"}
         (map #(make-patches-block % (get groupped-by-filename %) commit-sha)
              (map :filename commit-files))]]
       [:div])]))


(defn draw-commits-block
  "Each block have id = 'Issue #<number>'.
   Toggling by this id."
  [id commits other-changes? total-needed?]
  (let [commit-files (group-by :filename
                               (reduce into (map :changed-files commits)))
        api-link (last (re-matches #"(https://api.github.com/repos/.*/.*/)commits.*"
                                   (:url (first commits))))]
      [:div
       [:h2 id]
       [:button {:onclick (str "toggle(\"" id "\")")}
        (str "Show commits (" (count commits) ")")]
       [:div {:id id :style "display:none"}
        (map #(draw-commit % other-changes?) commits)
        (if total-needed?
          [:div [:h3 "Total changes:"]
           (map #(make-total-patches-block % (get commit-files %) api-link) (keys commit-files))]
          [:div])]]))


(defn draw-issue
  [issue-number commits]
  (draw-commits-block
   (str "Issue #" issue-number)
   (filter #(= issue-number (get-issue-number %)) commits)
   true
   true))
