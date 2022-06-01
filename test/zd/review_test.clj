(ns zd.review-test
  (:require
   [clojure.test :refer :all]
   [zd.release-review :as rr]))


(deftest github-api-link-making
  (testing "github api link making"
    (is
     (= (rr/get-api-link "https://github.com/author/project" "commits")
        "https://api.github.com/repos/author/project/commits"))
    (is
     (= (rr/get-api-link "https://www.github.com/author/project" "commits")
        "https://api.github.com/repos/author/project/commits"))
    (is
     (= (rr/get-api-link "github.com/author/project" "commits")
        "https://api.github.com/repos/author/project/commits"))
    (is
     (= (rr/get-api-link "https://www.github.com/author/project/" "commits")
        "https://api.github.com/repos/author/project/commits")))) ;; TODO: fix this case
