(ns timesheet.taxonomy-test
  (:require
   [clojure.test       :refer [is deftest testing]]
   [timesheet.query    :as query]
   [timesheet.taxonomy :as tax]))


(deftest taxonomy-test
  (let [taxonomy-map {"zero" {"one" {"two" ["three" "four"]
                                       "five" []}
                                "six" []}}
        taxonomy (tax/parse-taxonomy taxonomy-map nil #{})]
    (testing "Parsing works as expected"
      (is (= #{["two" "one"]
               ["three" "two"]
               ["four"  "two"]
               ["five"  "one"]
               ["one"   "zero"]
               ["six"   "zero"]}
             (set taxonomy))))
    (testing "subgroups"
      (with-redefs [query/taxonomy (constantly
                                    [{:subgroup "two"   :supergroup "one"}
                                     {:subgroup "three" :supergroup "two"}
                                     {:subgroup "four"  :supergroup "two"}
                                     {:subgroup "five"  :supergroup "one"}
                                     {:subgroup "one"   :supergroup "zero"}
                                     {:subgroup "six"   :supergroup "zero"}
                                     ])]
        (let [graph (tax/group-taxonomy)]
          (testing "Simple Case"
            (is (= (set ["three" "four" "two"])
                   (set (tax/subgroups graph "two")))))
          (testing "Whole Graph"
            (is (= (set ["zero" "one" "two" "three" "four" "five" "six"])
                   (set (tax/subgroups graph "zero")))))
          (testing "Leaf Node"
            (is (= (set ["five"])
                   (set (tax/subgroups graph "five"))))))))))
