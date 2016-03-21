(ns seriously.all
  (:use clojure.test))

(deftest all
  (let [namespaces '(
                      "seriously.core")]
    (do
      (apply require (map symbol namespaces))
      (is (successful? (time (apply run-tests (map symbol namespaces))))))))