(ns pallet.stevedore.batch-test
  (:use
   [pallet.common.string :only [quoted]]
   pallet.stevedore
   pallet.stevedore.batch
   midje.sweet
   clojure.test))

;;; TODO separate into another file, this is repeated in bash_test.clj
;;; Implementation coverage tests
(defn emit-special-coverage [impl]
  "Returns a vector of two elements. First elements is a vector
  of successfully dispatched special functions. Second element is a vector
  of failed dispatches."
  (c.seq/separate
    (fn [s]
      (try
        (with-stevedore-impl impl
          (emit-special s)
        true
        (catch Exception e
          (not (.contains
            (str e)
            (str "java.lang.IllegalArgumentException: No method in multimethod 'emit-special' for dispatch value: [" impl " " s "]")))))))
    special-forms))

(deftest implementation-coverage-test
  (future-fact "complete `emit-special` coverage"
    (let [unimplemented (second (emit-special-coverage :pallet.stevedore.batch/batch))]
      unimplemented => empty?)))

(deftest number-literal
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script 42) => "42")
    (future-fact "support floating point numbers"
      ;http://www.dostips.com/DtCodeFunctions.php
      (script 1/2) => "0.5"
      (script 0.5) => "0.5")))

(deftest test-string
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script "42") => "42"
      (script "1/2") => "1/2")))

(deftest simple-call-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script (a b c)) => "call:a b c"
      (script (a b)) => "call:a b"
      (script (a)) => "call:a")))

(deftest test-arithmetic
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script (* @x @y)) => "(%x% * %y%)"
      (script (* 1 2)) => "(1 * 2)")))

(deftest test-return
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (future-fact "handle return values from functions"
      ;; http://www.dostips.com/DtTutoFunctions.php
      (script (return 42)) => "return 42")))

(deftest test-set!
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (future-fact "handle arithmatic in set!"
      (script (set! foo (+ 1 1))) => "set /a foo=(1+1)"
      (script (set! foo 1)) => "set /a foo=1")
    (fact "assign simple strings"
      (script (set! foo "1")) => "set foo=1"
      (script (set! foo "1 + 1")) => "set foo=1 + 1"
      (script (set! foo-bar "1")) => (throws clojure.contrib.condition.Condition))))

(deftest test-str
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact (script (str foo bar)) => "foobar")))

(deftest println-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script (println "hello")) => "echo hello"
      (script (println "hello there")) => "echo hello there")))

(deftest deref-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script @TMPDIR) => "%TMPDIR%")
    (future-fact "support default value for defrefencing"
      (script @TMPDIR-/tmp) => "%TMPDIR%-/tmp")
    (future-fact "support equivilant of `ls`"
      (script @(ls)) "$(ls)")))

(deftest group-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script (group (ls))) => "(\ncall:ls\n)"
      (script (group (ls) (ls))) => "(\ncall:ls\ncall:ls\n)")))

(deftest test-fn
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact "defn fails on anonymous function"
      (script (defn [x y] (foo a) (bar b))) => (throws java.lang.AssertionError))
    (fact "defn without flags"
      (script (defn test1 [x y] (println "asdf"))) =>  ":test1\necho asdf\nGOTO :EOF")
    (future-fact "defn with flags"
      (script
        (defn test1 [[:string "host" "h" "Doc" "default"]]
          (println "asdf")))
        =>  "TODO")
    (future-fact "defn with docstring"
      (script
        (defn foo
          "This is doc"
          [[:string "host" "h" "Doc" "default"]]
          (foo a)))
      => "todo")))

(deftest test-if
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script
        (if (= "foo" "bar")
          (println "fred")))
      => "if foo == bar (\necho fred\n)")))
