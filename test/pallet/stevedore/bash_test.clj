(ns pallet.stevedore.bash-test
  (:use
   [pallet.common.string :only [quoted]]
   pallet.stevedore 
   pallet.stevedore.common
   clojure.test)
  (:require
   [clojure.contrib.seq :as c.seq]
   [pallet.script :as script]
   [pallet.stevedore.bash]
   [pallet.common.filesystem :as filesystem]
   [pallet.common.logging.log4j :as log4j]
   [pallet.common.shell :as shell]
   [clojure.contrib.logging :as logging]))

(defmacro bash-out
  "Check output of bash. Implemented as a macro so that errors appear on the
   correct line."
  ([str] `(bash-out ~str 0 ""))
  ([str exit err-msg]
     `(let [r# (shell/bash ~str)]
        (when-not (= ~exit (:exit r#))
          (logging/error
           (format
            "Unexpected exit status:\n:cmd %s\n:out %s\n:err %s"
            ~str (:out r#) (:err r#))))
        (is (= ~err-msg (:err r#)))
        (is (= ~exit (:exit r#)))
        (:out r#))))

(defn strip-ws
  "strip extraneous whitespace so tests don't fail because of differences in
   whitespace" [s]
  (-> s
    (.replaceAll "[ ]+" " ")
    .trim))

(defn strip-line-ws
  "strip extraneous whitespace so tests don't fail because of differences in
   whitespace"
  [#^String s]
  (-> s
      (.replace "\n" " ")
      (.replaceAll "[ ]+" " ")
      .trim))

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
    @#'pallet.stevedore.common/special-forms))


(deftest number-literal
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "42" (script 42)))
    (is (= "0.5" (script 1/2)))))

(deftest simple-call-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "a b" (script (a b))))))

(deftest call-multi-arg-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "a b c" (script (a b c))))))

(deftest test-arithmetic
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "(x * y)" (script (* x y))))))

(deftest test-return
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "return 42" (strip-ws (script (return 42)))))))

(deftest test-script-call
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (let [name "name1"]
      (is (= "grep \"^name1\" /etc/passwd"
             (script (grep ~(str "\"^" name "\"") "/etc/passwd")))))))


(deftest test-clj
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (let [foo 42
          bar [1 2 3]]
      (is (= "42" (script (clj foo))))
      (is (= "42" (script ~foo)))
      (is (= "foo 1 2 3" (script (apply foo ~bar)))))))

(deftest test-str
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "foobar"
           (script (str foo bar))))))

(deftest test-fn
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (thrown? java.lang.AssertionError
                 (strip-ws (script (defn [x y]
                                     (foo a) (bar b)))))
        "anonymous")

    (is (= "foo() {\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}"
           (strip-ws (script (defn foo [x y] 
                               (foo a) (bar b)))))
        "without flags")

    (is (= "foo() {\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nfoo a\nbar b\n}"
           (strip-ws (script (defn foo [[:string "host" "h" "Doc" "default"]] 
                               (foo a) (bar b)))))
        "with flags only")

    (is (= "foo() {\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}"
           (strip-ws (script (defn foo [x y 
                                        [:string "host" "h" "Doc" "default"]] 
                               (foo a) (bar b)))))
        "with flags and arguments")

    (is (= "foo() {\nFLAGS_HELP=\"This is doc\"\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}"
           (strip-ws (script (defn foo 
                               "This is doc" 
                               [x y 
                                [:string "host" "h" "Doc" "default"]] 
                               (foo a) (bar b)))))
        "with docstring and arguments")))


(deftest test-aget
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "${foo[2]}" (script (aget foo 2))))))


(deftest test-aset
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "foo[2]=1" (script (aset foo 2 1))))))

(deftest test-set!
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "foo=1" (script (set! foo 1))))
    (is (thrown? clojure.contrib.condition.Condition
                 (script (set! foo-bar 1))))))

(deftest var-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "foo=1" (script (var foo 1))))
    (is (thrown? clojure.contrib.condition.Condition
                 (script (var foo-bar 1))))))

(deftest alias-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "alias foo='ls -l'" (script (alias foo (ls -l)))))))

(deftest test-array
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "(1 2 \"3\" foo)" (script [1 "2" "\"3\"" :foo])))))

(deftest test-if
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "if [ \\( \"foo\" == \"bar\" \\) ]; then echo fred;fi"
           (script (if (= foo bar) (println fred)))))
    (is (= "if [ \\( \\( \"foo\" == \"bar\" \\) -a \\( \"foo\" != \"baz\" \\) \\) ]; then echo fred;fi"
           (script (if (&& (== foo bar) (!= foo baz)) (println fred)))))
    (is (= "fred\n"
           (bash-out (script (if (&& (== foo foo) (!= foo baz)) (println "fred"))))))
    (is (= "if foo; then\nx=3\nfoo x\nelse\ny=4\nbar y\nfi"
           (script (if foo (do (var x 3) (foo x)) (do (var y 4) (bar y))))))
    (is (= "not foo\n"
           (bash-out (script (if (== foo bar)
                               (do (println "foo"))
                               (do (println "not foo")))))))
    (is (= "if [ -e file1 ]; then echo foo;fi"
           (script (if (file-exists? "file1") (println "foo")))))
    (is (= "if [ ! -e file1 ]; then echo foo;fi"
           (script (if (not (file-exists? "file1")) (println "foo")))))
    (is (= "if [ \\( ! -e file1 -o \\( \"a\" == \"b\" \\) \\) ]; then echo foo;fi"
           (script (if (|| (not (file-exists? "file1")) (== "a" "b"))
                     (println "foo")))))
    (testing "if block as string with newline is treated as compound"
             (is (= "if [ -e f ]; then\nls\nls\nfi"
                    (script (if (file-exists? "f") "ls\nls")))))))

(deftest if-nested-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "if [ \\( \"foo\" == \"bar\" \\) ]; then\nif [ \\( \"foo\" != \"baz\" \\) ]; then echo fred;fi\nfi"
           (script (if (== foo bar)
                     (if (!= foo baz)
                       (println fred))))))
    (is (= "" (bash-out (script (if (== foo bar)
                                  (if (!= foo baz)
                                    (println fred)))))))))

(deftest test-if-not
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "if [ ! -e bar ]; then echo fred;fi"
           (script (if-not (file-exists? bar) (println fred)))))
    (is (= "if [ ! \\( -e bar -a \\( \"foo\" == \"bar\" \\) \\) ]; then echo fred;fi"
           (script (if-not (&& (file-exists? bar) (== foo bar)) (println fred)))))
    (is (= "if [ ! \\( \\( \"foo\" == \"bar\" \\) -a \\( \"foo\" == \"baz\" \\) \\) ]; then echo fred;fi"
           (script (if-not (&& (== foo bar) (== foo baz)) (println fred)))))
    (is (= "fred\n"
           (bash-out (script (if-not (&& (== foo foo) (== foo baz))
                               (println "fred"))))))))

(deftest test-when
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "if [ \\( \"foo\" == \"bar\" \\) ]; then\necho fred\nfi"
           (script (when (= foo bar) (println fred)))))
    (is (= "if foo; then\nx=3\nfoo x\nfi"
           (script (when foo (var x 3) (foo x)))))))

(deftest test-case
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "case ${X} in\n1)\nsomething;;\n\"2\")\nsomething else;;\nesac"
           (script (case @X
                         1 (something)
                         ~(quoted "2") (something else)))))))

(deftest test-doseq
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "for X in 1 2 3; do\nsomething ${X}\ndone"
           (script (doseq [X [1 2 3]] (something @X)))))))


(deftest test-map
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "([packages]=(columnchart))"
           (strip-ws (script {:packages ["columnchart"]}))))
    (is (= "{ hash_set x p c; hash_set x q d; }\necho ${x[p]}"
           (strip-ws (script (do (var x {:p "c" :q "d"})
                               (println (aget x :p)))))))
    (is (= "c\nd\n"
           (bash-out (script
                       ~pallet.stevedore.bash/hashlib
                       (var x {:p "c" "/a/b/c-e" "d"})
                       (println (get x :p))
                       (println (get x "/a/b/c-e"))))))
    (testing "assoc!"
      (is (= "c\n1\n2\n"
             (bash-out (script
                         ~pallet.stevedore.bash/hashlib
                         (var x {:p "c" :q "q"})
                         (assoc! x :q 1)
                         (assoc! x :r 2)
                         (println (get x :p))
                         (println (get x :q))
                         (println (get x :r)))))))
    (testing "merge!"
      (is (= "c\n1\n2\n"
             (bash-out (script
                         ~pallet.stevedore.bash/hashlib
                         (var x {:p "c" :q "q"})
                         (merge! x {:q 1 :r 2})
                         (println (get x :p))
                         (println (get x :q))
                         (println (get x :r)))))))))


(deftest test-do
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "let x=3\nlet y=4\nlet z=(x + y)"
           (strip-ws
             (script
               (let x 3)
               (let y 4)
               (let z (+ x y))))))
    (is (= "7\n"
           (bash-out
             (script
               (let x 3)
               (let y 4)
               (let z (+ x y))
               (println @z)))))))

(deftest deref-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "${TMPDIR-/tmp}" (script @TMPDIR-/tmp)))
    (is (= "$(ls)" (script @(ls))))))

(deftest test-combine-forms
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (let [stuff (quote (do
                         (local x 3)
                         (local y 4)))]
      (is (= "foo() {\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nx=$1\nlocal x=3\nlocal y=4\n}"
             (strip-ws (script (defn foo [x] ~stuff))))))))

(deftest defvar-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "x=1"
           (script (defvar x 1))))))

(deftest println-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "echo hello"
           (script (println "hello"))))
    (is (= "echo hello there"
           (script (println "hello there"))))))

(deftest do-script-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "fred\n" (do-script "fred")))
    (is (= "fred\nblogs\n" (do-script "fred" "blogs")))
    (is (= "fred\nblogs\n" (do-script "fred\n\n" "blogs\n")))
    (is (= "fred\nblogs\n" (do-script "fred\n\n" nil "blogs\n")))))

(deftest chain-commands*-test
  (with-stevedore-impl :pallet.stevedore.bash/bash
    (is (= "fred" (chain-commands* ["fred"])))
    (is (= "fred && blogs" (chain-commands* ["fred" "blogs"])))
    (is (= "fred && blogs" (chain-commands* ["fred\n\n" "blogs\n"])))
    (is (= "fred && blogs" (chain-commands* ["fred\n\n" nil "blogs\n"])))))

(deftest chain-commands-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "fred" (chain-commands "fred")))
    (is (= "fred && blogs" (chain-commands "fred" "blogs")))
    (is (= "fred && blogs" (chain-commands "fred\n\n" "blogs\n")))
    (is (= "fred && blogs" (chain-commands "fred\n\n" nil "blogs\n")))))

(deftest chain-script-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "fred" (chained-script (fred))))
    (is (= "fred && blogs" (chained-script (fred) (blogs))))))

(deftest checked-commands-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "echo \"test...\"\n{ echo fred && echo tom; } || { echo \"test\" failed; exit 1; } >&2 \necho \"...done\"\n"
           (checked-commands "test" "echo fred" "echo tom")))
    (is (= "test...\ntom\n...done\n"
           (bash-out (checked-commands "test" "echo tom"))))
    (is (= "test...\nfred\ntom\n...done\n"
           (bash-out (checked-commands "test" "echo fred" "echo tom"))))
    (is (= "test...\n"
           (bash-out
             (checked-commands "test" "test 1 = 2") 1 "test failed\n")))))

(deftest checked-script-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= (checked-commands "msg" (script ls) (script ls))
           (checked-script "msg" (ls) (ls))))
    (is (= "echo \"test...\"\n{ echo fred && echo tom; } || { echo \"test\" failed; exit 1; } >&2 \necho \"...done\"\n"
           (checked-script "test" (println fred) (println tom))))
    (is (= "test...\ntom\n...done\n"
           (bash-out (checked-script "test" (println tom)))))
    (is (= "test...\nfred\ntom\n...done\n"
           (bash-out (checked-script "test" (println fred) (println tom)))))
    (is (= "test...\n"
           (bash-out (checked-script "test" ("test" 1 = 2)) 1 "test failed\n")))))

(deftest group-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "{ ls }"
           (script (group (ls)))))
    (is (= "{ ls; ls; }"
           (script (group (ls) (ls)))))))

(deftest pipe-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "ls"
           (script (pipe (ls)))))
    (is (= "ls | ls"
           (script (pipe (ls) (ls)))))))

(deftest empty?-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "if [ -z ${a} ]; then echo true;fi"
           (script (if (empty? @a) (println true)))))))

(deftest unquote-splicing-test
  (with-stevedore-impl :pallet.stevedore.bash/bash

    (is (= "a b c" (script ~@["a" "b" "c"])))
    (is (= "x" (script x ~@[])))
    (is (= "x" (script (x ~@[]))))
    (let [x ["a" "b" "c"]]
      (is (= "a b c" (script ~@x))))
    (let [x []]
      (is (= "x" (script x ~@x))))
    (let [x nil]
      (is (= "" (script ~@x))))
    (let [x []]
      (is (= "" (script ~@x))))
    (let [fx (fn [] ["a" "b" "c"])]
      (is (= "a b c" (script ~@(fx)))))
    (let [xfn (script/script-fn [& args])]
      (script/defimpl xfn :default [& args]
                      ("xfn" ~args))
      (let [x nil]
        (is (= "xfn" (script (xfn ~@x)))))
      (let [x [:a 1]]
        (is (= "xfn a 1" (script (xfn ~@x))))))))

(log4j/with-appender-threshold [:error]
  (script/defscript x [a])
  (defimpl x :default [a] a))
