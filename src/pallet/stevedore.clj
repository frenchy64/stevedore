(ns pallet.stevedore
  "Embed shell script in clojure.

   Shell script is embedded by wrapping in the `script` macro.
       (script (ls)) => \"ls\"

   The result of a `script` form is a string."
  (:require
   [pallet.common.deprecate :as deprecate]
   [clojure.contrib.def :as def]
   [clojure.contrib.seq :as c.seq]
   [clojure.string :as string]
   [clojure.walk :as walk])
  (:use
   [pallet.common.string :only [underscore]]))


;;; Helper vars for parsing the stevedore DSL

(def/defunbound *stevedore-impl*
  "Current stevedore implementation")

(def/defunbound *script-ns*
  "Used to capture the namespace in which `script` is invoked.")

(def/defunbound *script-line*
  "Used to capture a form's line number.")

(def/defunbound *script-file*
  "Used to capture a form's file name.")

(defmacro with-line-number
  "Provide the source file and line number for use in reporting."
  [[file line] & body]
  `(do
     (binding [*script-line* ~line
               *script-file* ~file]
       ~@body)))


;;; moved back

;;; Splicing functions
(declare emit)

(defn splice-list
  "Emit a collection as a space separated list.
       (splice-list [a b c]) => \"a b c\""
  [coll]
  (if (seq coll)
    (string/join " " coll)
    ;; to maintain unquote splicing semantics, this term has to disappear
    ;; from the result
    ::empty-splice))

(defn filter-empty-splice
  [args]
  (filter #(not= ::empty-splice %) args))


;;; High level string generation functions
(def statement-separator "\n")

(defn statement
  "Emit an expression as a valid shell statement, with separator."
  [expr]
  ;; check the substring count, as it can be negative if there is a syntax issue
  ;; in a stevedore expression, and generates a cryptic error message otherwise
  (let [n (- (count expr) (count statement-separator))]
    (if (and (pos? n) (not (= statement-separator (.substring expr n))))
      (str expr statement-separator)
      expr)))


(defn emit-do [exprs]
  (string/join (map (comp statement emit) (filter-empty-splice exprs))))

(defn script* [forms]
  (let [code (if (> (count forms) 1)
               (emit-do (filter-empty-splice forms))
               (let [form (first forms)]
                 (if (= form ::empty-splice)
                   ""
                   (emit form))))]
    code))

(defn- unquote?
  "Tests whether the form is (clj ...) or (unquote ...) or ~expr."
  [form]
  (or (and (seq? form)
           (symbol? (first form))
           (= (symbol (name (first form))) 'clj))
      (and (seq? form) (= (first form) `unquote))))

(defn- unquote-splicing?
  "Tests whether the form is ~@( ...) or (unqote-splicing ...)."
  [form]
  (and (seq? form) (= (first form) `unquote-splicing)))

(defn- handle-unquote [form]
  (second form))

(defn- splice [form]
  (if (seq form)
    (string/join " " (map emit form))
    ::empty-splice))

(defn- handle-unquote-splicing [form]
  (list splice (second form)))

(declare inner-walk outer-walk)

(defn- inner-walk [form]
  (cond
   (unquote? form) (handle-unquote form)
   (unquote-splicing? form) (handle-unquote-splicing form)
   :else (walk/walk inner-walk outer-walk form)))

(defn- outer-walk [form]
  (cond
   (symbol? form) (list 'quote form)
   (seq? form) (list* 'list form)
   :else form))

(defmacro quasiquote
  [form]
  (let [post-form (walk/walk inner-walk outer-walk form)]
    post-form))

(defmacro script
  "Takes one or more forms. Returns a string of the forms translated into
   shell script.
       (script
         (println \"hello\")
         (ls -l \"*.sh\"))"
  [& forms]
  `(with-line-number [~*file* ~(:line (meta &form))]
     (binding [*script-ns* ~*ns*]
       (script* (quasiquote ~forms)))))

;; `emit-script` is the main entry point for stevedore implementations.
;; It should not be called by users. It is only public so it can be implemented
;; by different implementations.
;;
;; The main requirement of implementations is this function being implemented.
;;
;; It is called via the public `script` function.

;;(defmulti emit-script
;;  "Emit a script given forms. Some forms have
;;  (= (first form) pallet.stevedore/empty-splice)
;;  which is almost always useless and should be filtered out."
;;  (fn [ forms ] *stevedore-impl*))



;; `script` is the public interface to stevedore. All scripts must be
;; wrapped in a `script` form.
;;
;; (script
;;   (println "asdf"))

;;(defmacro script
;;  "Takes one or more forms. Returns a string of the forms translated into
;;   shell script.
;;       (script
;;         (println \"hello\")
;;         (ls -l \"*.sh\"))"
;;  [& forms]
;;  `(with-line-number [~*file* ~(:line (meta &form))]
;;     (binding [*script-ns* ~*ns*]
;;       (emit-script ~forms))))


;; Whenever `script` is called, it must be wrapped in a `with-stevedore-impl`,
;; which instructs which implementation to utilize.
;;
;; (with-stevedore-impl :pallet.stevedore.bash/bash
;;   (script
;;     (println "asdf")))

(defmacro with-stevedore-impl
  "Set which stevedore implementation to use. Currently supports:
   :pallet.stevedore.bash/bash"
  [impl & body]
  `(do
     (binding [*stevedore-impl* ~impl]
       ~@body)))


;;; Script combiners
;;;
;;; Each script argument to these functions must be wrapped in
;;; an explicit `script`.
;;;
;;; Eg. (do-script (script ls) (script ls))
;;;  => (script
;;;       ls
;;;       ls)

(defmulti do-script 
  "Concatenate multiple scripts."
  (fn [& scripts] *stevedore-impl*))

(defmulti chain-commands
  "Chain commands together so if one command exits with error,
  then the remaining commands will not execute."
  (fn [& scripts] *stevedore-impl*))

(defmulti checked-commands
  "Wrap a command in a code that checks the return value. Code to output the
  messages is added before the command."
  (fn [message & cmds] *stevedore-impl*))

;;; These macros have an implicit `script` around each script argument.
;;;
;;; Eg. (chained-script ls ls)
;;;     => (script
;;;           ls
;;;           ls)

(defmacro chained-script
  "Takes one or more forms. Returns a string of the forms translated into a
   chained shell script command."
  [& forms]
  `(chain-commands
    ~@(map (fn [f] (list `script f)) forms)))

(defmacro checked-script
  "Takes one or more forms. Returns a string of the forms translated into
   shell scrip.  Wraps the expression in a test for the result status."
  [message & forms]
  `(checked-commands ~message
    ~@(map (fn [f] (list `script f)) forms)))


;;; Script argument helpers
;;; TODO eliminate the need for this to be public by supporting literal maps for expansion

(defn- arg-string
  [option argument do-underscore do-assign dash]
  (let [opt (if do-underscore (underscore (name option)) (name option))]
    (if argument
      (if (> (.length opt) 1)
        (str dash opt (if-not (= argument true)
                        (str (if do-assign "=" " ") \" argument \")))
        (str "-" opt (if-not (= argument true) (str " " \" argument \")))))))

(defn map-to-arg-string
  "Output a set of command line switches from a map"
  [m & {:keys [underscore assign dash] :or {dash "--"}}]
  {:pre [(or (nil? m) (map? m))]}
  (apply
   str (interpose
        " "
        (map
          #(arg-string (key %) (val %) underscore assign dash)
          (filter val m)))))

(defn option-args
  "Output a set of command line switches from a sequence of options"
  [{:as m}]
  (let [assign (:assign m)
        underscore (:underscore m)]
    (map-to-arg-string
     (dissoc m :assign :underscore) :assign assign :underscore underscore)))


;; Dispatch function for script functions

(defn script-fn-dispatch-none
  "Script function dispatch. This implementation does nothing."
  [name args ns file line]
  nil)

(def ^{:doc "Script function dispatch."}
  *script-fn-dispatch* script-fn-dispatch-none)

(defn script-fn-dispatch!
  "Set the script-fn dispatch function"
  [f]
  (alter-var-root #'*script-fn-dispatch* (fn [_] f)))

(defmacro with-no-script-fn-dispatch
  [& body]
  `(binding [*script-fn-dispatch* script-fn-dispatch-none]
     ~@body))

(defmacro with-script-fn-dispatch
  [f & body]
  `(binding [*script-fn-dispatch* ~f]
     ~@body))





;; DEPRECATED

(defmacro defimpl
  {:deprecated "0.5.0"}
  [script specialisers [& args] & body]
  (require 'pallet.script)
  `(do
     (deprecate/deprecated-macro
      ~&form
      (deprecate/rename 'pallet.stevedore/defimpl 'pallet.script/defimpl))
     (pallet.script/defimpl ~script ~specialisers [~@args] ~@body)))
