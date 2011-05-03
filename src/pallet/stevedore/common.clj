(ns pallet.stevedore.common
  (:require 
    [pallet.stevedore :as stevedore]
    [clojure.string :as string]
    [clojure.contrib.logging :as logging]
    [clojure.walk :as walk])
  (:use
    [pallet.stevedore 
     :only [emit-script *stevedore-impl*]]))


(declare emit)

;;;;; Stevedore DSL parsing functions
;;;;;
;;;;; The "common" implementation performs an initial pass over the script
;;;;; forms, readying it for dispatch with `emit` and its counterparts.
;;;;;
;;;;; It evaluates any unquoted and unquoted-splicing forms.
;;;;;
;;;;; They are general enough that most implementations can reuse them.

;;; Splicing functions

(def 
  ^{:doc "The empty splice"}
  empty-splice
  ::empty-splice)

(defn- splice-list
  "Emit a collection as a space separated list.
       (splice-list [a b c]) => \"a b c\""
  [coll]
  (if (seq coll)
    (string/join " " coll)
    ;; to maintain unquote splicing semantics, this term has to disappear
    ;; from the result
    empty-splice))

(defn- filter-empty-splice
  [args]
  (filter #(not= empty-splice %) args))


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


;; Common functions/predicates

(defn emit-do [exprs]
  (string/join (map (comp statement emit) (filter-empty-splice exprs))))

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
    empty-splice))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; DSL parsing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Keyword and Operator Classes

(def
  ^{:doc
    "Special forms are handled explicitly by an implementation of
     `emit-special`."
    :private true}
  special-forms
  #{'aget 'alias 'and 'apply 'aset 'assoc! 'case 'chain-and 'chain-or 'defn 
    'defvar 'deref 'directory? 'do 'doseq 'empty? 'file-exists? 'get 'group 
    'if 'if-not 'let 'literally 'local 'merge! 'not 'or 'pipe 'print 'println 
    'quoted 'readable? 'return 'set! 'str 'symlink? 'var 'when 'while 'writeable? 
    '== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '| '&& '|| 
    })


;;; Predicates for keyword/operator classes

(defn special-form?
  "Predicate to check if expr is a special form"
  [expr]
  (contains? special-forms expr))

(defn compound-form?
  "Predicate to check if expr is a compound form"
  [expr]
  (= 'do  (first expr)))


;; Main dispatch functions.
;;
;; `emit` is the entry point for parsing after `emit-script`.
;; It dispatches on the type of its argument. 
;;
;; For example (emit (ls "asdf")) dispatches on `clojure.lang.IPersistentList`.
;;
;; The above example, along with some others, call `emit-special`,
;; `emit-function` or `emit-function-call`.
;;
;; For example:
;;
;;  (emit (ls "asdf"))
;; calls
;;  (emit-special 'ls (ls "asdf"))
;; ,
;;  (emit (defn foo [a] 
;;          "Docstring" 
;;          (println "asdf")))
;; calls
;;  (emit-function foo "Docstring" [a] (println "asdf"))
;; ,
;;  (emit (test-fn 1 2 "a"))
;; calls
;;  (emit-function-call test-fn [1 2 "a"])
;;
;; Generally, the implementations of `emit` in pallet.stevedore.common which dispatch on compound types
;; should be sufficient for most implementations. 
;;
;; The other emit-* functions are convenience functions 
;; which avoid reimplementing the common `emit` implementation for each Stevedore implementation.


(defmulti emit
  "Emit a shell expression as a string. Dispatched on the :type of the
   expression."
  (fn [ expr ] [*stevedore-impl* (type expr)]))

(defmulti emit-special
  "Emit a shell form as a string. Dispatched on the first element of the form."
  (fn [ & args] [*stevedore-impl* (identity (first args))]))

(defmulti emit-infix
  (fn [type [operator & args]] *stevedore-impl*))

(defmulti emit-function
  "Emit a shell function"
  (fn [name doc? sig body] *stevedore-impl*))

(defmulti emit-function-call
  "Emit a shell function call"
  (fn [name & args] *stevedore-impl*))

(defmulti infix-operator?
  "Predicate to check if expr is an infix operator. Each implementation
  should implement its own multimethod."
  (fn [expr] *stevedore-impl*))


;; This is the main entry point for implementations.

(defmethod emit-script ::common [forms]
  (let [code (if (> (count forms) 1)
               (emit-do (filter-empty-splice forms))
               (let [form (first forms)]
                 (if (= form empty-splice)
                   ""
                   (emit form))))]
    code))


;; Script functions (ie. `defscript`/`defimpl`) are handled with the
;; 'invoke form. During the first stage of parsing, script functions
;; are converted from their unquoted (escaped) form into a map.
;;
;; Pseudo Code:
;;
;; (~my-script-fn "test")
;; => ('invoke {:name example.ns/my-script-fn :line 10} "test")
;;
;; This form is common over all implementations and shouldn't be overridden.

(defmethod emit-special [::common-impl 'invoke]
  [type [name & args]]
  (logging/trace (str "INVOKE " name " " args))
  (if (map? name)
    (try
      (stevedore/*script-fn-dispatch*
       name (filter-empty-splice args) stevedore/*script-ns* stevedore/*script-file* stevedore/*script-line*)
      (catch java.lang.IllegalArgumentException e
        (throw (java.lang.IllegalArgumentException.
                (str "Invalid arguments for " name) e))))
    (let [argseq (->>
                    args
                    filter-empty-splice
                    (map emit)
                    (filter (complement string/blank?))
                    (interpose " "))]
      (apply emit-function-call name argseq))))

(defn- emit-s-expr [expr]
  (if (symbol? (first expr))
    (let [head (symbol (name (first expr))) ; remove any ns resolution
          expr1 (conj (rest expr) head)]
      (cond
       (and (= (first (str head)) \.)
            (> (count (str head)) 1)) (emit-special 'dot-method expr1)
       (special-form? head) (emit-special head expr1)
       (infix-operator? head) (emit-infix head expr1)
       :else (emit-special 'invoke expr)))
    (if (map? (first expr))
      (emit-special 'invoke expr)
      (when (seq expr)
        (string/join " " (filter (complement string/blank?) (map emit expr)))))))

(defn- spread
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (apply list (first arglist) (spread (next arglist)))))

(defmethod emit [::common-impl clojure.lang.IPersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.Cons]
  [expr]
  (if (= 'list (first expr))
    (emit-s-expr (rest expr))
    (emit-s-expr expr)))

(defmethod emit-special [::common-impl 'apply] [type [apply & exprs]]
  (emit-s-expr (spread exprs)))


;; This method is the entry point of `emit-function`. Generally
;; implementations should implement `emit-function` instead of 
;; overriding this.

(defmethod emit-special [::common-impl 'defn] [type [fn & expr]]
  (let [name (first expr)]
    (if (string? (second expr))
      (let [doc (second expr)
            signature (second (next expr))
            body (rest (rest (rest expr)))]
        (emit-function name doc signature body))
      (let [signature (second expr)
            body (rest (rest expr))]
        (emit-function name nil signature body)))))
