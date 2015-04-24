;; -- general
;; 80 character warning sign ---------------------------------------------- hereOVER LIMIT
;; Comment
;; TODO: to do
;; FIXME: fix me
;; BUG: bug

((parens-are-dimmed-a-little))

"A string is a sequence of characters"
#"regexp"

[]
{}
#{}
[] {} #{}
[data] {data} #{data}

true false nil    ;; specials
:keyword1 :keyword2

% %1 %2           ;; lambda params
@deref
#'var

;; -- macro-writing
~data
'data
`data
'~data
~'data
`~data
data#
(eval data)

;; -- definitions
(let let-form)
(def var-name data)
(def+ var-name data)
(defn func-name [arg1 arg2 & argn])
(defmacro macro-name [arg1 arg2 & argn])
(fn [arg1 arg2 & argn])
(fn+ [arg1 arg2 argn])
(defn+ func-name [arg1 arg2 argn])
(def-decorator :name [arg1 arg2 argn])

;; -- java
JavaClass
(.methodCall "param")
(JavaClass/static-method)

;; -- debugging
TODO:

;; -- prettification
nil
(into [] empty-vector)
(into {} empty-map)
(into #{} empty-set)

(def var-name data)
(def+ var-name data)
(defn func-name [two-unnamed-vars _ _ & argn])
(defmacro macro-name [arg1 arg2 & argn])
(fn [arg1 arg2 & argn])
(fn+ [arg1 arg2 argn])
(defn+ func-name [arg1 arg2 argn])
(def-decorator :name [arg1 arg2 argn])
#(shortcut-lambda %1 %2)

(>= 2 1)
(<= 1 2)
(= 4 5)

(-> f (inc))
(->> f (dec))

(map f t)
(mapv x f)
(vec f)
(vector x)

(| func1 func2)       ;; composition
(& juxt1 juxt2)       ;; juxtaposition
(=> func param)        ;; currying

(apply sum 1 2 3)       ;; sum
(count f)             ;; count

;; should not be prettified
(partial 1 2)
(comp 1 2)

;; -- indentation
(defn f [x y p]
  (command))

(fn f [param1 :a
      param2 :b]
  (command))

#(lambda-shortcut %1 %2)
(fn[x y] (can-it-be-confused-with-fn?))

(when-not-let-empty [x f]
  (command))

(when-not-let-empty [x f
                     y g]
  (command))

(function-call
 long-argument-list1 long-argument-list1 long-argument-list1)

(let [variable-name
        (very-log-value)]
  {:key-1
     (a-very-long-function-call arg1 arg2)
   :key-1
     (a-very-long-function-call arg1 arg2)})

;; namespaces here!
(defmacro yop
  `(ns (:require [clojure.string :as s])
     (:gen-class)))
