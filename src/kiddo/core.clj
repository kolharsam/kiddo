(ns kiddo.core
  (:require [clojure.main :as main]
            [clojure.string :as str]))

;; things to be done

;; being able to run a REPL
;; break the code into tokens
;; eval the code
;; display output

;; This is just a toy language on a REPL to help
;; me just get an idea of could be using this
;; there are plenty more options available on
;; the main/repl docs

(def repl-options
  [:prompt #(printf "~> : ")
   :read   (fn [request-prompt request-exit]
             (or ({:line-start request-prompt :stream-end request-exit}
                  (main/skip-whitespace *in*))
                 (re-find #"^(\d+)([\+\-\*\/])(\d+)$" (read-line))))
   :eval   (fn [[_ x op y]]
             (({"+" + "-" - "*" * "/" /} op)
              (Integer. x)
              (Integer. y)))])

(defn REPL
  [repl-options]
  (apply main/repl repl-options))

;; (REPL repl-options)

(defn is-lowercase?
  "Returns true if word is in lowercase"
  [word]
  (let [low-case-word (str/lower-case word)]
    (= word low-case-word)))

(def reserved-words ["is" "a" "list" "add" "take" "write" "ask" "to" "from" "true" "false"])

(defn- is-present-in-reserved?
  [word]
  (some #(= % word) reserved-words))

(defn is-valid-token?
  "Abstraction for methods that are using Regexp"
  [re word]
  (not= (re-find re word) nil))

(defn- is-reserved?
  "Returns true if word is present in the `reserved-words` list"
  [word]
  (is-present-in-reserved? word))

(defn- is-number?
  "Returns true if the word is a number"
  [word]
  (is-valid-token? #"[\d+]+$" word))

(defn- is-string?
  "Returns true if it is a string"
  [word]
  (is-valid-token? #"\"[A-Za-z\s,+*_-]*\"$" word))

(defn- is-symbol?
  "Returns true if word is a valid variable name"
  [word]
  (is-valid-token? #"[A-Za-z]+$" word))

;; FIXME: Check the regex of the symbol - it is incorrect for certain inputs like
;; asdasd12asda

(defn- get-number-value
  [word]
  (Integer/parseInt word 10))

(defn- get-string-value
  [word]
  (last (str/split word #"\"")))

(defn tokenize
  [input]
  (let [trim-str (str/trim input)
        cut-whitespaces (str/split trim-str #" ")
        check-if-lowercase (every? is-lowercase? cut-whitespaces)]
    (if-not check-if-lowercase
      (throw (Exception. "Ooh! Something's fishy!"))
      ;; Now's where the fun begins - tokens are set into a flat list
      (mapv (fn [val]
              (cond
                (is-reserved? val) {:type "reserved" :value val}
                (is-number? val) {:type "number" :value (Integer/parseInt val 10)}
                (is-string? val) {:type "string" :value val}
                (is-symbol? val) {:type "symbol" :value val}))
            cut-whitespaces))))

(tokenize "kiddo is \"language\"")
(tokenize "alter is 23")

(def var-store
  "The main store for storing variables and other values"
  (atom {}))

(defn add-to-store
  "Appends a new value to the var-store"
  [state name value]
  (swap! state assoc name value))

;; (add-to-store var-store "kiddo" "language")

(defn get-instruction
  "Returns the first token that is reserved"
  [tokens-coll]
  (let [instr (nth (filter #(= "reserved" (:type %)) tokens-coll) 0)
        operands (filter #(not= instr %) tokens-coll)]
    [instr operands]))

(declare exec-instruction)

(defn exec-is
  "Executes the `is` method"
  [operands]
  ;; Make a helper called get value common for all the instructions
  (let [key (:value (some #(when (= "symbol" (:type %)) %) operands))
        value (:value (some #(when (not= "symbol" (:type %)) %) operands))]
    (add-to-store var-store key value)))

(defn exec-instruction
  "Returns the value obtained from executing an instruction"
  [[instr-map operands]]
  (let [instr (:value instr-map)]
    ;; FIXME: Replace with a better way to choose btw instructions
    (when (= instr "is")
      (exec-is operands))))

;; Execution of the sample program from tokenization to
;; performing the action

(-> (tokenize "kio is 42")
    (get-instruction)
    (exec-instruction))

;; Maybe keep this store permanent
@var-store
