#!/usr/bin/env janet

(defn compile-var
  [self output args]

  (if (not= 2 (length args))
    (error (string/format "'var' must have two args, found %d for '%p'" (length args) args)))

  (if (not (nil? (get self :el-name)))
    (error (string/format "'var' expression not allowed in morphism bodies")))

  (def name (get args 0))
  (if (not= :symbol (type name))
    (error (string/format "'var' name arg must be a symbol, found a %p for '%p'" (type name) args)))

  (def val (get args 1))
  (case (type val)
    :number
    (array/push output (string/format "var %s %d" name val))

    :tuple
    (do
      (each el val
        (if (not= :number (type el))
          (error (string/format "'var' arrays must be a simple list of numbers, found '%p'" val))))

      (array/push output (string/format "var %s [%s]" name (string/join (map string val) " "))))

    (error (string/format
             "'var' value must be a number or tuple, found %p for '%p'" (type val) args))))

(defn compile-bin-op
  [self output args op-sym]

  (if (not= 2 (length args))
    (error (string/format "'%s' must have two args, found %d for '%p'" op-sym (length args) args)))

  (def arg0 (get args 0))
  (def arg1 (get args 1))

  (def el-name (get self :el-name))

  (cond
    (and (= arg0 el-name)
         (= arg1 el-name))
    (do
      (array/push output "dup")
      (array/push output "dup"))

    (= arg0 el-name)
    (do
      (array/push output "dup")
      (:compile self output arg1))

    (= arg1 el-name)
    (do
      (array/push output "dup")
      (:compile self output arg0)
      (array/push output "swap"))

    (do
      (:compile self output arg0)
      (:compile self output arg1)
      (array/push output "rot")
      (array/push output "rot")))

  (array/push output (case op-sym
                       '+ "add" '- "sub" '* "mul" '/ "div" '% "mod"
                       (error (string/format "todo bin-op %s" op-sym))))

  (array/push output "swap"))

(defn compile-eq
  [self output args]

  (if (not= 2 (length args))
    (error (string/format "'eq' must have 2 args, found %d for '%p'" (length args) args)))

  (if (not (nil? (get self :el-name)))
    (error (string/format "'eq' expression not allowed in morphism bodies")))

  (def ary-name (get args 0))
  (def expr (get args 1))

  (if (not= :symbol (type ary-name))
    (error (string/format "'eq' must take a var name as its first arg, found '%p'" ary-name)))

  (:compile self output expr)
  (array/push output (string/format "cmp %s" ary-name)))

(defn compile-assert
  [self output args]

  (if (not= 1 (length args))
    (error (string/format "'assert' must have 1 arg, found %d for '%p'" (length args) args)))

  (if (not (nil? (get self :el-name)))
    (error (string/format "'assert' expression not allowed in morphism bodies")))

  (:compile self output (get args 0))

  (array/push output "asrt"))

# (map array (fn [el] body))
(defn compile-map
  [self output args]

  (if (not= 2 (length args))
    (error (string/format "'map' must have two args (an array and a function), found %d for '%p'"
                          (length args) args)))

  (if (not (nil? (get self :el-name)))
    (error (string/format "'map' expression not allowed recursively in morphism bodies (yet)")))

  # Assuming that the array exists and is an array, as long as the arg is a symbol.
  (def ary_name (get args 0))
  (if (not= :symbol (type ary_name))
    (error (string/format "'map' expects array name for first arg, found '%p'" ary_name)))

  (let [lambda (get args 1)]
    (if (or (not= :tuple (type lambda))
            (not= 3 (length lambda))
            (not= 'fn (get-in args [1 0])))
      (error (string/format
               "'map' expects a lambda (fn args body) second arg, found '%p'" lambda))))

  (def lambda-args (get-in args [1 1]))
  (def lambda-body (get-in args [1 2]))

  (if (or (not= :tuple (type lambda-args))
          (not= :tuple (type lambda-body)))
    (error (string/format
             "'map' lambda args and body must be tuples, found '%p' and '%p'"
             lambda-args lambda-body)))

  (if (or (not= 1 (length lambda-args))
          (not= :symbol (type (get lambda-args 0))))
    (error (string/format
             "'map' must have only a single arg for the lambda, found '%p'" lambda-args)))

  (array/push output (string/format "len %s" ary_name))
  (array/push output "rep")
  (array/push output "idx")
  (array/push output (string/format "get %s" ary_name))

  (put self :el-name (get lambda-args 0))
  (:compile self output lambda-body)

  (array/push output "drop")
  (array/push output "end")

  nil)

(defn compile_
  [self output item]

  (cond
    (or (= :number (type item))
        (= :symbol (type item)))
    (let [el-name (get self :el-name)]
      (array/push output (string/format "push %p" item))
      (if (not (nil? el-name))
        (array/push output "swap")))

    (or (not= :tuple (type item))
        (empty? item))
    (error (string/format "malformed expression: expecting tuple, found '%p'" item))

    (let [f (get item 0)]
      (if (not= :symbol (type f))
        (error (string/format
                 "malformed expression: function '%p' must be a symbol: found a %p" f (type f))))

      (cond
        (= f 'var)
        (:compile-var self output (slice item 1))

        (has-value? ['+ '- '* '/ '%] f)
        (:compile-bin-op self output (slice item 1) f)

        (= f 'eq)
        (:compile-eq self output (slice item 1))

        (= f 'assert)
        (:compile-assert self output (slice item 1))

        (= f 'map)
        (:compile-map self output (slice item 1))

        (error (string/format "unhandled function '%p'" f))))))

(defn make-compiler
  []

  @{:el-name nil

    :compile compile_
    :compile-var compile-var
    :compile-bin-op compile-bin-op
    :compile-eq compile-eq
    :compile-assert compile-assert
    :compile-map compile-map})

(defn main
  [& args]

  (def in-path (get args 1))

  (def prog (->>
              in-path
              (slurp)
              (parse-all)))

  (def output @[])

  (each item prog
    (let [compiler (make-compiler)]
      (:compile compiler output item)))

  (def out-path (string in-path ".ism"))

  (spit out-path (string/join (map |(string $ "\n") output)))

  (printf "%s -> %s ... Done." in-path out-path))
