#!/usr/bin/env janet

(defn compile-symbol
  [self output sym]

  (assert (not (nil? sym)))

  (def acc-name (get self :acc-name))
  (def el-name (get self :el-name))

  (cond
    (= sym el-name)
    (array/push output "dup")

    (= sym acc-name)
    (array/push output "over")

    (array/push output (string/format "push %s" sym)))

  (cond
    (not (nil? acc-name))
    (do
      (array/push output "rot")
      (array/push output "rot"))

    (not (nil? el-name))
    (array/push output "swap")))

(defn compile-var
  [self output args]

  (if (not= 2 (length args))
    (error (string/format "'var' must have two args, found %d for '%p'" (length args) args)))

  (if (not (and (nil? (get self :acc-name))
                (nil? (get self :el-name))))
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

(defn compile-bin-op-with-el
  [self output args op-str]

  (def arg0 (get args 0))
  (def arg1 (get args 1))

  (def el-name (get self :el-name))

  (cond
    # (op el el)
    (and (= arg0 el-name)
         (= arg1 el-name))
    (do
      (array/push output "dup")
      (array/push output "dup"))

    # (op el f)
    (= arg0 el-name)
    (do
      (:compile self output arg1)
      (array/push output "swap")
      (array/push output "over") # swap over == tuck
      (array/push output "swap"))

    # (op f el)
    (= arg1 el-name)
    (do
      (:compile self output arg0)
      (array/push output "swap")
      (array/push output "over")) # swap over == tuck

    # (op f g)
    (do
      (:compile self output arg0)
      (:compile self output arg1)
      (array/push output "rot")
      (array/push output "rot")))

  (array/push output op-str)
  (array/push output "swap"))

(defn compile-bin-op-with-el-acc
  [self output args op-str]

  (def arg0 (get args 0))
  (def arg1 (get args 1))

  (def acc-name (get self :acc-name))
  (def el-name (get self :el-name))

  (def arg0-is-acc (= arg0 acc-name))
  (def arg0-is-el (= arg0 el-name))
  (def arg1-is-acc (= arg1 acc-name))
  (def arg1-is-el (= arg1 el-name))

  (def arg0-is-f (and (not arg0-is-el) (not arg0-is-acc)))
  (def arg1-is-f (and (not arg1-is-el) (not arg1-is-acc)))

  (cond
    # (op acc el)
    (and arg0-is-acc arg1-is-el)
    (do
      (array/push output "over")
      (array/push output "over"))

    # (op el acc)
    (and arg0-is-el arg1-is-acc)
    (do
      (array/push output "over")
      (array/push output "over")
      (array/push output "swap"))

    # (op acc f)
    (and arg0-is-acc arg1-is-f)
    (do
      (:compile self output arg1)
      (array/push output "rot")
      (array/push output "push 2")
      (array/push output "pick")
      (array/push output "swap"))

    # (op el f)
    (and arg0-is-el arg1-is-f)
    (do
      (:compile self output arg1)
      (array/push output "rot")
      (array/push output "over")
      (array/push output "swap"))

    # (op f acc)
    (and arg0-is-f arg1-is-acc)
    (do
      (:compile self output arg0)
      (array/push output "rot")
      (array/push output "push 2")
      (array/push output "pick"))

    # (op f el)
    (and arg0-is-f arg1-is-el)
    (do
      (:compile self output arg0)
      (array/push output "rot")
      (array/push output "over"))

    # (op acc acc)
    (and arg0-is-acc arg1-is-acc)
    (do
      (array/push output "over")
      (array/push output "dup"))

    # (op el el)
    (and arg0-is-el arg1-is-el)
    (do
      (array/push output "dup")
      (array/push output "dup"))

    # (op f g)
    (and arg0-is-f arg1-is-f)
    (do
      (:compile self output arg0)
      (array/push output "rot")
      (array/push output "push 2")
      (array/push output "pick")
      (array/push output "push 2")
      (array/push output "pick")
      (:compile self output arg1)
      (array/push output "drop")
      (array/push output "drop"))

    (error "unhandled in compile-bin-op-with-el-acc"))

  (array/push output op-str)
  (array/push output "rot")
  (array/push output "rot"))

(defn compile-bin-op
  [self output args op-sym]

  (if (not= 2 (length args))
    (error (string/format "'%s' must have two args, found %d for '%p'" op-sym (length args) args)))

  (def op-str
    (case op-sym
      '+ "add" '- "sub" '* "mul" '/ "div" '% "mod"
      (error (string/format "todo bin-op %s" op-sym))))

  (if (nil? (get self :acc-name))
    (compile-bin-op-with-el self output args op-str)
    (compile-bin-op-with-el-acc self output args op-str)))

(defn compile-eq
  [self output args]

  (if (not= 2 (length args))
    (error (string/format "'eq' must have 2 args, found %d for '%p'" (length args) args)))

  (if (not (and (nil? (get self :acc-name))
                (nil? (get self :el-name))))
    (error (string/format "'eq' expression not allowed in morphism bodies")))

  (def arg0 (get args 0))
  (def arg1 (get args 1))

  # If either arg is a symbol then `cmp` it against the other arg.  If either arg is a number then
  # `eq` it instead.
  (cond
    (= :symbol (type arg0))
    (do
      (:compile self output arg1)
      (array/push output (string/format "cmp %s" arg0)))

    (= :symbol (type arg1))
    (do
      (:compile self output arg0)
      (array/push output (string/format "cmp %s" arg1)))

    (or (= :number (type arg0))
        (= :number (type arg1)))
    (do
      (:compile self output arg0)
      (:compile self output arg1)
      (array/push output "eq"))

    (error "unhandled in compile-eq")))

(defn compile-assert
  [self output args]

  (if (not= 1 (length args))
    (error (string/format "'assert' must have 1 arg, found %d for '%p'" (length args) args)))

  (if (not (and (nil? (get self :acc-name))
                (nil? (get self :el-name))))
    (error (string/format "'assert' expression not allowed in morphism bodies")))

  (:compile self output (get args 0))

  (array/push output "asrt"))

# (map array (fn [el] body))
(defn compile-map
  [self output args]

  (if (not= 2 (length args))
    (error (string/format "'map' must have two args (an array and a function), found %d for '%p'"
                          (length args) args)))

  (if (not (and (nil? (get self :acc-name))
                (nil? (get self :el-name))))
    (error (string/format "'map' expressions not allowed in morphism bodies (yet)")))

  # Assuming that the array exists and is an array, as long as the arg is a symbol.
  (def ary_name (get args 0))
  (if (not= :symbol (type ary_name))
    (error (string/format "'map' expects array name for first arg, found '%p'" ary_name)))

  (let [lambda (get args 1)]
    (if (or (not= :tuple (type lambda))
            (not= 3 (length lambda))
            (not= 'fn (get lambda 0)))
      (error (string/format
               "'map' expects a lambda (fn args body) second arg, found '%p'" lambda))))

  (def lambda-args (get-in args [1 1]))
  (def lambda-body (get-in args [1 2]))

  (if (or (not= :tuple (type lambda-args)))
    (error (string/format "'map' lambda args must be a tuple, found '%p'" lambda-args)))

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
  (array/push output "end"))

# (fold array acc (fn [acc el] body))
(defn compile-fold
  [self output args]

  (if (not= 3 (length args))
    (error (string/format
             "'fold' must have three args (an array, init and a function), found %d for '%p'"
             (length args) args)))

  (if (not (and (nil? (get self :acc-name))
                (nil? (get self :el-name))))
    (error (string/format "'fold' expressions not allowed in morphism bodies (yet)")))

  # Assuming that the array exists and is an array, as long as the arg is a symbol.
  (def ary_name (get args 0))
  (if (not= :symbol (type ary_name))
    (error (string/format "'fold' expects array name for first arg, found '%p'" ary_name)))

  (def acc-init (get args 1))

  (let [lambda (get args 2)]
    (if (or (not= :tuple (type lambda))
            (not= 3 (length lambda))
            (not= 'fn (get lambda 0)))
      (error (string/format
               "'fold' expects a lambda (fn args body) third arg, found '%p'" lambda))))

  (def lambda-args (get-in args [2 1]))
  (def lambda-body (get-in args [2 2]))

  (if (or (not= :tuple (type lambda-args)))
    (error (string/format "'fold' lambda args must be a tuple, found '%p'" lambda-args)))

  (if (or (not= 2 (length lambda-args))
          (not= :symbol (type (get lambda-args 0)))
          (not= :symbol (type (get lambda-args 1))))
    (error (string/format
             "'fold' must have two args for the lambda (acc el), found '%p'" lambda-args)))

  (:compile self output acc-init)

  (array/push output (string/format "len %s" ary_name))
  (array/push output "rep")
  (array/push output "idx")

  (array/push output (string/format "get %s" ary_name))

  (put self :acc-name (get lambda-args 0))
  (put self :el-name (get lambda-args 1))
  (:compile self output lambda-body)

  (array/push output "drop") # drop the element
  (array/push output "drop") # drop the original accumulator
  (array/push output "end"))

(defn compile_
  [self output item]

  (cond
    (= :number (type item))
    (do
      (array/push output (string/format "push %p" item))
      (cond
        (not (nil? (get self :acc-name)))
        (do
          (array/push output "rot")
          (array/push output "rot"))

        (not (nil? (get self :el-name)))
        (array/push output "swap")))

    (= :symbol (type item))
    (:compile-symbol self output item)

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

        (= f 'fold)
        (:compile-fold self output (slice item 1))

        (error (string/format "unhandled function '%p'" f))))))

(defn make-compiler
  []

  @{:acc-name nil
    :el-name nil

    :compile compile_
    :compile-symbol compile-symbol
    :compile-var compile-var
    :compile-bin-op compile-bin-op
    :compile-eq compile-eq
    :compile-assert compile-assert
    :compile-map compile-map
    :compile-fold compile-fold})

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
