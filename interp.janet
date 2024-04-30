#!/usr/bin/env janet

(defn parse-line
  [line]

  (def grammar
    ~{:main (* (+ :item) -1)

      :item (+ :var-decl
               :push
               :drop)

      :var-decl (group (* (constant :var) "var" :s+ :ident :literal))

      :push (group (* (constant :push) "push" :s+ (+ :num :ident)))
      :drop (* (constant :drop) "drop" :s*)

      :ident (* (<- (* :a (any :w))) :s*)

      :literal (+ :num
                  :array)

      :num (* (number (some :d)) :s*)

      :array (* "[" :s* (replace (some :num) ,tuple) "]" :s*)})

  (let [matches (peg/match grammar (string/trim line))]
    (if (nil? matches)
      (error (string "parse failure: '" line "'"))
      (get matches 0))))

(defn main
  [& args]

  (when (<= (length args) 1)
    (print "use: " (get args 0) " FILE")
    (os/exit 0))

  (def src-buf (slurp (get args 1)))

  (def vars @{})
  (def stack @[])

  (defn print-stack
    []

    (prin "  - stack:")
    (each el (reverse stack)
      (prin " " el))
    (print))

  (var line-idx 1)
  (loop [line :in (string/split "\n" src-buf)
         :after (+= line-idx 1)]
    (when (not (empty? line))
      (let [cmd (parse-line line)]
        #(pp cmd)
        (match cmd

          @[:var name val]
          (do
            (print (string/format "- initialising %s to %p" name val))
            (put vars name val))

          @[:push val]
          (case (type val)
            :number
            (do
              (print (string/format "- pushing number %d" val))
              (array/push stack val)
              (print-stack))

            :string
            (let [num (get vars val)]
              (if (nil? num)
                (error (string/format "line %d: unknown variable '%s'" line-idx val)))
              (print (string/format "- pushing variable %s" val))
              (array/push stack num)
              (print-stack))

            (error (string/format "line %d: unhandled 'push' value type: %p" line-idx (type val))))

          :drop
          (if (empty? stack)
            (error (string/format "line %d: attempt to drop on empty stack" line-idx))
            (do
              (print "- dropping")
              (array/pop stack)
              (print-stack)))

          (error (string/format "line %d: unknown cmd: %p" line-idx cmd)))))))
