# Stack ops

push a  `( -- a )`
dup     `( a  -- a a )`
drop    `( a -- )`
swap    `( a b -- b a )`
rot     `( a b c -- b c a )`
over    `( a b -- a b a )`

-rot    `( a b c -- c a b )`    same as `rot` `rot`
2dup    `( a b -- a b a b )`    same as `over` `over`
nip     `( a b -- b )`          same as `swap` `drop`
tuck    `( a b -- b a b )`      same as `swap` `over`

# Rules

- For single var:
    - Assume var `a` is on top of stack.
    - If binary op:
        - If `a` is LHS then `dup`, push RHS, op, `swap`.
        - If `a` is RHS then `dup`, push LHS, `swap`, op, `swap`.
        - If `a` is neither then `-rot`, op, `swap`.
        - If `a` is both then `dup`, `dup`, op, `swap`.
    - If unary op:
        - If `a` is arg then `dup`, op, `swap`.
        - If `a` is not arg then `swap`, op, `swap`.
- For double var:
    - Assume vars `a` and `b` on top of stack.

# Single examples

`(+ a 1)`

dup     # a a
push 1  # a a 1
add     # a a+1
swap    # a+1 a

`(- (* a 2) (+ a 2))`

dup     # a a
push 2  # a a 2
mul     # a ax2
swap    # ax2 a

dup     # ax2 a a
push 2  # ax2 a a 2
add     # ax2 a a+2
swap    # ax2 a+2 a

-rot    # a ax2 a+2
sub     # a ax2-a+2
swap    # ax2-a+2 a

`(+ a a)`

dup     # a a
dup     # a a a
add     # a a+a
swap    # a+a a

`(- 10 a)`

dup     # a a
push 10 # a a 10
swap    # a 10 a
sub     # a 10-a
swap    # 10-a a

`(! (! a))`

dup     # a a
not     # a !a
swap    # !a a

swap    # a !a          ( `swap` `swap` is redundant, could be optimised away )
not     # a !!a
swap    # !!a a

# Double examples

`(+ a b)`

2dup    # a b a b
add     # a b a+b
-rot    # a+b a b

`(- b a)`

`(* a 2)`

`(+ b 10)`

`(- 20 a)`

`(/ 100 b)`

`(+ (* a 2) b)`

`(- (+ b (* b b)) (+ 10 a))`


%% vim:foldlevel=2
