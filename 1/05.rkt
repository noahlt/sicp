(define (p) (p))

Ben's function p is an endless loop; if applied (that is, if the expression
"(p)" is ever evaluated), the interpreter will never terminate.

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

Applicative-order evaluators first evaluate the operator and operands, then
apply the operator to the (evaluated) operands.  Normal-order evaluators
first apply the operator to the operands, then evaluate the resulting 
expression.  A normal-order evaluator first expands everything.

If Ben's interpreter uses an applicative-order evaluator, the first step will
be to evaluate the operands, 0 and (p).  As noted above, evaluating the 
expression (p) will cause the interpreter to hang.

If Ben's interpreter uses a normal-order evaluator, the first step will be to
apply the function test to its arguments, resulting in the expression:

(if (= 0 0)
    0
    (p))

`if` is a special form which only evaluates one of its latter two arguments,
depending on the truthiness of the first argument.  When this `if` expression
is evaluated, since the predicate is true (0 = 0), it will return its second
argument, 0, without ever evaluating (p).  Thus, if Ben's interpreter uses a
normal-order evaluator, evaluating (test 0 (p)) will return 0.

