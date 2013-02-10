The problem with Eva's new-if is that, since it is not a special form, all
of its arguments will always be evaluated.  This doesn't make a difference
in her (bad) example, because 0 and 5 evaluate to themselves.  But in the
case of sqrt-iter, which conditionally recurses, new-if will always evaluate
all its arguments first, including the recursive clause.  So if Alyssa uses
new-if to write her sqrt-iter procedure, it will never terminate, causing the
interpreter to hang.

