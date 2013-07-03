Flattening of terms:
====================

Given
>  S   => F (G b)
>  F φ => φ c

which is encoded as

>  A (C "F") (A (C "G") (C "b"))
>  A (V "φ") (C "c")

then, when we do a OI-step, we do a
substitution of φ by (G b) and end up with:

>  (G b) c

which is again encoded as:

>  A (A (C "G") (C "b")) (C "c")

but this can be written as

>  A (C "G") (C "b", C "c")

*if* we know, that arity(G) = 2.

Is this a special operation? Probably not,
as partial application is crucial in λ-calculi in
general.

Partial application in a term rewriting system is
*not* possible.

=> See how term rewriting system implement this?

