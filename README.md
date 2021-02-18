# About

My SICP playground...

Some examples and exercises I found highly interesting in [SICP](http://mitpress.mit.edu/sicp/full-text/book/book.html).

| Source                                          | Description                                                                                                                        |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| [mceval.scm](../blob/master/mceval.scm)         | The metacircular evaluator from SICP, plus the `let` special form and `load` support.                                              |
| [mceval_ext.scm](../blob/master/mceval_ext.scm) | The mceval, plus the `and`, `or`, `let*` and named-let special forms, internal definitions, varargs.                               |
| [dynscope.scm](../blob/master/dynscope.scm)     | The mceval, but with dynamic scoping.                                                                                              |
| [lazy.scm](../blob/master/lazy.scm)             | The lazy evaluator. Primitive procedures are strict, while compound procedures are non-strict in each arguments. Uses memoization. |
| [analyze.scm](../blob/master/analyze.scm)       | The anayzing evaluator.                                                                                                            |
| [ambeval.scm](../blob/master/ambeval.scm)       | The nondeterministic Scheme evaluator.                                                                                             |

# Copyright

MIT, see [LICENSE](LICENSE)

Copyright (C) Imre Horvath 2021
