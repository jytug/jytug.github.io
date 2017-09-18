---
title: An interpreter for a trivial imperative language, part 2 (implementation)
---

## BNFC
Like mentioned in the [first part](2017-09-06-interpreter-p1) of
this tutorial, we will not concern ourselves with creating a lexer
and a parser for our language - instead, we will use
[bnfc](http://bnfc.digitalgrammars.com), a tool for precisely that.
If you wish to read something about lexing and parsing, go
[here](http://savage.net.au/Ron/html/graphviz2.marpa/Lexing.and.Parsing.Overview.html).

You can download `bnfc` from the website, but it's
also available in various package managers, including apt.
It uses `alex` to create a lexer, and `happy` to create a
parser, both of which are easily available as well.

### `Tiny`'s grammar
I do not wish to repeat after the
[official bnfc tutorial](http://bnfc.digitalgrammars.com/tutorial.html),
so I will provide only the essential information,
and if you don't feel comfortable with `bnfc`'s syntax,
please refer to their tutorial.
Productions in `bnfc` are labeled, for instance

```{bnfc}
EAdd. Exp   ::= Exp "+" Exp ;
```

Such a production will cause `bnfc` to create the following
abstract syntax in Haskell:

```
data Exp =
    ...
    | EAdd Exp Exp
    ...
```

> **Note:** Some context-free grammars are *ambiguous*, i.e.
one string can be derived in two different ways. Consider, for instance,
an expression `1 + 2 * 3`. This could be parsed in two ways:
<p class="center"><img width=100% src="../images/add_first.jpg"></p>
<p class="center"><img width=100% src="../images/mul_first.jpg"></p>
Clearly, any person familiar with operator precedence would choose
the first of the pictures. Bnf grammars, however, are not of such kind.
For this purpose, `bnfc`
