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
#### Expressions
I do not wish to repeat after the
[official bnfc tutorial](http://bnfc.digitalgrammars.com/tutorial.html),
so I will provide only the essential information,
and if you don't feel comfortable with `bnfc`'s syntax,
please refer to their tutorial.
Productions in `bnfc` are labeled, for instance

```{bnfc}
EAdd. Exp   ::= Exp "+" Exp ;
EMul. Exp   ::= Exp "*" Exp ;
```

Such a production will cause `bnfc` to create the following
abstract syntax in Haskell:

```
data Exp =
    ...
    | EAdd Exp Exp
    | EMul Exp Exp
    ...
```

Now, be very careful with those productions. As you may recall,
some context-free grammars are *ambiguous*, i.e.
one string can be derived in two different ways. Consider, for instance,
an expression `1 + 2 * 3`. This could be parsed in two ways:
<p class="center"><img style="width:50%;float:left" src="../images/add_first.jpg"></p>
<p class="center"><img style="width:50%;float:right" src="../images/mul_first.jpg"></p>
Clearly, any person familiar with operator precedence would choose
the first of the pictures. Bnf grammars, however, are not of such kind.

Fortunately, we are not the first ones to encounter this
problem. Generally, the solution to this would be to add
another nonterminal symbol for higher precedence operations,
like
```
EAdd. Exp   ::= Exp "+" Exp1 ;
EMul. Exp1  ::= Exp1 "*" Exp2 ;
...
```
This can be easily interpreted: the higher the number
at the end of the nonterminal, the higher its precedence.
This means that `Exp1` (multiplication) will be parsed
*before* `Exp`, and so will multiplication.

This would require a bunch of additional productions, like
`Exp ::= Exp1`. Fortunately, `bnfc` has a mechanism for that,
called [*coercions*](https://bnfc.readthedocs.io/en/latest/lbnf.html#coercions).

And so, the whole concrete syntax for
expressions could look like:

```
EAdd.   Exp     ::= Exp "+" Exp1 ;
ESub.   Exp     ::= Exp "-" Exp1 ;
EMul.   Exp1    ::= Exp1 "*" Exp2 ;
EDiv.   Exp1    ::= Exp1 "/" Exp2 ;
ENum.   Exp2    ::= Integer ;
EVar.   Exp2    ::= Ident ;

coercions Exp 2 ;
```
> **Exercise**: read the [tutorial](http://bnfc.digitalgrammars.com/tutorial.html)
and think of the productions for boolean expressions.

#### Statements
The concrete syntax for statements should be trivial
to write now. Two hints though:

* `Ident` is a special nonterminal symbol
that allows to parse "variables", i.e. nonempty string
starting with non-digits. It can be used like:
`
SAss .  Stm     ::= Ident ":=" Exp ;
`
* *Blocks* in bnfc allow us to parse a sequence of items, so
we can write the production for `SBlock` like this:
```
SBlock ::= "{" [Stm] "}" ;
separator Stm ";" ;
terminator Stm "" ;
```

> **Exercise**: write all the productions for statements.

## Interpreter
You can download the `bnfc` grammar for `Tiny`
[here](https://raw.githubusercontent.com/jytug/jytug.github.io/develop/interpreter/tiny.bnfc)
