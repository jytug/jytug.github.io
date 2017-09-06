---
title: An interpreter for a trivial imperative language in Haskell, part 2
---
## Implementation

### Syntax
Like I mentioned before, we are going to use
[bnfc](bnfc.digitalgrammars.com)
to write the lexer and parser for us - we won't be focusing
on this topic too much, however you can read up on it
[elsewhere](http://savage.net.au/Ron/html/graphviz2.marpa/Lexing.and.Parsing.Overview.html).

In order to do that, we'll need to get to know `bnfc` and
its format for the grammar. In general, we'll use only the
most basic features, so there's no need to read the whole
tutorial from their site.

In general, rules in `bnfc` have the following format:

```
Label "." Ident "::=" (Ident | String)* ";"
```

The label will be the name of our type in Haskell.
The identifiers correspond to non-terminals and the strings
- to the terminals. Bnfc also supports some special identifiers,
such as `Integer` and `Ident`, which correspond, respectively,
to strings that respresent integers, and string that are
acceptable as variable names.

For instance, our productions for expressions would go as:

```
ENum.   Exp     ::= Integer ;
EVar.   Exp     ::= Ident ;
EAdd.   Exp     ::= Exp "+" Exp ;
EMul.   Exp     ::= Exp "*" Exp ;
ESub.   Exp     ::= Exp "-" Exp ;
```

For some it might be immediately clear that this is not
entirely correct. Cosinder the following expression:

```
1 + 2 * 3
```

Our grammar doesn't define the way this will be parsed.
There are two options.

<p class="center">
<img style="text-align:center" src="/images/add_first.jpg" width="400">
</p>

This is obviously the correct one. However, we never said that
is the desired way to parse it, so `bnfc` might as well parse
it like this:

<p class="center">
<img src="/images/mul_first.jpg" width="400">
</p>

Fortunately, we're not the first ones to encounter
this problem, and `bnfc` has a solution for it, called
*precedence levels*. This means that we can specify what
will be parsed first, by giving it *higher precedence*:

```
ENum.   Exp2    ::= Integer ;
EVar.   Exp2    ::= Ident ;
EAdd.   Exp     ::= Exp "+" Exp1 ;
ESub.   Exp     ::= Exp "-" Exp1 ;
EMul.   Exp1    ::= Exp1 "*" Exp2 ;

coercions Exp 2 ;
```

Not that the right-hand side argument of all binary
operations has higher precedence, because those
operations naturally *bind to the right*. Also,
note the last line

```
coercions Exp 2 ;
```

It tells `bnfc` that the highest precedence level for `Exp`
is two and that expressions can be put in parenthesis.
This line is equivalent to

```
_. Exp  ::= Exp1 ;
_. Exp1 ::= Exp2 ;
_. Exp2 ::= "(" Exp ")" ;
```

**Exercise**: Write the productions for boolean expressions
(or you can see the [source code](#source)).

Now, let's head to the statements. It all should be pretty
straightforward by now. We can make use of one convenient
trick `bnfc` provides to parse blocks more easily, namely
lists.

Suppose we already have all productions for statements:

```
SAss.   Stm     ::= Ident ":=" Exp ;
SSkip.  Stm     ::= "skip" ;
SIfel.  Stm     ::= "if" BExp "then" Stm "else" Stm ;
SWhile. Stm     ::= "while" BExp "do" Stm ;
```

Now, instead of writing the block clause as

```
SBlock. Stm     ::= Stm ";" Stm ;
```

and using the fact that a in `Tiny` is just a sequence
of statements, we will add one additional clause `Program`,
looking like this

```
Program.    Prog     ::= [Stm] ;

separator Stm ";" ;
terminator Stm "" ;
```

The meaning of this is pretty straighforward. A `Program` is
a sequence of statements - we will soon see how this translates
to Haskell. Moreover, what separates the elements of this
list is a semicolon, and that nothing terminates such sequence
(in particular this means that, unlike in `C`, the last statement
in a sequence doesn't require being followed by the semicolon).

Now, that we have saved out `bnfc` file as, let's say, `tiny.bnfc`,
we can proceed to compile our compiler! To do so, run the following
command:

```{sh}
bnfc -haskell -m tiny.bnfc
```

This will produce a lot of files, including a `Makefile`.
By running it with `make`, we will get one executable, called,
in our case, `TestTiny`. Now, you can run it an see how nicely
our langue is being parsed, for example:

```{sh}

$ ./TestTiny 
x := 1;
y := 2;
if x <= y then z := y else z := x

Ctrl-D

Parse Successful!

[Abstract Syntax]

Program [SAss (Ident "x") (ENum 1),SAss (Ident "y") (ENum 2),SIfel (BLeq (EVar (Ident "x")) (EVar (Ident "y"))) (SAss (Ident "z") (EVar (Ident "y"))) (SAss (Ident "z") (EVar (Ident "x")))]

[Linearized tree]

x := 1 ;
y := 2 ;
if x <= y then z := y else z := x
```

If you wish to get to know bnfc more and use it to create more
advanced parsers, please refer to the official
[tutorial](http://bnfc.digitalgrammars.com/tutorial.html).

Hey! How are we going to test the whole thing? Just for the sake
of visual effects, we will add an additional statement, `print`,
in our `bnfc` file, like this:

```
SPrint. Stm      ::= "print" Exp ;
```

We haven't defined its semantics - no need to worry, as Haskell
will take care for that for us.

### The interpreter
Out interpreter will be a Haskell module, exporting one function
to interpret programs. First, let's figure out what the signature
of that function should be. If you take a look at `AbsTiny.hs`,
you should see the definitions of data structures corresponding
to the labels defined in our grammar, for instance for expressions:

```
data Exp
    = ENum Integer
    | EVar Ident
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
  deriving (Eq, Ord, Show, Read)
```

What we need to interpret is a `Prog`, defined as follows:

```
data Prog = Program [Stm]
  deriving (Eq, Ord, Show, Read)
```

This means that our interpreting function should take one `Prog`
argument, and the result ought to be an instance of a monad that
takes into account the following three:

* Runtime errors, like use of undefined variables or division by zero,
* Output, for the `print` statement,
* State of the computation

Recall that a `Tiny` program's state is just a partial function

$$ s: \textbf{Var} \rightharpoonup \mathbb{Z} $$

In Haskell, we can write it as:

```
IState = Ident -> Maybe Int
```

Having that, we can define the monad to work with, using the monad
transformers `StateT` and `ExceptT`

```
import Control.Monad.State
import Control.Monad.Except

type ExStIO = ExceptT String (StateT IState IO) ()
```

Let's read through this step by step. `StateT IState IO` is just
a stateful `IO` monad, where the state is represented by an `IState`
object. If we denote this as `M`, then the whole type takes the form

```
ExceptT String 
```
