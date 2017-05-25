---
title: An interpreter for a trivial imperative language in Haskell
---

## Preliminaries
This is a beginners' tutorial on how to write an interpreter
for a *really* simple imperative language. I wrote it because
my friend asked me for some information on where to even begin
looking around for such a thing, and I couldn't redirect him
to a specific place, so here we are.

If you're in haste, you can already jump to the language's
[specification](#spec).

### What you'll need
First of all, I require basic knowledge of Haskell and basic
understanding of some mathematical concepts. We won't do any
algebraic topology or advanced Haskell programming, however I
recommend you take a peek at
[monad transformers](http://book.realworldhaskell.org/read/monad-transformers.html).
Also, I am assuming you know what a BNF Grammar is.

We will also use some crazy software which will solve the
problem of parsing for us - `bnfc`. In order to produce
Haskell, it requires also `alex` and `happy`. You can get
all of them from most package managers like apt.

### Notation
I'm going to use some convenient notation shortcuts, listed
here:

* Lambda-notation: \\( \\lambda x \\in X .\\varphi(x) \\) means
a function \\( f : X \\to Y \\) such that \\( f(x) = \varphi(x) \\).
We'll often omit the domain \\( X \\).

* Function application: we'll use the Haskell-ey way of
applying funtions to arguments, where `f x` means the same
as `f(x)` does in math.

### What we're going to do

## <a name="spec"></a> The language
The language we'll be implementing is called `Tiny` where I
come from. We won't prove its Turing completeness, which some
[madmen](https://esolangs.org/wiki/Turing_tarpit) might attempt.
However this language represents the most basic constructs
of imperative programming.

### Syntax
`Tiny`'s syntax contains the following constructs:

* Numerals \\( n \\in \\textbf{Num} \\) with the following syntax:
    
    $$ \textbf{Num} \ni n \to 0~|~-1~|~1~|~-2~|~\dots $$

* Variables \\( x \\in \\textbf{Var} \\):

    $$ \textbf{Var} \ni x \to \texttt{x}~|~\texttt{y}~|~\dots $$

* Integer expressions \\( e \\in \\textbf{Exp} \\):

    $$ \textbf{Exp} \ni e \to n~|~x~|~e_1+e_2~|~e_1*e_2~|~e_1-e_2 $$

* Boolean expressions \\( b \\in \\textbf{BExp} \\):

    $$ \textbf{BExp} \ni b \to \texttt{true}~|~\texttt{false}~|~e_1 \geq e_1~|~\texttt{not } b'~|~b_1 \texttt{ and } b_2 $$

* Statements \\( S \\in \\textbf{Stm} \\):

    $$ \textbf{Stm} \ni S \to x \texttt{ := } e ~|~\texttt{skip}~|
    ~S_1 \texttt{;}S_2 \\ |~ \texttt{if } b \texttt{ then } S_1 \texttt{ else } S_2~|~\texttt{while } b \texttt{ do } S'$$

This is it! All `Tiny` can do is an assignment, an `if`
statement and a `while` loop. It looks pretty easy for now,
especially since we know what all those constructs should
**do**. The next question we'll be facing is: what do those
syntactic constructs actually **mean**?

### Semantics
The crucial question here is *how do we model a programming
language's behaviour*? The correct answer to this question
is: it depends on the language! Obviously, a purely functional
programming language, like Haskell, is stateless, while an
actual imperative language *has to have a state*. And it's just
the tip of the iceberg.

In order to avoid this iceberg, we'll answer a simpler question:
what do syntactic constructs of `Tiny` mean?

Let's begin with everything but statements - devising what
statements mean is going to be a little more tricky. In
order to do that, we need to define what a **state** of the
program is. At any given time, a `Tiny` program's state is defined
by what all variables' values are. At the beginning none of
them are defined and their definition happens at their
first assignment.

This means, that a state can be modelled as a *partial function*
$$ s \in \textbf{State} =  \textbf{Var} \rightharpoonup \mathbb{Z} $$

* **Numerals**. That's an easy one. Obviously,
\\(\\texttt{1}\\) means \\(1\\). Hence, the semantic function
\\(\\mathcal{N} : \\textbf{Num} \\to \\mathbb{Z} \\) is given by:
$$\mathcal{N}(\texttt{0}) = 0$$
$$\mathcal{N}(\texttt{-1}) = -1$$
$$\mathcal{N}(\texttt{1}) = -1$$
$$ \dots $$

* **Variables**. A variable means as much as its current value.
This means that a semantic function needs to take the state
into account. One way of expressing this is to define the
semantic function as the state applied to the variable.
$$ \mathcal{V} : \textbf{Var} \to \textbf{State} \rightharpoonup \mathbb{N}$$
$$ \mathcal{V}(x) = \lambda s . (s x) $$

* **Expressions**. The semantic function for expressions can
be defined analogously to the one for variables. Some expressions'
values depend on the state, not all of them. Let's see how this
could work:

$$ \mathcal{E}: \textbf{Var} \to \textbf{State} \rightharpoonup \mathbb{N} $$
$$ \mathcal{E} (n) = \lambda s . \mathcal{N}(n) $$
$$ \mathcal{E} (x) = \mathcal{V}(x) $$
$$ \mathcal{E} (e_1 + e_2) = \lambda s . (\mathcal{E}(e_1) s + \mathcal{E}(e_2) s)$$
$$ \mathcal{E} (e_1 * e_2) = \lambda s . (\mathcal{E}(e_1) s * \mathcal{E}(e_2) s)$$
$$ \mathcal{E} (e_1 - e_2) = \lambda s . (\mathcal{E}(e_1) s - \mathcal{E}(e_2) s)$$

Everything looks fine for now. Now, let's head to the beef -
statements.
