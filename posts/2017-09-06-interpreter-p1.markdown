---
title: An interpreter for a trivial imperative language, part 1 (semantics)
---

## Preliminaries
This is a beginners' tutorial on how to write an interpreter
for a *really* simple imperative language. I wrote it because
my friend asked me for some information on where to even begin
looking around for such a thing, and I couldn't redirect him
to a specific place, so here we are.

This is the first part, covering mostly the theoretical concepts.
In part two, I will provide a step-by-step tutorial for implementing
the interpreter in Haskell.

### What you'll need
First of all, I require basic knowledge of Haskell and some
understanding of fundamental mathematical concepts, like functions.
We won't do any category theory or advanced programming, however I
recommend you take a peek at
[monad transformers](http://book.realworldhaskell.org/read/monad-transformers.html).
Also, I am assuming you know what a
[BNF Grammar](http://www.cs.man.ac.uk/~pjj/bnf/bnf.html)
is.

We will also use some crazy software which will solve the
problem of parsing for us - `bnfc`. In order to produce
Haskell, it requires also `alex` and `happy`. You can get
all of them from most package managers, including apt.

### Notation
I'm going to use some convenient notation shortcuts, listed
here:

* Lambda-notation: \\( \\lambda x \\in X .\\varphi(x) \\) means
a function \\( f : X \\to Y \\) such that \\( f(x) = \\varphi(x) \\).
We'll often omit the domain \\( X \\).

* Function application: we'll use the Haskell-ey way of
applying funtions to arguments, where `f x` means the same
as `f(x)`.

* Function update: suppose \\( f : X \\rightharpoonup Y \\) is
a partial function. By \\( f [ x \\to y ] \\) we'll denote a
function updated at \\( x \\), i.e. a function given by:
$$f[x \to y] z = \begin{cases} y & \text{if } z = x\\ f z &\text{otherwise} \end{cases} $$

### What we're going to do

A programming language is defined by its syntax and semantics.
The syntax is usually defined with a context-free grammar,
which is the case for our working example. Note, however, that most
languages are not context-free, for instance
[Python](http://trevorjim.com/python-is-not-context-free/).

Like I mentioned before, we will not implement a parser, making
use of existing tools.

What we will focus on mostly are the language's semantics.
To put it as simple as it gets: program's semantics are
a mathematical model of the computation the program
represents.

## <a name="spec"></a> The language
The language we'll be implementing is called `Tiny` where I
come from. We won't prove its Turing completeness, which some
[madmen](https://esolangs.org/wiki/Turing_tarpit) might attempt.
However this language represents all of the most basic constructs
of imperative programming.

### Syntax
`Tiny`'s syntax consists of the following clauses:

* Numerals \\( n \\in \\textbf{Num} \\) with the following syntax:
    
    $$ \textbf{Num} \ni n \to 0~|~-1~|~1~|~-2~|~\dots $$

* Variables \\( x \\in \\textbf{Var} \\):

    $$ \textbf{Var} \ni x \to \texttt{x}~|~\texttt{y}~|~\dots $$

* Integer expressions \\( e \\in \\textbf{Exp} \\):

    $$ \textbf{Exp} \ni e \to n~|~x~|~e_1+e_2~|~e_1*e_2~|~e_1-e_2 $$

* Boolean expressions \\( b \\in \\textbf{BExp} \\):

    $$ \textbf{BExp} \ni b \to \texttt{true}~|~\texttt{false}~|~e_1 \leq e_1~|~\texttt{not } b'~|~b_1 \texttt{ and } b_2 $$

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
language, like Haskell, would not require a state to be a part
of the semantics, unlike any of the imperative languages known
to man.

In order to avoid a lengthy discussion on various approaches to
defining semantics, we'll answer a simpler question:
what do syntactic constructs of `Tiny` mean?

Let's begin with everything but statements. Expressions' values
depend on what has happend in the code before their evaluation,
and might even cause some errors, like `undeclared variable` or
`division by zero`. In order to devise the semantics of expressions,
first we need to define what a **state** of the
program is. At any given time, a `Tiny` program's state is defined
by what all variables' values are. At the beginning none of
them are defined and they stop being undefined when something is
assigned to them for the first time.

This means that a state can be modelled as a *partial function*
$$ s \in \textbf{State} =  \textbf{Var} \rightharpoonup \mathbb{Z} $$

Recall that a partial function is a function that might not be
defined everywhere on its domain, in other words it is an entire function

$$ s : \textbf{Var} \to \mathbb{Z} \cup \{\perp\} $$

where \\( s ~ x = \\perp \\) means that \\( s \\) is undefined on \\( x \\).

* **Numerals**. That's an easy one. Obviously,
\\(\\texttt{1}\\) means \\(1\\). Hence, the semantic function
\\(\\mathcal{N} : \\textbf{Num} \\to \\mathbb{Z} \\) is given by:
$$\mathcal{N}(\texttt{0}) = 0$$
$$\mathcal{N}(\texttt{-1}) = -1$$
$$\mathcal{N}(\texttt{1}) = 1$$
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

>**Note**: We implicitly assume that errors propagate, i.e. if 
 \\(  V(e_1) = \\perp \\), then so is \\( V(e_1 + e_2) \\).
 As you can imagine, in such situations monads come in very handy.

* **Boolean expressions**. Like in the case of integer expressions,
the semantic function will indicate a boolean expression's value,
from the set \\( \\{ \\mathrm{tt}, \\mathrm{ff} \\} \\). Which
represent respectively truth and falsehood.

$$ \mathcal{B} : \textbf{BExp} \to \textbf{State} \rightharpoonup \{ \mathrm{tt}, \mathrm{ff} \}$$
$$ \mathcal{B} (\texttt{true}) = \lambda s . \mathrm{tt} $$
$$ \mathcal{B} (\texttt{false}) = \lambda s . \mathrm{ff} $$
$$ \mathcal{B} (e_1 \leq e_2) = \lambda s . \left( \mathcal{E}(e_1) s \leq \mathcal{E}(e_2) s \right)  $$
$$ \mathcal{B} (\texttt{not } b') = \lambda s . \left( \neg \mathcal{B}(b') s \right) $$

Everything looks fine for now. Now, let's head to the beef -
statements.

* **Statements**. We won't define a semantic function for
statements. What we'll do instead is define how a statement
*changes* the state. This approach is called *small step
computational semantics*
([see more](http://fsl.cs.illinois.edu/images/7/74/CS522-Spring-2011-PL-book-smallstep.pdf)).\
We will use the following logical notation to denote structural
rules:\
$$\frac{C_1, \dots, C_k}{C}$$
This can be read as *assuming all \\(C_1, \dots, C_k \\), then \\( C \\)
follows*.\
A configuration of the program is a pair
\\( \\langle \\texttt{stm}, s\\rangle \\) or a single \\( s \\),
where \\( \\texttt{stm} \\) is the next statement to execute
and \\( s \\) is the current state. The configurations consisting
of a lone state are considered *final* (i.e. there's nothing to be done).\
Now, we define transitions between configurations.
A transition \\( (stm, s) \\Rightarrow s' \\), in its essence,
can be read as: "if we are in state s and execute the statement
\\( stm \\), then the state changes to \\( s' \\)". Following this
intuition, it is not difficult to devise the transitions for
our statements:
$$\left \langle x \texttt{ := } e, s \right \rangle \Rightarrow s\left[x \to \mathcal{E}(e) s\right]$$
$$\left \langle \texttt{skip }, s \right \rangle \Rightarrow s$$
$$\frac
{\left \langle S_1, s \right \rangle \Rightarrow s', ~~ \left \langle S_2, s' \right \rangle \Rightarrow s''}
{\left \langle S_1\texttt{;}S_2, s \right \rangle \Rightarrow s''}$$
$$\frac
{\mathcal{B}(b) s = \mathrm{tt}, ~~ \left \langle S_1, s \right \rangle \Rightarrow s'}
{\left \langle \texttt{if } b \texttt{ then } S_1 \texttt{ else } S_2, s \right \rangle \Rightarrow s'}$$
$$\frac
{\mathcal{B}(b) s = \mathrm{ff}}
{\left \langle \texttt{while } b \texttt{ do } S, s \right \rangle \Rightarrow s}
$$
$$\frac
{\mathcal{B}(b) s = \mathrm{tt}, ~~
\left \langle S, s \right \rangle \Rightarrow s', ~~
\left \langle \texttt{while } b \texttt{ do } S, s' \right \rangle \Rightarrow s''
}
{\left \langle \texttt{while } b \texttt{ do } S, s \right \rangle \Rightarrow s''}
$$

And that would be all! We have constructed a very simple mathematical model
for the semantics of `Tiny`'s semantics. In the next post, we will
use this model to implement an actual interpreter for this language,
possibly using some cheats for I/O.
