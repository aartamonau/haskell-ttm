---
title: haskell-ttm
---

## Rationale

**TTM** stands for *type-level Turing machine*. So **haskell-ttm** is a Turing
machine working withing **GHC**'s type checker. It's a bit hard to find sane
reason doing such things. I can say that it was just fun. If anyone can invent
any practical application of this please make me know. I will be extremely
grateful.

## Usage

In order to perform type-level computation using **ttm** package two things
must be done:

* Turing machine transition table must be defined
* Starting state of machine's state must be defined

### Common types

**TTM** uses alphabet contains two symbols:

~~~~~{.haskell}
data One  = One
data Zero = Zero
~~~~~

Both transition tables and tapes are defined in the terms of type lists. The
latter is defined as follows:

~~~~~{.haskell}
infixr 5 :+:
data Cons x xs = x :+: xs
data Nil = Nil
~~~~~

Actually, as we need only type-level representation of lists this definition
is excessive. We could only define appropriate type constructors omitting data
constructors. What's more, using **GHC**'s *TypeOperators* extensions it's
possible to define infix type constructors. But I find it more convenient to
write `Zero :+: Zero :+: One :+: Nil` instead of
`undefined :: Zero :+: Zero :+: One :+: Nil`.

### Transition tables

Transition table is just a list of transition rules. Here is the definition of
transition rule:

~~~~~{.haskell}
data Rule s a s' a' m = Rule s a s' a' m
~~~~~

It prescribes that given a machine in a state `s` and a symbol `a` under its
head new symbol `a'` must be written to the tape, new state `s'` must be
assigned to the machine and its head must be moved according to `m`.

Machine's state is represented as a decimal number as it's described in the
*"Number-parameterized types"*[^1] paper.

Examples:

~~~~~{.haskell}
Sz           -- zero state
(S1 (S0 Sz)) -- tenth state
~~~~~

And so forth.

Possible movements are the following:

~~~~~{.haskell}
data MLeft  -- move left
data Mright -- mover right
data MNoop  -- hold still
~~~~~

Simple transition table to move machine's head to times to the left and stop:

~~~~~{.haskell}
table = Rule     Sz   One (S1 Sz)  One MLeft :+:
        Rule     Sz  Zero (S1 Sz) Zero MLeft :+:
        Rule (S1 Sz)  One (S2 Sz)  One MLeft :+:
        Rule (S1 Sz) Zero (S2 Sz) Zero MLeft :+:
        Nil
~~~~~

### Tape

Tape is represented with respect to the current position of machine's head:

~~~~~{.haskell}
data Tape l c r = Tape l c r
~~~~~

Here `c` is the symbol that is situated under the head, `l` is the list of
symbols to the left of the head, and, similarly, `r` is the list of symbols to
the right of the head. As tape should be infinite `l` and `r` must contain
only meaningful part of the symbols from the relevant side. It's supposed that
all the cells after the specified in `l` or `r` are filled with `Zero`
symbols. If machine passes the end of specified part of the tape `Zero`
symbols are automatically created. And in case machine moves back without
changing anything those `Zero`s are eliminated to not contaminate the result.

### Execution

To execute Turing machine `exec` function must be used. It has the following
definition:

~~~~~{.haskell}
exec :: (Exec tbl (t, Sz) ~ r, IsTape t, IsTransitionTable tbl) => tbl -> t -> r
exec = undefined
~~~~~

The function takes a transition table, a tape and returns a pair of
transformed tape and the final state of machine. `IsTape` and
`IsTransitionTable` constraints just ensure that the arguments actually have
needed type structure. Type family `Exec` is what actually performs the
execution. Details are omitted. The body of the `exec` function which is just
`undefined` reminds us one more time that computation exists only in
compile-time.

When execution is performed all the rules in the transition table are
traversed until one that matches current state and symbol under the head is
found. Tape and machine's state are changed in accordance with this rule and
the process is repeated. Execution stops when it's impossible to find matching
rule.

### Examples

All the numbers in the examples are encoded using *unary system*[^2].

We can determine whether a number to the right of starting position is even or
odd. If it's odd we leave `One` symbol on the tape. If it's even — the tape is
left completely empty.

~~~~~{.haskell}
>>> import TTM
>>>
>>> even =
>>>   -- stop on first non-zero item to the right
>>>   Rule     Sz  Zero     Sz  Zero MRight :+:
>>>   Rule     Sz   One (S1 Sz)  One  MNoop :+:
>>>
>>>   -- remove the number keeping the oddity of eliminated part;
>>>   -- S1 Sz — erased part is even
>>>   -- S2 Sz — erased part is odd
>>>   Rule (S1 Sz)  One (S2 Sz) Zero MRight :+:
>>>   Rule (S2 Sz)  One (S1 Sz) Zero MRight :+:
>>>
>>>   -- write back the result
>>>   Rule (S1 Sz) Zero (S9 Sz) Zero MNoop  :+:
>>>   Rule (S2 Sz) Zero (S9 Sz)  One MNoop  :+:
>>>
>>>   Nil
>>>
>>> :t exec even (Tape Nil Zero (One :+: One :+: One :+: Nil))
exec even (Tape Nil Zero (One :+: One :+: One :+: Nil))
  :: (Tape Nil One Nil, S9 Sz)
~~~~~

So now we finally know that *3* is odd number.

Additionally we can find out what is the sum of *3* and *2*:

~~~~~{.haskell}
>>> import TTM
>>>
>>> add =
>>>   -- stop on first non-zero item to the right
>>>   Rule     Sz  Zero          Sz  Zero MRight :+:
>>>   Rule     Sz   One      (S1 Sz)  One  MNoop :+:
>>>
>>>   -- find first zero separator between the numbers
>>>   Rule (S1 Sz)  One      (S1 Sz)  One MRight :+:
>>>   Rule (S1 Sz) Zero      (S2 Sz) Zero  MNoop :+:
>>>
>>>   -- go to position next to the first unit of the second number
>>>   Rule (S2 Sz) Zero      (S2 Sz) Zero MRight :+:
>>>   Rule (S2 Sz)  One      (S3 Sz)  One MRight :+:
>>>
>>>   -- if we've found zero here then it's the last item of the second number;
>>>   -- so we stop after shifting the last unit;
>>>   Rule (S3 Sz) Zero      (S4 Sz) Zero  MLeft :+:
>>>
>>>   -- -- otherwise we continue shuttling
>>>   Rule (S3 Sz)  One      (S7 Sz)  One  MLeft :+:
>>>
>>>   -- move to the last unit to the first number
>>>   Rule (S4 Sz)  One      (S5 Sz) Zero  MLeft :+:
>>>   Rule (S5 Sz) Zero      (S5 Sz) Zero  MLeft :+:
>>>   Rule (S5 Sz)  One      (S6 Sz)  One MRight :+:
>>>   -- write unit here and halt
>>>   Rule (S6 Sz) Zero (S1 $ S0 Sz)  One MRight :+:
>>>
>>>   -- move one unit and then return back for the next one
>>>   Rule (S7 Sz)  One      (S8 Sz) Zero  MLeft :+:
>>>   Rule (S8 Sz) Zero      (S8 Sz) Zero  MLeft :+:
>>>   Rule (S8 Sz)  One      (S9 Sz)  One MRight :+:
>>>   Rule (S9 Sz) Zero      (S2 Sz)  One MRight :+:
>>>
>>>   Nil
>>>
>>> tape = (Tape Nil
>>>              Zero
>>>              (One :+: One :+: One :+: Zero :+: Zero :+: One :+: One :+: Nil))
>>>
>>> :t exec add tape
exec add tape
  :: (Tape
        (Cons One (Cons One (Cons One (Cons One (Cons One Nil)))))
        Zero
        Nil,
      S1 (S0 Sz))
~~~~~

## Reflections

This package was implemented using techniques from *"Fun with Functional
Dependencies"*[^3] at first. But transition from **GHC 6.12** to **GHC 7.0**
made that implementation really, really slow. That's why I rewritten it using
type families (*"Fun with Type Functions"*[^4] was of great help). Another
problem that arose during the transition is that in order to execute
transition tables bigger than one or two rules I was obliged to increase
**GHC**'s context stack (using *-fcontext-stack=n* option) significantly. It
was a hope that this would be resolved with migration to type families
implementation. Alas! And I'm still not sure whether
there's a way to resolve this inconvenience. Currently, on the **GHC 7.0.2** I
must set context stack size to *150* in order to execute summation example
from above (*100* will not go; other values has not been trialed).

## References

[^1]: [Number-parameterized Types](http://okmij.org/ftp/Haskell/number-parameterized-types.html)
[^2]: [Unary Numeral System](http://en.wikipedia.org/wiki/Unary_numeral_system)
[^3]: [Fun with Functional Dependencies](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.7806)
[^4]: [Fun with Type Functions](http://www.haskell.org/haskellwiki/Simonpj/Talk:FunWithTypeFuns)
