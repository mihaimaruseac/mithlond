---
date: 2018-07-04
title: Math in ancient Babylon
postid: 4
---

One of my hobbies is learning about history, and, since I like to claim I am a
man of science, I will always enjoy reading about the way people did science
in the past. This post is just an example of that, it came after I read
Knuth's "Ancient Babylonian Algorithms" (TODO: cite) from the collection found
in "Selected Papers on Computer Science" (TODO: cite).

The paper turned out to be interesting because it offered new interesting
insights beside the already known facts about ancient Summer. In fact, the
first page of the article brings a significant piece of information: while it
is well known that Babylonians used a base 60 system (TODO: image and link to
it, zero handling), very few people know that they actually used floating
point arithmetic.

What does this mean? The two digit number $2,20$ (using the same notation as
Knuth to not paint the hieroglyphs in this article) means both $2 \times 60 +
20 = 140$ but also $2 + 20/60 = 2.3333..$. Yes, it is a peculiar notation, the
exponent of the floating point is not written at all, so the same number
$2,20$ can represent any number of the form $140*60^n$ where $n$ is any
integer, positive or negative.

How did scientists came to this finding? Probably because of the YBC 7289
tablet (TODO: link, image). On it, we see a square labeled $30$ (i.e., the
side's length) and two sets of numbers marking the diagonal. Assuming base 60
with no floating point, the first one, $1,24,51,10$, would represent $1 \times
60^3 + 24 \times 60^2 + 51 \times 60 + 10 = 305470$ and the second one,
$42,25,35$, would represent $42 \times 60^2 + 25 \times 60 + 35 = 152735$. If
any of them would represent the side of the diagonal, it should have been
$30\sqrt{2} = 42.4264..$. However, the first number is $7200$ larger than the
desired diagonal length ... so we can try ... (use floating point) and we see
we get a good approximation. In fact, if we apply the same approach to the
other number we get ... which is a very good approximation to $\sqrt{2}$.

(TODO: recheck these numbers)

Addition and subtraction in this system is easy. If both numbers are in the
same range of powers of 60 (TODO: example), then all one has to do is add the
nibbles by itself and take care of the carry, similar to how we do addition
in base 10. If the numbers don't have the same exponent, the scribe would
first have to align them and then proceed as before.

Multiplication and division are more complicated and were done via tables.
TODO: expand, add recriprocal table, tables for each principal p, multiples of
1,2,3,20,30,40,50 and 47 is 40 + 7.

For example, here is how a scribe would compute the reciprocal of $2,5$ using
the tables TODO expand.

This sounds very close to an algorithm that one can have a computer execute.
And, it turns out that the scribes were following standard procedures, for
example this almost ritualistic text was written on the clay table associated
with the solution to the above reciprocal (loosely translated into English):

TODO: cite procedure

We know that this is a procedure followed by rote because a similar problem
with a different input has the following solution:

TODO: cite procedure

Only the numbers have changed, the rest of the text is the same. In fact, even
in cases where one would have to multiply by $1$, the text wouldn't change.
Thus, we can say that the scribe solving math problems was just a human
computer at the time, following texts he had to learn by rote and reproduce
daily.

Besides the simple operations mentioned here, it turns out Babylonians had
procedures to solve quadratic problems, compute geometric series,
algorithmically generate larger tables of reciprocals and square roots, etc.
From analyzing these procedures we find that primitive concepts of procedure
calls/macro expansions were present: when computing a complex expression, the
text for the component expressions was inserted into the larger text,
including constructs such as "make a copy of this value" to keep the old one
intact (similar to how registers are pushed on a stack before a function
call). Control flow constructs are somehow present too, albeit in a very
primitive form requiring a lot of duplication. To keep this article short, I
will not go into details on these. They are readily accessible in Knuth or in
many other resources on the internet (TODO: links)

We are now saying that Charles Babagge is the first creator of the computer
and that Euclid's algorithm for finding the greatest common divisor is the
first algorithm ever. However,is it possible that people in Mesopotamia were
ahead of these? Or it's just extrapolating from the few tablets we have found
and decyphered?

TODO: Haskell code for all of these things
