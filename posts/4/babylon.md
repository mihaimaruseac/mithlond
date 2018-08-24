---
date: 2018-07-04
title: Floating Point Numbers First Used in Ancient Babylon
postid: 4
---

Besides Computer Science (and related topics), I also like history. I enjoy
the piece which combine these topics. Such is the case of Knuth's ["Ancient
Babylonian Algorithms"][knuth-1] which I found while reading the collection
titled ["Selected Papers on Computer Science"][knuth-2].

The paper turned out to be interesting because it offered new interesting
insights beside the already known facts about ancient Summer. In fact, the
first page of the article brings a significant piece of information. It is
well known that Babylonians used a base 60 system -- with the caveat that they
didn't have a symbol for 0 at first and later they just used a gap for it.
However, very few people know that floating point arithmetic was actually used
in the land between Tigris and Euphrates.

What does this mean? Reproducing the example from Knuth, the two digit number
$2,20$ (that is, the symbols for 2 and 20 one next to the other) means both $2
\times 60 + 20 = 140$ but also $2 + 20/60 = 2.3333..$. In this peculiar
notation, the exponent of the floating point is not written at all. That is,
the number $2,20$ can represent any number of the form $140 \times 60^n$ where
$n$ is any integer, positive or negative.

This is easily visible from the [YBC 7289][ybc-7289] clay tablet, pictured
below for ease of reference:

![ybc 7289](https://upload.wikimedia.org/wikipedia/commons/0/0b/Ybc7289-bw.jpg)

The image is a square and there are 3 numbers marked on it. One is clearly the
length of the square, $30$. The other two numbers refer to the diagonal, but
which one and with what precision?

For a square of length $30$, the length of the diagonal should be $30\sqrt{2}
= 42.4264\ldots$. The first number on the diagonal, $1,24,51,10$ would
represent in normal base 60 the number $1 \times 60^3 + 24 \times 60^2 + 51
\times 60 + 10 = 305470$. The other number, $42,25,35$ represents $42 \times
60^2 + 25 \times 60 + 35 = 152735$. Both of these are too large to be the
length of the diagonal.

However, if we assume the use of floating points, then the first number is
$305470 \times 60^{n_1}$ and the second is $152735 \times 60^{n_2}$ for some
integers $n_1$ and $n_2$. Picking $n_1 = -3$ we get $1.4142\ldots \approx
\sqrt{2}$ for the first number, thus getting to an approximation for
$\sqrt{2}$ which was also used in the Greek mathematics of later times. Also,
$n_2 = -2$ gives us the length of $42.426388\ldots$ length of the diagonal
that we were seeking.

The next thing to do after having a way to represent numbers is to operate on
them. Addition and subtraction are easy; multiplication and division were done
via tables. In fact, the scribes doing the computations had to learn not only
tables for the common operations but also algorithms for solving various
problems, including quadratic problems, geometric series, square roots, etc.
Careful analyzes of these processes revealed a close relationship between them
and primitive versions of programming languages/algorithms. A quick dive into
these algorithms is the subject of Knuth's paper with which I opened this
article, one that I'd recommend you to read if interested in these things.

[knuth-1]: http://steiner.math.nthu.edu.tw/disk5/js/computer/1.pdf
[knuth-2]: https://www-cs-faculty.stanford.edu/~knuth/cs.html
[ybc-7289]: https://en.wikipedia.org/wiki/YBC_7289
