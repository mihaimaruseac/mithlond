---
date: 2018-01-15
title: Another post from the past
postid: -2
---

# Description

This is a **test** to see the layout of blog __article__ contents.

This should be a second *paragraph* of the _article_, in English.

## More details

This is a ~~paragraph~~ inside a level 2 header.

### One header with attributes {#foo .class-en key-lang=en}

Here we test both a level 3 header as well as the possibility of using
attributes for headers.

#### Level 4 header {-}

Using `.unnumbered` or just a `-` as an attribute should not number the header.

Here we should get a [link to a previous section][One header with attributes].

# Quotes

> This is a block quote.
> The quoted paragraph has two lines.
>
> > This is an embedded quote.

# Code

Simple code:

```
main = putStrLn "This should work"
```

Code with attributes:

``` {#maincode .haskell .numberLines startFrom=5}
data X = A | B | C
```

You can also use `line-anchors`. If you only need one class and no other
attributes, that can be specified immediately when opening the code block.

# Line blocks

This preserves division of text into lines.

| This is the first line,
| This is the second line;
|     Now here's some indented line.

# Lists

* one
* two
* three

<!-- end of list -->

- A paragraph as a list element, with longer text lines and proper formatting
  and line wraps

- The second paragraph of the list

<!-- end of list -->

1. one
1. two
1. three

<!-- end of list -->

#. one with default marker
#. two
#. three

<!-- end of list -->

5. list starting from 5

## Definition lists

Term 1

: This is the definition of term 1

Term 2

: This is the definition of term 2

<!-- end of list -->

Compact term 1
    ~ Definition for term 1

# Rules and tables

Some paragraph first

* * *

A paragraph after a horizontal rule.

---------

Another horizontal rule just passed. Here's a table, check caption and column
alignment based on position of column title relative to the underline markers.

  Right     Left     Center     Default
-------     ------ ----------   -------
     12     12        12            12
    123     123       123          123
      1     1          1             1

Table:  Demonstration of simple table syntax.

Another table, with no column names. We must end it with lines now, a blank
line like above doesn't work.

-------     ------ ----------   -------
     12     12        12             12
    123     123       123           123
      1     1          1              1
-------     ------ ----------   -------

Now, a table with multiline cells:

-------------------------------------------------------------
 Centered   Default           Right Left
  Header    Aligned         Aligned Aligned
----------- ------- --------------- -------------------------
   First    row                12.0 Example of a row that
                                    spans multiple lines.

  Second    row                 5.0 Here's another one. Note
                                    the blank line between
                                    rows.
-------------------------------------------------------------

Table: Here's the caption. It, too, may span
multiple lines.

Note that this must always end with lines and with a blank line.

: Sample grid table.

+---------------+---------------+--------------------+
| Fruit         | Price         | Advantages         |
+===============+===============+====================+
| Bananas       | $1.34         | - built-in wrapper |
|               |               | - bright color     |
+---------------+---------------+--------------------+
| Oranges       | $2.10         | - cures scurvy     |
|               |               | - tasty            |
+---------------+---------------+--------------------+

| Right | Left | Default | Center |
|------:|:-----|---------|:------:|
|   12  |  12  |    12   |    12  |
|  123  |  123 |   123   |   123  |
|    1  |    1 |     1   |     1  |

  : Demonstration of pipe table syntax.


# Other inline formatting

Text with subscripts such as H~2~O. And now text with superscripts such as
2^10^.

Also note the [Small caps]{.smallcaps} possibility.

# Math

Inline math with dollars: $1 + 2 = 3$.

Math blocks?

Extra

\newcommand{\tuple}[1]{\langle #1 \rangle}

$\tuple{a, b, c}$

Some more display math:

\\[
1 + 2 = \frac{3}{1} + \sqrt{\sin{\pi}}
\\]

# Other links

We can have <http://google.com> or <sam@green.eggs.ham>.

Also [Write me!](mailto:sam@green.eggs.ham) and [inline link](/url), and
here's [one with a title](http://fsf.org "click here for a good time!").

# Images

![la lune](lalune.jpg "Voyage to the moon")

# Others

Here is a footnote reference,[^1] and another.[^longnote]

[^1]: Here is the footnote.

[^longnote]: Here's one with multiple blocks.

    Subsequent paragraphs are indented to show that they
belong to the previous footnote.

        { some.code }

    The whole paragraph can be indented, or just the first
    line.  In this way, multi-paragraph footnotes work like
    multi-paragraph list items.

This paragraph won't be part of the note, because it
isn't indented.
