---
date: 2018-07-04
title: About this blog
postid: 1
---

The purpose of this article is twofold. First, I want to give a reason for
starting a new blog and document what type of writings will be here. This
includes a short history of my previous blogs, but I'll try to not get to
great lengths there.

The second part of this blog post is a self-referential documentation of the
changes done to this blog between its releases, as I suspect there will be
many in the future.

So, let's begin with the first part.

# Background

I created my first blog when I was 18, on the now defunct Yahoo 360; I didn't
post much at the time, was just experimenting. The real blog, the one where I
posted the most, is the Wordpress one which I started in 2007 at the same time
as I was going to college for my undergrad. As the number of articles there
increased and diversified, I split it into two components: the splinter blog
was only for coding and programming articles and the main one for personal
life and short essays about politics, AI, research. None of these blogs are
publicly available now, however, I might resurface some of their articles at a
later time.

There were several issues with Wordpress, the main one being the lack of
control on posting code samples as I wanted them to look like. This and some
other changes made me shut down the sites mentioned in the paragraph above and
start experimenting with creating my personal blog engine. As I was already
invested into the Haskell language, I wanted to experiment with
[Yesod][yesod]. It took me a while to get that going, during which time I
started maintaining a [Hakyll][hakyll]-based technical blog for
[ROSEdu][rosedu].

Unfortunately, [Techblog][techblog] didn't survive the passing of the years
and now no one writes on it. But it has helped me in finding out that a better
design for my first blog would be to use a static-site approach. Sure, that
meant giving up some of the features I wanted the blog to have, but I realised
it's better to actually first have something and then expand on that; if need
arises in the future, I will always be able to convert the static site to one
using a database backend.

After many months of procrastinating and tweaking design and changing layout,
I can finally launch a version of the site that I like and stop delaying it
anymore. But first, why do I want to blog?

# Motivation

One thing that comes to mind is that it was way easier for me to write
technical documents and papers (i.e., bachelor's, master's and PhD theses and
research articles) when I was writing on a blog than otherwise. Since I
imagine that I will write more research articles in the future, having a blog
can only help.

Furthermore, I realised over the years that as I was reading more technical
books and research papers or just other blog articles or experimenting with
things and learning, I would need a place to write results and conclusions so
that I could reference them later. From there, it is just a small step before
getting to the idea of a public blog, where this information would be
accessible to all.

Finally, just a few months ago, one coworker was telling us a story from the
time he was a PhD student: whenever he was going to his advisor with a new
paper claiming that he understood it, the advisor will reply with a simple
question: "Did you code it?" It turns out that consciously transposing an
article into code is a sure way of finding out corner cases and places where
the understanding is not that perfect. Thus, another reason for this blog is
to make myself a habit of explaining what I just understood and uncover places
I missed.

Thus, most of the reasons behind this blog are personal: just me wanting a
help to better analyze and research topics in computer science and
other/related fields. This brings to light the subject of the topic of the
articles of this blog.

# Topics covered

As mentioned before, one of the main source of subjects for the articles
posted here would be papers and technical books that I'm reading. As I
understand them or find interesting exercises to solve, I will write small
articles here. And, speaking of exercises, I will also post articles about
interesting puzzles and problems that I solve using programming.

Since one of my interests is in data science, I might also post articles where
I use machine learning algorithms to extract information from some datasets.
Probably, some of the articles will just be presenting the datasets -- at the
moment I have at least 3 datasets of information that I have collected over
the past few years and I'd like to mine.

Furthermore, another source of articles will be me explaining the topics of
differential privacy and/or Haskell programming. Of course, there are other
blogs touching these subjects and it is very possible that I'll expand on
other fields in the future too. But I wanted it mentioned here that I will
turn Leonardo da Vinci's motto -- he once wrote "Let no one who is not a
mathematician read the elements of my work" -- upside down: while the articles
won't shy away from math, I will try to explain the concept as much as
possible, making them very intuitive.

Finally, there is another anecdote that provides a new category of articles.
One Haskell programmer once said that he is able to immediately answer
questions and solve bugs related to his packages because he keeps a journal of
all the mistakes he had made and always references that when a new question
arises.

Of course, this is a personal blog so I might from time to time also include
non-technical articles in here, or just drop some hints. I will try to
minimize this but I make no promise. Nor do I make a promise that all articles
will include code, math and/or interactive demos. I will try to keep articles
short, but no promise there either.

This covered, it's time to move to the second subject of this article: a
changelog of sorts of different versions of this blog.

# Changelog

Each subsection here will include changes between two different releases of
the blog. A new release is given by changes to the CSS, URL scheme or some
extra features that I have planned and will add over time as I explore the
power of the [Hakyll][hakyll] ecosystem.

## TODO: Name of first version

The first version of the blog has just the bare minimum. TODO: description

[yesod]: https://www.yesodweb.com/ "Yesod"
[hakyll]: https://jaspervdj.be/hakyll/
[rosedu]: http://www.rosedu.org/
[techblog]: http://techblog.rosedu.org/
