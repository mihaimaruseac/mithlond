---
date: 2018-07-04
title: About this blog
postid: 1
---

TODO: write as an abstract, reformat, add TOC

TODO: expand on title and tagline

In this article, I want to give some details about this blog, its history and
the technology and design choices behind it. This article will change
every time something about the blog does: be it changing the URL scheme or
just the CSS, that change will be mentioned here. You might consider this to
be a `CHANGELOG` file, a living history of the changes that this blog has
passed through, from the small ones such as just changing the provided
features or the styling of the elements to really important ones such as
changing back-end or completely rewriting components of the system.

But I'm getting ahead of myself, so we should start in a proper order.

# History of my blogging life

I created my first blog when I was 18, on the now defunct Yahoo 360. I didn't
post much at the time, was just experimenting. But those experiments were the
seed for the future.

In 2007, as I started my undergrad, I created a Wordpress blog (not
accessible to the public anymore) in which I mostly posted about stuff
happening in the University. As I wrote more and more and got a solid
readership, I split the blog in two components: the original one was left for
stories about my personal life and for short essays on philosophical questions
about AI, about being an INTJ with many ideas and little time to pursue all of
them, etc. The other blog became the place where I was posting programming
related articles. And, that's how I discovered that Wordpress is not suited
for posting code as all the styles I attempted turned out to not work properly
or require quite a significant amount of work.

Thus, almost 5 years after starting blogging, I decided to shut down those
sites and work on my personal blog engine. As I was already invested into the
Haskell language, I wanted to experiment with [Yesod][yesod]. It took me a
while to get that going, during which time I started maintaining a
[Hakyll][hakyll]-based technical blog for [ROSEdu][rosedu].

Unfortunately, [Techblog][techblog] didn't survive the passing of the years
and now no one writes on it. But it has helped me in finding out that a better
design for my first blog would be to use a static-site approach. Sure, that
meant giving up some of the features I wanted the blog to have, but I realised
it's better to actually first have something and then expand on that; if need
arises in the future, I will always be able to convert the static site to one
using a database backend. And that's how I decided to work on this[^1].

After many months of procrastinating and tweaking design and changing layout,
I can finally launch a version of the site that I like and stop delaying it
anymore. But first, why do I want to blog?

# Motivation

Maybe this should have been the first section in case some readers got bored
by the history above. But, it also fits nicely as the end of the initial part
of this article, so it stays here.

I realised over the years that as I was reading more research papers, more
articles, more books or just experimenting with things and learning, I would
need a place to write this things up and easily reference them later. From
there, it is just a small step before getting to the idea of a public blog,
even if the history mentioned above didn't exist.

Secondly, just a few months ago, one coworker was telling us a story from the
time he was a PhD student: whenever he was going to his advisor with a new
paper claiming that he understood it, the advisor will reply with a simple
question: "Did you code it?" It turns out that consciously transposing an
article into code is a sure way of finding out corner cases and places where
the understanding is not that perfect. Thus, a second reason for this blog is
to make myself a habit of explaining what I just understood and uncover places
I missed.

Next, there is a correlation that I have observed. I used to write on the
Wordpress blog almost every day when I was writing my bachelor's thesis. And I
wrote that thesis fast, with no desire of procrastination, with no time spent
fidgeting over a handful of paragraphs. However, by the time I was required to
write my masters' thesis I had stopped blogging frequently. Unsurprisingly,
writing that thesis went slower. Finally, when I wrote my PhD thesis almost 2
years ago, I was not blogging at all, so writing it took a very long amount of
time. I don't know if I'm reading too much into this, but I feel that there is
a connection between the two so I'd prefer to keep blogging regularly to help
me write proposals and other documents in my future career(s).

To conclude, I am writing this blog first for myself, to have a place where I
dump various information I learn from various sources and to help me
understand that better and improve my writing in general. However, to reach
all these goals, the posts have to have readers and be public. So I shall
strive to make them useful to others too.

It is said that Leonardo da Vinci once wrote "Let no one who is not a
mathematician read the elements of my work". In his works, Knuth refactors
this into "Let everyone who is not a mathematician read my works", as he
attempts to explain difficult concepts to people who don't afford to spend
countless hours understanding the deepest theories in order to use those
concepts. In my case, I will try to have articles both rich in mathematical
content (to the limits of my abilities, I am not a mathematician despite the
nickname I have in smaller circles) and explained in an intuitive way.

And there is one more promise I'm making here. It is possible that I will from
time to time post something about my personal life, career choices, life in
general, etc. But, to honor the subtitle, I will always write at least one
line of code in every article. Hence:

```haskell
main :: IO ()
main = do
  putStrLn "Welcome to my blog"
  postBelow changelog
  post otherPosts
```

# Changelog

## TODO: launching the blog

### Header level 3

#### Header level 4

##### Header level 5

###### Header level 6

TODO: see styling of sections and code

[yesod]: https://www.yesodweb.com/ "Yesod"
[hakyll]: https://jaspervdj.be/hakyll/
[rosedu]: http://www.rosedu.org/
[techblog]: http://techblog.rosedu.org/

[^1]: I actually have a Tumblr blog at the moment but I won't share that as it
  is only used to share some jokes and won't interest many people. I just
  wanted to mention it here to have the full history, not to invite anyone to
  search for it.
