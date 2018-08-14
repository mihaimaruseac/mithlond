---
date: 2018-07-04
title: GDPR
postid: 3
---

This post talks about the steps taken to ensure that this blog passes the
European Union's General Data Protection Regulation (GDPR) law. That is, here
you will find what data I am collecting about each visitor and how am I using
that.

Since this is a static blog, with no comments and no user accounts, the data
about visitors is already in a limited form. Furthermore, I won't ever have
any Javascript or other mechanism of tracking users, with the following two
exceptions:

First, I will have Javascript enabled on some articles to properly present a
demo if the topic requires it. I should make it clearly visible what the demo
is about so a user who views the site with JS disabled can still see what is
missing.

Second, and the relevant part here, I will use Google Analytics to track
views -- I'm deferring to them for the GDPR stuff related to how Google uses
GA to track the users.  From my point of view, the statistics that I get from
GA are only to see which articles are read most and which articles need a
rehash if they are viewed frequently years after they've been posted.

Since this blog is self-hosted on a virtualised environment, I also have
access to the web server's access log. I'm using the data from there for two
things: (auto-)blocking of script kiddies via `fail2ban` and, optionally, more
details about access patterns (i.e., I could see if a potential redirect I
might make in the future when the URL scheme changes is no longer needed).

No other sites will get access to your viewing patterns. This means that I
won't include tracking pixels, adds or share buttons to social media through
which you can be tracked.

If I ever post statistics gathered from GA or web server's logs they would be
aggregated per article, per country or per first 2 IP blocks (e.g.
`192.168.*.*`). Furthermore, each number presented will be presented by adding
[Laplace noise][laplace] with scale $10$. This ensures [Differential
Privacy][dp] with budget $0.1$ per release. Or, in other terms, the reported
counts would be within $\pm 10 \log 10 \approx 23.03$ of the real ones. There
will be a rounding and truncation step of post processing so that the counts
look like counts, not floating point numbers.

In case any of these change, I will update this page accordingly, with a
changelog.

[laplace]: https://en.wikipedia.org/wiki/Laplace_distribution
[dp]: https://en.wikipedia.org/wiki/Differential_Privacy
