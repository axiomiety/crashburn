---
layout: default
title: llog - javascript
category: pages
---

#### E

Always specify a local variable for your iterators - so `for (var i=0; i<x; i++)` and not `for (i=0; i<x; i++)`

#### E

Funkyness using default parameters - don't always work as expected.

#### E

When using `for` loops, it's sometimes more beneficial to iterate through the indexes directly as opposed to using `for (var foo in bar)`. The latter can lead to some funky behaviour.

