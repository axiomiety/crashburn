---
layout: default
title: llog - j
category: pages
---

#### E

z=: '[a,b]'
(>: z i. ',') { z

taking z, extract the element at the index defined by (>: z i. ',')
>: z i. ','

z i. ',' NB. gets the index of the first occurrence of ',' in z
>: z i. ',' increments that index by 1 (so b)

#### E

>. - monadyic is ceiling, dyadic is larger of

running maximum:

      >./\ 7 8 5 9 2
    7 8 8 9 9

can't explain that just yet...

#### E
(x u\ y)

       3 <\ 'abcdefg'
    +---+---+---+---+---+
    |abc|bcd|cde|def|efg|
    +---+---+---+---+---+

if x is negative, it's non-overlapping (and as such the last infix may be short)

       x=: 2               NB. NOT negated because you want *overlapping* pairs
       x +/\ 3 1 4 1 5 9   NB. Sum successive pairings: (3 1), (1 4), (4 1), etc.
    4 5 5 6 14

-- do the same for exclusive pairs!

#### E

       #@> 'Newton';'Einstein'
    6 8

#### E

What about & for argument blocking?

