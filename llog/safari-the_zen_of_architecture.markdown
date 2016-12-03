---
layout: default
title: llog - safari, the zen of architecture
category: pages
---

Notes from the webinar from Juval Lowy

{% highlight bash %}

Avoid functional decomposition
Bad because
        * never in isolation - A -> B -> C - can't use B in isolation
        * bloating
        * don't put business logic in the client
                * force to do this in the business logic
        * A needs to have enough args for B & C
        * what happens if C blows up?
        * cycolmatic complexity
        * very easy

1st rule of thermodynamics - cannot add value without sweating
        => don't use functional decomposition!

Try anti-design - one team aims for the best design, one for the worst

Domain decomposition is functional decomposition in disguise
In software, waste is hidden - debris is time, energy and career
House analogy - same with software. E.g.
        * change email to SMS
        * switching to cloud storage
        * moving to async (total rewrite often, as implied ordering designed)

More flexibility in your future than in your past
"Decompose based on volatility"
        * identify areas of potential change
        * milestones based on integration not features

Universal, nothing to do with software
Encapsulate change to insulate
Functional decomposition maximises the impact of change

Volatility is not self-evident
Getting management support
Takes longer than functional

Method - axes of volatility
Requirements - solution masquerading as requirement (e.g. cooking -> feeding -> 'take care of well-being of its occupants')

Prepare areas of volatility during requirements phase
What is the mindset required to hunt for areas of volatility?
Use generic terms - like database -> storage, pubsub -> message bus
Anything absent assumed not volatile enough (nothing to encapsulate)

Speculative design trap
Avoid encapsulating changed to the nature of the business (housing a family -> a skyscraper)
Design for your arch-nemesis/competitor - discover what the volatility is (if it's the same way, it's not volatile! Doesn't need encapsulation)
Your cadavers are your past projects - what was wrong is usually evident

Build a vertical slice - better insight
Change architecture to accommodate the slice
Design a project to build a system
Change architecture to accommodate plan


{% endhighlight %}
