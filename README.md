# iso8601-timestamp
## Implementation of ISO 8601-1:2019 and ISO 8601-2:2019

This library is an implementation of the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) standard for dates and times. 
It attempts to implement the standard as correctly as possible, while being extendable
and reasonably straightforward to use. Unfortunately, parts of the standard are still quite unclear, so there may be unforseen mistakes.

It has been a good learning exercise. Certain parts of the code can doubtless be 
significantly improved in their efficiency, documentation and readability.

This library is largely built using [regex-applicative](https://hackage.haskell.org/package/regex-applicative), 
which is a reasonable parsing library. As best as I can tell, this is pretty fast for this particular purpose,
and it certainly is very straightforward and intuitive to use. I very much enjoyed learning to use it.

Additionally, there is a test suite. This uses quickCheck2 + hspec and tests all of the datatypes exported.
The tests are quite basic. They generate a given value, serialise it, then attempt to read it back, and compare 
the result. At the moment, everything passes, which is great!

However, it should be noted that section 5 of ISO 8601-2, the rules for recurrence of dates and times, are unimplemented. 
I hope to implement this at a later date but didn't, because it's technically part of the [iCalendar](https://tools.ietf.org/html/rfc5545)
standard, section 3.3.10 ('Recurrence rule').

It should be noted that [time-1.9.3](https://hackage.haskell.org/package/time-1.9.3) is a dependency; this is because it includes a few
improvements designed for implementing ISO 8601:2004. Unfortunately, it is nowhere near complete in this respect. Hopefully it will 
become more complete. The naming of these modules does not conflict with it.

To get started, simply install using [Stack](https://docs.haskellstack.org/en/stable/README/):

```
stack install .
```
