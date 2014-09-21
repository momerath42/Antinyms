Notes on 'style' currently used

When I got started with Erlang, I was transpiling game-DSL macros
written in Common Lisp (and previously expanded to CL code) to Erlang.
OTP just made the impedence mismatch worse, so I ignored it until I
decided to start writing new things (and rewriting difficult to debug
things) in Erlang.  At that point, I wrote some things "the OTP way,"
but I found it more awkward than straight-Erlang.

So, while if I were to implement something serious (read: work), I would
take the time to honor the OTP wisdom, for my own experimental hacking,
I prefer the way this code is written, including the process dictionary
abuse.  Which is not to say that it couldn't be refactored and polished,
made more robust, and given test coverage.
