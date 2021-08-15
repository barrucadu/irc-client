**This project is essentially abandonware!**

I may respond to minor issues, like version bounds which need
changing, but I won't be doing any significant work.

Offer to take over the package if you want any significant changes.

[irc-client][]
===========

An IRC client library.

 - Built on [irc-conduit][].

 - Handles a connection to a single IRC server.

 - Manages "event handlers", calling them as appropriate on receipt of
   messages.

 - Provides default event handlers for some common messages (e.g.,
   server PINGs).

 - Executes each event handler in its own thread, and uses a message
   queue to guarantee thread-safe message delivery.

 - Provides a few helper functions for common operations.

Note
----

This used to be a part of [yukibot][], so if you want the history from
before this was split out into its own library, check there.

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on
libera.chat), or email (mike@barrucadu.co.uk).

[irc-client]:  https://hackage.haskell.org/package/irc-client
[irc-conduit]: https://hackage.haskell.org/package/irc-conduit
[yukibot]:     https://github.com/barrucadu/yukibot
