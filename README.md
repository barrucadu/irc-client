# irc-client: IRC client library

 - Built on [irc-conduit][].

 - Handles a connection to a single IRC server.

 - Manages "event handlers", calling them as appropriate on receipt of
   messages.

 - Provides default event handlers for some common messages (e.g.,
   server PINGs).

 - Executes each event handler in its own thread, and uses a message
   queue to guarantee thread-safe message delivery.

 - Provides a few helper functions for common operations.

Note: this used to be a part of [yukibot][], so if you want the
history from before this was split out into its own library, check
there.

[irc-conduit]: https://hackage.haskell.org/package/irc-conduit
[yukibot]:     https://github.com/barrucadu/yukibot
