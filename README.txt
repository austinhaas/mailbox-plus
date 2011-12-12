----------------------------------------------------------------------
Description

This package wraps SBCL's concurrent mailbox implementation [1] and
extends it with a version of receive-message (receive-message-if) that
can selectively receive a message based on a predicate, while keeping
any non-matching messages intact for later receive* calls.

One very important caveat is that, while any thread can send messages
to the mailbox, only one thread can safely use the receive functions,
since we are storing messages skipped over by receive-message-if in a
buffer that is not thread-safe.

The motivation for this code was to create an analog to Erlang's
receiver function, which can selectively match incoming messages
against a pattern while transparently maintaining the original order
of the incoming messages (including those that didn't match) after the
receive function exits. One use-case is to send an asynchronous
request to another process and then block on an incoming message
stream until the reply is received. Any other incoming messages are
ignored in the meantime, but they will be intact for subsequent
operations.

----------------------------------------------------------------------
API

See docstrings in mailbox-plus.lisp and examples in tests.lisp.

----------------------------------------------------------------------
Implementation Notes

The implementation could probably be a lot better, especially if we
were to dig into SBCL's mailbox and queue code and try to include our
extensions at the level of the queue's linked list. Maybe we could
even create a completely thread-safe version that uses the same
optimistic lock-less strategy as the queue. However, that's a lot more
work to implement and I'm not sure there would be much gain for my
current use-case, which only requires a single mailbox reader.

We could easily wrap the body of the receive functions with a mutex to
make the whole thing thread-safe, but that would seriously reduce the
value of using the lock-free mailbox implementation. I have considered
making that optional, though, either via a special variable or
alternative functions (e.g., safe-receive). It might be nice to
provide that interface now, even if the current implementation is
inefficient.

----------------------------------------------------------------------

[1] From the SBCL manual: sb-concurrency:mailbox is a lock-free
message queue where one or multiple ends can send messages to one or
multiple receivers. http://www.sbcl.org/manual/#sb_002dconcurrency
