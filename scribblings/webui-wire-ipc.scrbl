#lang scribble/manual

@(require scribble/manual
          (for-label racket/base
                     racket/system
                     racket/string
                     "../private/webui-wire-download.rkt"
                     "../private/webui-wire-ipc.rkt")
          )

@title{WebUI Wire IPC Bridge}
@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[webui-wire-ipc]

This module provides a small IPC abstraction around the external
@tt{webui-wire} executable.  It is responsible for:

@itemlist[
  @item{starting the @tt{webui-wire} process (via
        @racket[ww-get-webui-wire-command]);}
  @item{reading and decoding its stdout and stderr streams;}
  @item{dispatching incoming events to a callback;}
  @item{serialising access to the process so it can be shared by
        multiple threads.}
]

The public API consists of a single function, @racket[webui-ipc], which
starts the helper process and returns a function for sending commands
and receiving replies.

The helper process itself implements a simple text-based protocol; this
module hides those details and presents a synchronous “send a string,
get a string back” interface.

@section{IPC entry point}

@defproc[
 (webui-ipc
  [event-queuer (-> string? any)]
  [log-processor (-> symbol? string? any)])
 (-> string? string?)
]{

Starts the @tt{webui-wire} executable and returns a function that can be
used to send commands to it.

The arguments:

@itemlist[
  @item[@racket[event-queuer]]{
    A callback that receives event payloads produced by the helper
    process.

    Whenever the process emits an @tt{EVENT} line on stderr, the
    textual payload after the @tt{"EVENT:"} tag is passed to this
    callback as:

    @racketblock[
      (event-queuer line)
    ]

    The callback is executed in the context of a dedicated reader
    thread and should return quickly (for example by placing the event
    on a queue to be handled elsewhere).
  }
  @item[@racket[log-processor]]{
    A callback used for all non-event messages and error conditions.

    It is called as:

    @racketblock[
      (log-processor kind msg)
    ]

    where @racket[kind] is a symbol describing the source or category
    of the message (for example @racket['stderr-reader] or a tag from
    the incoming line) and @racket[msg] is a human-readable string.
  }
]

The return value is a @emph{command function} with the contract
@racket[(-> string? string?)]:

@itemlist[
  @item{When you call it with a command string, it sends that command
        to the @tt{webui-wire} process and blocks until one reply is
        received.}
  @item{The reply payload (decoded as UTF-8) is returned as a plain
        string.}
  @item{If anything goes wrong with the IPC protocol (invalid prefix,
        unexpected EOF, etc.), an exception is raised.}
]

A typical usage pattern:

@racketblock[
  (define (enqueue-event line)
    ;; Handle incoming EVENT messages from webui-wire
    (printf "EVENT: ~a\n" line))

  (define (log-message kind msg)
    ;; Central logging hook
    (printf "[~a] ~a\n" kind msg))

  ;; Start the IPC bridge and obtain the command function:
  (define send-cmd
    (webui-ipc enqueue-event log-message))

  ;; Send a command (no newline required) and wait for the reply:
  (define reply
    (send-cmd "HELLO 42"))

  (printf "Reply was: ~a\n" reply)
]

Notes:

@itemlist[
  @item{The command function writes a line to the process stdin using
        @racket[displayln]; callers do @emph{not} need to append a
        newline themselves.}
  @item{Calls to the command function are executed one at a time, even
        when invoked from multiple threads (see
        @secref["ipc-internal-concurrency"]).}
]

}

@section[#:tag "ipc-protocol"]{Protocol overview (informative)}

The @tt{webui-wire} process uses a simple length-prefixed text protocol
on both stdout and stderr.  This section describes the format for
reference; the details are handled internally by this module.

@subsection{Message framing}

Each message from the helper has the following structure:

@itemlist[
  @item{An 8-character decimal length prefix (ASCII digits),}
  @item{followed by a single colon character @tt{":"},}
  @item{followed by @emph{exactly} that many bytes of payload,}
  @item{followed by a single newline character.}
]

The payload is treated as UTF-8 text by this module.

@subsection{Stdout replies}

On @bold{stdout}:

@itemlist[
  @item{Each command sent via the returned command function results in
        exactly one reply on stdout with the framing described above.}
  @item{The payload is decoded as UTF-8 and returned to the caller as a
        plain string.}
]

If the helper sends malformed output (for example, a non-numeric
prefix, incorrect length, or missing colon), the IPC code raises an
error indicating “unexpected input from webui-wire executable”.

@subsection{Stderr events and logs}

On @bold{stderr}:

@itemlist[
  @item{Each payload is interpreted as a single logical line;}
  @item{The line is expected to start with a symbolic tag followed by
        a colon, e.g. @tt{"EVENT:..."} or @tt{"INFO:..."}.}
]

The module distinguishes:

@itemlist[
  @item{@tt{EVENT} lines: the text after the @tt{"EVENT:"} prefix is
        delivered to the @racket[event-queuer] callback.}
  @item{All other tags (and untagged lines): turned into
        @racket[(kind msg)] calls to @racket[log-processor], where
        @racket[kind] is a symbol derived from the tag or from the
        stderr reader, and @racket[msg] is the remaining text.}
]

If the stderr reader encounters EOF, it reports this via
@racket[log-processor] (using an appropriate @racket[kind] symbol) and
then terminates its reader thread.

@section[#:tag "ipc-internal"]{Internal behaviour (informative)}

This section summarises how @racket[webui-ipc] is implemented.  The
details are not part of the public API, but they can help to interpret
log messages and errors.

@subsection{Process startup}

When you call @racket[webui-ipc], the following steps are performed:

@itemlist[
  @item{Obtain the command string for @tt{webui-wire} by calling
        @racket[ww-get-webui-wire-command] from
        @racketmodname[webui-wire-download].}
  @item{Launch the helper process using @racket[process] from
        @racketmodname[racket/system].}
  @item{Keep handles to the process stdin, stdout, and stderr ports.}
  @item{Spawn a background thread dedicated to reading and decoding
        stderr events and log lines.}
]

If the process cannot be started, an exception is raised immediately
from @racket[webui-ipc].

@subsection[#:tag "ipc-internal-concurrency"]{Concurrency and serialisation}

The command function returned by @racket[webui-ipc] may be called from
multiple threads.

Internally, a semaphore is used to ensure that only one caller at a
time:

@itemlist[
  @item{writes a command line to the process stdin,}
  @item{waits for a single, complete reply on stdout,}
  @item{returns that reply to the caller.}
]

This guarantees that requests and replies do not interleave: the reply
string you receive always corresponds to the command you sent in that
call.

The stderr reader runs independently in its own thread and does not
block command calls.

@subsection{Error handling and shutdown}

Several error conditions can occur:

@itemlist[
  @item{EOF or broken pipe on stdin/stdout/stderr;}
  @item{malformed length prefix or framing errors;}
  @item{helper process exiting unexpectedly.}
]

In these cases, the module:

@itemlist[
  @item{raises an exception from the command function on the next call
        that attempts to send a command or read a reply;}
  @item{reports context via @racket[log-processor] (for example, that
        the stderr reader hit EOF or that the executable exited).}
]

It is the responsibility of the caller to decide how to react:
typically by catching such exceptions at a higher level, informing the
user, and possibly restarting the application or IPC bridge.

Once the @tt{webui-wire} process has exited, the previously returned
command function is no longer usable; further calls are expected to
fail with an error.
