#lang scribble/manual

@(require scribble/manual
          (for-label racket/base
                     racket/file
                     racket/path
                     racket/system
                     racket/string
                     racket/port
                     "../private/webui-wire-download.rkt"))

@title{WebUI Wire: Automatic Downloader}
@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[webui-wire-download]

This module centralises how the @tt{webui-wire} helper executable is
invoked and how its version is obtained.

It takes care of:

@itemlist[
  @item{determining the command used to run @tt{webui-wire}
        (platform-specific, Flatpak on Linux, a direct executable on
        other platforms);}
  @item{checking whether the helper is installed and matches the
        expected protocol version (see @racket[ww-wire-version]);}
  @item{optionally allowing user code to override the command string.}
]

The public surface of this module is intentionally small and stable:
most callers only need “get me the command” and “tell me the version”.

@section{Getting the @tt{webui-wire} command}

@defproc[(ww-get-webui-wire-command) string?]{

Returns the command that should be used to invoke the @tt{webui-wire}
helper.

Typical usage is:

@racketblock[
  (define cmd (ww-get-webui-wire-command))
  (define-values (proc in out err) (process cmd))
]

Internally, this function:

@itemlist[
  @item{checks whether a custom command has been installed via
        @racket[ww-set-custom-webui-wire-command!]; if so, that value
        is returned immediately;}
  @item{otherwise, determines a platform-specific default command:
        on Linux this is typically a @tt{flatpak run ...} invocation,
        while on other platforms it is the path to the installed
        executable;}
  @item{ensures that the helper is actually present and executable;}
  @item{verifies that the running helper reports a compatible
        @racket[ww-wire-version] (from @racketmodname[web-racket-version]).}
]

If the helper is not installed, cannot be found, or reports an
unexpected version, an error is raised with a message indicating that
@tt{webui-wire} needs to be installed (or upgraded) in order to use
Web Racket.

The returned string is intended to be passed to @racket[process] or
similar functions from @racketmodname[racket/system].  The result may
contain spaces (for example for Flatpak invocations), so treat it as a
single shell command, not as a list of arguments.
}

@section{Getting the installed version}

@defproc[(ww-get-webui-wire-version) string?]{

Returns the version string of the installed @tt{webui-wire} helper.

Conceptually, the function:

@itemlist[
  @item{obtains the command via @racket[ww-get-webui-wire-command];}
  @item{invokes @tt{<command> --version};}
  @item{reads the stdout of that process;}
  @item{trims whitespace and returns the resulting string.}
]

The returned value is the version as reported by the helper itself,
for example:

@racketblock[
  "0.2.3"
]

In normal operation, this version is expected to match (or be
compatible with) the protocol version @racket[ww-wire-version] that
the current Web Racket library was built for.

If the helper is missing or cannot be executed, the same error
conditions as @racket[ww-get-webui-wire-command] apply, and an
exception is raised.
}

@section{Overriding the command}

@defproc[(ww-set-custom-webui-wire-command!
          [cmd string?])
         string?]{

Installs a custom command string @racket[cmd] that will be used by
@racket[ww-get-webui-wire-command] instead of the automatically
computed, platform-specific default.

This is useful when:

@itemlist[
  @item{@tt{webui-wire} is installed in a non-standard location;}
  @item{you want to run a wrapper script around the real executable;}
  @item{you are developing or testing against a different build of the
        helper.}
]

The command string should be something that can be passed directly to
@racket[process], for example:

@itemlist[
  @item{@racket["/opt/webui-wire/bin/webui-wire"]}
  @item{@racket["flatpak run --user nl.dijkewijk.webui-wire"]}
]

The function stores the override and returns it.

Subsequent calls to @racket[ww-get-webui-wire-command] will use this
custom command as-is; the internal platform detection and default
location logic are bypassed.  The version check performed by
@racket[ww-get-webui-wire-version] still applies, so your custom
binary must speak the expected protocol version.
}

@section{Internal behaviour (informative)}

The details in this section describe the typical internal behaviour of
the module.  They are not part of the public API and may change
without notice, but they are useful to understand how error messages
and version checks arise.

@subsection{Platform and installation lookup}

When no custom command is installed, the module roughly follows these
steps:

@itemlist[
  @item{Determine the host operating system and, if relevant, whether
        it is running under Flatpak or a similar sandbox;}
  @item{Construct a candidate command:
        on Linux this is usually a @tt{flatpak run} incantation;
        on other platforms it is the full path to the installed
        @tt{webui-wire} executable;}
  @item{Attempt to run @tt{<candidate> --version} to confirm that the
        helper is present and working;}
  @item{Compare the reported version with @racket[ww-wire-version]; if
        the versions are incompatible, an error is raised.}
]

The resolved command may be cached internally so that subsequent calls
to @racket[ww-get-webui-wire-command] do not repeat all checks.

@subsection{Error reporting}

When a problem occurs (no helper, wrong version, command not
executable, etc.), errors are raised with human-readable messages that
aim to explain:

@itemlist[
  @item{what went wrong (e.g. @emph{command not found},
        @emph{unsupported version});}
  @item{which command was attempted;}
  @item{which version was expected (via @racket[ww-wire-version]).}
]

Callers of @racket[ww-get-webui-wire-command] and
@racket[ww-get-webui-wire-version] are encouraged to catch these
exceptions near the application entry point and present a friendly
message to the end user (for example: “The WebUI helper needs to be
installed or upgraded”).
}
