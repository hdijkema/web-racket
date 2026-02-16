#lang scribble/manual

@(require scribble/manual
          (for-label racket/base
                     racket/string
                     "../private/web-racket-version.rkt"))

@title{Web Racket Version Information}
@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[web-racket-version]

The @racket[web-racket-version] module centralises version information
for the Web Racket framework and the associated @emph{Web Wire} FFI
layer.  It provides both numeric version components and a
pre-formatted string for each of these version identifiers.

The numeric bindings are useful when you want to compare versions in
code; the string bindings are convenient for display in logs, about
dialogs, and diagnostic output.

Internally, versions are formatted as @tt{"MAJOR.MINOR.PATCH"}.

@section{Web Racket version}

@defthing[ww-version-major exact-nonnegative-integer?]{

The major version number of the Web Racket framework.

This component is incremented for changes that are not backwards
compatible or that represent a significant evolution of the system.
}

@defthing[ww-version-minor exact-nonnegative-integer?]{

The minor version number of the Web Racket framework.

This component is typically incremented for backwards-compatible
feature additions.
}

@defthing[ww-version-patch exact-nonnegative-integer?]{

The patch version number of the Web Racket framework.

This component is incremented for small, backwards-compatible
bug fixes and maintenance releases.
}

@defthing[ww-version string?]{

A human-readable string representation of the Web Racket framework
version, combining @racket[ww-version-major],
@racket[ww-version-minor], and @racket[ww-version-patch] in the form:

@verbatim{"MAJOR.MINOR.PATCH"}

For example, if the numeric components are @racket[0], @racket[1],
and @racket[3], then @racket[ww-version] is @racket["0.1.3"].
}

@section{Web Wire IPC version}

The Web Wire IPC version describes the version of the low-level
interface between Racket and the embedded web UI (for example, a
shared library or external command used to drive the webview).

This version is tracked separately from the main Web Racket framework
version so that protocol or binary compatibility changes can be
managed independently.

@defthing[ww-wire-version-major exact-nonnegative-integer?]{

The major version number of the Web Wire IPC interface.

A change in the major version generally indicates that the IPC 
protocol is not backwards compatible with previous major versions.
}

@defthing[ww-wire-version-minor exact-nonnegative-integer?]{

The minor version number of the Web Wire IPC interface.

This component is typically incremented for backwards-compatible
extensions or enhancements to the IPC protocol.
}

@defthing[ww-wire-version-patch exact-nonnegative-integer?]{

The patch version number of the Web Wire IPC interface.

This is usually incremented for small, backwards-compatible bug
fixes or internal refinements that do not change the protocol
surface.
}

@defthing[ww-wire-version string?]{

A human-readable string representation of the Web Wire IPC version,
combining @racket[ww-wire-version-major],
@racket[ww-wire-version-minor], and @racket[ww-wire-version-patch]
in the form:

@verbatim{"MAJOR.MINOR.PATCH"}

For example, with numeric components @racket[0], @racket[2], and
@racket[8], the resulting version string is @racket["0.2.8"].
}
