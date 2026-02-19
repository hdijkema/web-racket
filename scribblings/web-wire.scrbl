#lang scribble/manual

@(require scribble/manual
          ;scribble/class
          (for-label racket/base
                     racket/class
                     racket/gui/base
                     json
                     "../private/css.rkt"
                     "../private/menu.rkt"
                     "../private/webui-wire-download.rkt"
                     "../private/webui-wire-ipc.rkt"
                     "../private/web-wire.rkt"))

@title{Web Wire: Low-Level WebUI Command Layer}
@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[web-wire]

The @racket[web-wire] module is the low-level bridge between Racket and
the external @tt{webui-wire} helper process.

On the Racket side it exposes functions like:

@itemlist[
 @item{starting and stopping the backend (@racket[ww-start],
       @racket[ww-stop]);}
 @item{log inspection and debugging helpers;}
 @item{window management (create, move, resize, set title/icon/menu);}
 @item{DOM operations (HTML content, attributes, CSS, classes, values);}
 @item{element queries and event binding;}
 @item{file/directory chooser dialogs.}
]

Most of these functions ultimately send a textual command to
@tt{webui-wire} (via @racket[webui-ipc]) and convert the reply into a
convenient Racket value.

@section{Startup and shutdown}

@defproc[(ww-start [log-level symbol?]) web-rkt?]{

Starts the Web Wire backend (if it is not already running) and returns
the current @racket[web-rkt] handle.

Internally this:

@itemlist[
 @item{creates queues and semaphores for events and logs;}
 @item{starts an IPC connection via @racket[webui-ipc];}
 @item{spawns a background thread that handles incoming events and
       log lines;}
 @item{stores the handle in an internal variable.}
]

If @racket[log-level] is supplied, it is passed to
@racket[ww-log-level] to configure the remote log level.

Calling @racket[ww-start] when the backend is already running is a
no-op: the existing handle is returned.

The backend type is currently always @racket['ipc]; FFI integration is
not implemented yet.
}

@defproc[(ww-stop) void?]{

Stops the Web Wire backend.

This function:

@itemlist[
 @item{notifies registered window event handlers that windows are being
       destroyed;}
 @item{sends an @tt{"exit"}-style command to @tt{webui-wire};}
 @item{stops the event-handling thread;}
 @item{clears the current backend handle.}
]

After @racket[ww-stop], calling window/DOM functions will fail until
@racket[ww-start] is used again.
}

@section{Debugging and log inspection}

These functions control and inspect the logging used by this module and
its event thread.

@defproc[(ww-set-debug [on? boolean?]) void?]{

Enables or disables verbose internal debug logging in
@racket[web-wire].  This affects calls through @racket[ww-debug], but
does not change the remote log level of @tt{webui-wire}; for that, use
@racket[ww-log-level].
}

@defform*[[(ww-debug msg-expr) #:contracts ([msg-expr any/c])
           (ww-debug id-expr msg-expr) #:contracts ([id-expr any/c] [msg-expr any/c])]]{
Debug logging macros used inside the module.

When @racket[ww-set-debug] has been enabled, these macros:

@itemlist[
 @item{format @racket[msg-expr] (and optionally @racket[id-expr]);}
 @item{enqueue a debug log line in the in-memory log buffer.}
]

They are primarily intended for development and diagnostics.
}

@defform*[[(ww-error msg-expr) #:contracts ([msg-expr any/c])
           (ww-error id-expr msg-expr) #:contracts ([id-expr any/c] [msg-expr any/c])]]{
Error logging macros.

These always log, regardless of the debug flag, and are used for
internal error conditions.  Log lines are stored in the same log
buffer used by @racket[ww-display-log] and @racket[ww-tail-log].
}

@defproc[(ww-set-log-lines! [n exact-nonnegative-integer?]) void?]{

Sets the maximum number of log lines kept in the in-memory ring buffer.

The value is clamped between 10 and 10,000.  When the buffer exceeds
this limit, the oldest entries are dropped.
}

@defproc[(ww-display-log
          [filter (or/c #f string? regexp? (listof string?)) #f])
         void?]{

Prints the contents of the log buffer to @racket[current-output-port].

The optional @racket[filter] controls which lines are shown:

@itemlist[
 @item{@racket[#f] — show all entries;}
 @item{@racket[string?] — case-insensitive substring match;}
 @item{@racket[regexp?] — regular expression match;}
 @item{@racket[(listof string?)] — OR-combination of substring filters.}
]
}

@defproc[(ww-tail-log [args any/c] ...) void?]{

Shows the last part of the log buffer and then follows new entries,
similar to @tt{tail -f}.

The optional @racket[args] are interpreted as:

@itemlist[
 @item{a number: how many last lines to show initially;}
 @item{a filter (string, regexp, or list of strings) as in
       @racket[ww-display-log];}
 @item{a boolean @racket[#f] to stop an existing tail session.}
]

This is mainly intended for interactive debugging.
}

@section{Backend settings and protocol info}

@defproc[(ww-log-level [level symbol?]) symbol?]{

Configures the log level inside the @tt{webui-wire} helper.

This function is defined via:

@racketblock[
  (def-cmd ww-log-level
    loglevel () ((level symbol?)) -> symbol)
]

so @racket[level] is optional:

@itemlist[
 @item{with an argument, the remote log level is set to @racket[level]
       and the effective level is returned;}
 @item{without arguments, the current remote log level is queried and
       returned.}
]
}

@defproc[(ww-protocol) exact-integer?]{

Returns the protocol version spoken by the @tt{webui-wire} helper.

Definition (simplified):

@racketblock[
  (def-cmd ww-protocol
    protocol () () -> int)
]

The result is an integer that should match the protocol version that
the Racket side expects.
}

@defproc[(ww-cwd [path path-or-string?]) path?]{

Gets or sets the current working directory used by @tt{webui-wire}.

This function is defined as:

@racketblock[
  (def-cmd ww-cwd
    cwd () [(path path-or-string?)] -> path)
]

Semantics:

@itemlist[
 @item{With a @racket[path], the helper’s working directory is set and
       the new directory (as a path) is returned.}
 @item{Without arguments, the current directory (as seen by the helper)
       is returned.}
]

This directory is used to resolve relative file names (HTML files,
icons, etc.).
}

@section{Global stylesheet}

@defproc[(ww-set-stylesheet [st stylesheet-or-string?]) void?]{

Sets the global stylesheet used by WebUI windows:

@racketblock[
  (def-cmd ww-set-stylesheet
    set-stylesheet ((st stylesheet-or-string?)) () -> void)
]

The argument @racket[st] may be either:

@itemlist[
 @item{a stylesheet value understood by the WebUI side;}
 @item{a raw CSS string.}
]
}

@defproc[(ww-get-stylesheet) stylesheet?]{

Returns the currently configured global stylesheet:

@racketblock[
  (def-cmd ww-get-stylesheet
    get-stylesheet () () -> stylesheet)
]
}

@section{Window handles and window management}

@defstruct*[ww-win ([id exact-integer?])]{

Opaque handle representing a WebUI window.

The internal @racket[id] field corresponds to the numeric window id
used by the @tt{webui-wire} backend.
}

@defproc[(ww-new
          [profile symbol?]
          [use-browser boolean?]
          [parent ww-win?])
         ww-win?]{

Creates a new WebUI window:

@racketblock[
  (def-cmd ww-new
    new ((profile symbol?)) [(use-browser boolean?) (parent ww-win?)]
    -> ww-win)
]

Arguments:

@itemlist[
 @item{@racket[profile] symbolic profile name used by the helper to
       select configuration (e.g. @racket['default]).}
 @item{@racket[use-browser] when true, prefers an external browser
       instead of an embedded webview where supported.}
 @item{@racket[parent] optional parent window (for dialog-like windows).}
]

Returns a @racket[ww-win] handle.
}

@defproc[(ww-close [win-id ww-win?]) void?]{

Closes the given window:

@racketblock[
  (def-cmd ww-close
    close ((win-id ww-win?)) [] -> void)
]
}

@defproc[(ww-move [win-id ww-win?]
                  [x number?]
                  [y number?])
         void?]{

Moves the window to position (@racket[x], @racket[y]) in pixels:

@racketblock[
  (def-cmd ww-move
    move ((win-id ww-win?) (x number?) (y number?)) [] -> void)
]
}

@defproc[(ww-resize [win-id ww-win?]
                    [width number?]
                    [height number?])
         void?]{

Resizes the window to the given pixel @racket[width] and
@racket[height]:

@racketblock[
  (def-cmd ww-resize
    resize ((win-id ww-win?) (width? number?) (height number?)) [] -> void)
]
}

@defproc[(ww-set-title [win-id ww-win?]
                       [title string?])
         void?]{

Sets the window title:

@racketblock[
  (def-cmd ww-set-title
    set-title ((win-id ww-win?) (title string?)) [] -> void)
]
}

@defproc[(ww-set-icon [win-id ww-win?]
                      [svg-file (is-icon-file? 'svg)]
                      [png-file (is-icon-file? 'png)])
         void?]{

Sets the window icon, given paths to an SVG and PNG file:

@racketblock[
  (def-cmd ww-set-icon
    set-icon ((win-id ww-win?)
              (svg-file (is-icon-file? 'svg))
              (png-file (is-icon-file? 'png))) [] -> void)
]

The predicates @racket[(is-icon-file? 'svg)] and
@racket[(is-icon-file? 'png)] ensure that the paths refer to existing
files with the correct file extension.
}

@defproc[(ww-set-menu [win-id ww-win?]
                      [menu is-menu?])
         void?]{

Configures the window menu:

@racketblock[
  (def-cmd ww-set-menu
    set-menu ((win-id ww-win?)
              (menu is-menu?)) [] -> void)
]

The @racket[menu] value is a menu structure as defined in
@racket["menu.rkt"].
}

@defproc[(ww-popup-menu [win-id ww-win?]
                        [menu is-menu?])
         void?]{

Pops up a (context) menu at the current cursor position:

@racketblock[
  (def-cmd ww-popup-menu
    set-menu ((win-id ww-win?)
              (menu is-menu?)) [] -> void)
]

The @racket[menu] value is a menu structure as defined in
@racket["menu.rkt"].
}



@defproc[(ww-set-show-state [win-id ww-win?]
                            [state symbol?])
         void?]{

Sets the show state of the window:

@racketblock[
  (def-cmd ww-set-show-state
    set-show-state ((win-id ww-win?)
                    (state symbol?)) () -> void)
]

Typical states are things like @racket['normal], @racket['maximized],
etc., depending on what the backend supports.
}

@defproc[(ww-get-show-state [win-id ww-win?]) symbol?]{

Returns the current show state of the window:

@racketblock[
  (def-cmd ww-get-show-state
    show-state ((win-id ww-win?)) () -> symbol)
]
}

@section{HTML / URL content}

@defproc[(ww-set-html-file [win-id ww-win?]
                           [html-file html-file-exists?])
         exact-integer?]{

Loads the given HTML file into the window:

@racketblock[
  (def-cmd ww-set-html-file
    set-html ((win-id ww-win?)
              (html-file html-file-exists?)) ()
    -> number)
]

The @racket[html-file] argument must exist (resolved against
@racket[ww-cwd]); otherwise an error is raised.  The return value is a
numeric status code from the helper.
}

@defproc[(ww-set-url [win-id ww-win?]
                     [url string?])
         void?]{

Navigates the window to the given URL:

@racketblock[
  (def-cmd ww-set-url
    set-url ((win-id ww-win?)
             (url string?)) () -> void)
]
}

@defproc[(ww-set-inner-html [win-id ww-win?]
                            [element-id symbol-or-string?]
                            [html-of-file html-or-file?])
         void?]{

Replaces the @tt{innerHTML} of the given element:

@racketblock[
  (def-cmd ww-set-inner-html
    set-inner-html ((win-id ww-win?)
                    (element-id symbol-or-string?)
                    (html-of-file html-or-file?)) () -> void)
]

The @racket[html-of-file] value is either a string containing HTML or
a path to a file whose contents are used as HTML.
}

@defproc[(ww-get-inner-html [win-id ww-win?]
                            [element-id symbol-or-string?])
         jsexpr?]{

Gets the @tt{innerHTML} of the element as JSON:

@racketblock[
  (def-cmd ww-get-inner-html
    get-inner-html ((win-id ww-win?)
                    (element-id symbol-or-string?)) () -> json)
]
}

@section{Attributes, CSS, classes}

@defproc[(ww-set-attr [win-id ww-win?]
                      [element-id symbol-or-string?]
                      [attr symbol-or-string?]
                      [val any?])
         void?]{

Sets an attribute on an element:

@racketblock[
  (def-cmd ww-set-attr
    set-attr ((win-id ww-win?)
              (element-id symbol-or-string?)
              (attr symbol-or-string?)
              (val any?)) () -> void)
]
}

@defproc[(ww-get-attr [win-id ww-win?]
                      [element-id symbol-or-string?]
                      [attr symbol-or-string?])
         jsexpr?]{

Returns the value of a specific attribute as JSON:

@racketblock[
  (def-cmd ww-get-attr
    get-attr ((win-id ww-win?)
              (element-id symbol-or-string?)
              (attr symbol-or-string?)) () -> json)
]
}

@defproc[(ww-get-attrs [win-id ww-win?]
                       [element-id symbol-or-string?])
         jsexpr?]{

Returns all attributes of the element in JSON form, converted on the
Racket side by a helper:

@racketblock[
  (def-cmd ww-get-attrs
    get-attrs ((win-id ww-win?)
               (element-id symbol-or-string?)) () -> json
    -> mk-attrs)
]
}

@defproc[(ww-del-attr [win-id ww-win?]
                      [element-id symbol-or-string?]
                      [attr symbol-or-string?])
         void?]{

Removes an attribute from the element:

@racketblock[
  (def-cmd ww-del-attr
    del-attr ((win-id ww-win?)
              (element-id symbol-or-string?)
              (attr symbol-or-string?)) () -> void)
]
}

@defproc[(ww-add-style [win-id ww-win?]
                       [element-id symbol-or-string?]
                       [css-style css-style?])
         void?]{

Adds or merges CSS style for an element:

@racketblock[
  (def-cmd ww-add-style
    add-style ((win-id ww-win?)
               (element-id symbol-or-string?)
               (css-style css-style?)) () -> void)
]
}

@defproc[(ww-set-style [win-id ww-win?]
                       [element-id symbol-or-string?]
                       [css-style css-style?])
         void?]{

Replaces the CSS style of an element:

@racketblock[
  (def-cmd ww-set-style
    set-style ((win-id ww-win?)
               (element-id symbol-or-string?)
               (css-style css-style?)) () -> void)
]
}

@defproc[(ww-get-style [win-id ww-win?]
                       [element-id symbol-or-string?])
         css-style?]{

Gets the CSS style of an element:

@racketblock[
  (def-cmd ww-get-style
    get-style ((win-id ww-win?)
               (element-id symbol-or-string?)) ()
    -> css-style)
]
}

@defproc[(ww-add-class [win-id ww-win?]
                       [element-id symbol-or-string?]
                       [class symbol-or-string?])
         void?]{

Adds a CSS class:

@racketblock[
  (def-cmd ww-add-class
    add-class ((win-id ww-win?)
               (element-id symbol-or-string?)
               (class symbol-or-string?))
    () -> void)
]
}

@defproc[(ww-remove-class [win-id ww-win?]
                          [element-id symbol-or-string?]
                          [class symbol-or-string?])
         void?]{

Removes a CSS class:

@racketblock[
  (def-cmd ww-remove-class
    remove-class ((win-id ww-win?)
                  (element-id symbol-or-string?)
                  (class symbol-or-string?))
    () -> void)
]
}

@defproc[(ww-has-class? [win-id ww-win?]
                        [element-id symbol-or-string?]
                        [class symbol-or-string?])
         boolean?]{

Tests for a CSS class:

@racketblock[
  (def-cmd ww-has-class?
    has-class ((win-id ww-win?)
               (element-id symbol-or-string?)
               (class symbol-or-string?)) ()
    -> bool)
]
}

@section{Values and element queries}

@defproc[(ww-get-value [win-id ww-win?]
                       [element-id symbol-or-string?])
         string?]{

Gets the “value” of an element:

@racketblock[
  (def-cmd ww-get-value
    value ((win-id ww-win?)
           (element-id symbol-or-string?)) () -> string)
]
}

@defproc[(ww-set-value [win-id ww-win?]
                       [element-id symbol-or-string?]
                       [value any?])
         void?]{

Sets the “value” of an element:

@racketblock[
  (def-cmd ww-set-value
    value ((win-id ww-win?)
           (element-id symbol-or-string?)
           (value any?)) () -> void)
]
}

@defproc[(ww-get-elements [win-id ww-win?]
                          [selector selector?])
         jsexpr?]{

Queries elements matching a CSS-like selector:

@racketblock[
  (def-cmd ww-get-elements
    get-elements ((win-id ww-win?)
                  (selector selector?)) () -> json
    -> (λ (r) ...))
]

The raw JSON result is converted by a small helper into a more usable
Racket structure (see the implementation).
}

@defproc[(ww-element-info [win-id ww-win?]
                          [element-id symbol-or-string?])
         jsexpr?]{

Returns structural information about an element:

@racketblock[
  (def-cmd ww-element-info
    element-info ((win-id ww-win?)
                  (element-id symbol-or-string?)) ()
    -> json
    -> (λ (r) ...))
]
}

@section{Events}

@defproc[(ww-bind [win-id ww-win?]
                  [event symbol-or-string?]
                  [selector selector?])
         jsexpr?]{

Binds an @racket[event] to all elements matching @racket[selector]:

@racketblock[
  (def-cmd ww-bind
    bind ((win-id ww-win?)
          (event symbol-or-string?)
          (selector selector?)) () -> json
    -> (λ (r) ...))
]

The JSON result includes information about the bound elements.
}

@defproc[(ww-on [win-id ww-win?]
                [event symbol-or-string?]
                [id symbol-or-string?])
         void?]{

Binds @racket[event] for a single element id:

@racketblock[
  (def-cmd ww-on
    on ((win-id ww-win?)
        (event symbol-or-string?)
        (id symbol-or-string?)) () -> void)
]
}

@section{File and directory dialogs}

@defproc[(ww-file-open [win-id ww-win?]
                       [caption string?]
                       [dir string?]
                       [file-filters string?])
         string?]{

Opens a file-open dialog:

@racketblock[
  (def-cmd ww-file-open
    file-open ((win-id ww-win?)
               (caption string?)
               (dir string?)
               (file-filters string?)) ()
    -> string)
]

Returns the selected path as a string, or an empty string on cancel.
}

@defproc[(ww-file-save [win-id ww-win?]
                       [caption string?]
                       [dir string?]
                       [file-filters string?]
                       [overwrite boolean?])
         string?]{

Opens a file-save dialog:

@racketblock[
  (def-cmd ww-file-save
    file-save ((win-id ww-win?)
               (caption string?)
               (dir string?)
               (file-filters string?)
               (overwrite boolean?)) ()
    -> string)
]

Returns the chosen file path as a string, or an empty string if the
user cancels.
}

@defproc[(ww-choose-dir [win-id ww-win?]
                        [caption string?]
                        [dir string?])
         string?]{

Opens a directory chooser dialog:

@racketblock[
  (def-cmd ww-choose-dir
    choose-dir ((win-id ww-win?)
                (caption string?)
                (dir string?)) ()
    -> string)
]

Returns the selected directory path as a string, or an empty string
on cancel.
}

@section{Low-level command access}

@defproc[(ww-cmd [cmd string?]) cmdr?]{

Sends a raw command string @racket[cmd] to @tt{webui-wire} and returns
a @racket[cmdr] struct describing the reply.

This is the lowest-level escape hatch; all @racket[def-cmd]-generated
functions use this under the hood.
}

@defproc[(ww-cmd-nok? [r any/c]) boolean?]{

Predicate that recognises a failed command reply:

@itemlist[
 @item{returns @racket[#t] if @racket[r] is a @racket[cmdr] with a
       false @tt{ok} field, or the symbol @racket['cmd-nok];}
 @item{returns @racket[#f] otherwise.}
]
}

@defproc[(ww-from-string [s string?]) string?]{

Helper that “unquotes” and unescapes a string previously encoded for
transmission over the wire:

@itemlist[
 @item{removes surrounding quotes;}
 @item{replaces escaped quotes (@tt{\"\\\"\"}) by plain quotes.}
]
}

@section{Window tables}

@defthing[windows (hash/c exact-integer? ww-win?)]{

Hash table containing the currently known windows, keyed by numeric id.

This is mainly useful for diagnostics and advanced introspection; most
application code keeps explicit @racket[ww-win] handles instead.
}

@defthing[windows-evt-handlers
          (hash/c exact-integer? (-> symbol? any/c any))]{

Hash table mapping window ids to their registered event handlers.
Handlers are called with an event kind (a symbol) and associated data.
}

@defproc[(ww-get-window-for-id [win-id exact-integer?])
         (or/c ww-win? #f)]{

Looks up the @racket[ww-win] struct for a numeric @racket[win-id] in
the @racket[windows] table.  Returns @racket[#f] if no such window is
known.
}

@section{Helper predicates used in contracts}

The following predicates are used as argument checkers in the
@racket[def-cmd]-generated functions above.

@defproc[(stylesheet-or-string? [st any/c]) boolean?]{

Returns @racket[#t] if @racket[st] is either a stylesheet value or a
string:

@racketblock[
  (define (stylesheet-or-string? st)
    (or (stylesheet? st) (string? st)))
]
}

@defproc[(is-icon-file? [ext symbol?]) (-> any/c boolean?)]{

Returns a predicate that checks “existing file with the right
extension”:

@racketblock[
  (define (is-icon-file? ext)
    (lambda (v)
      (and (string? v)
           (string-suffix? v (string-append "." (symbol->string ext)))
           (file-exists? v))))
]

Used in @racket[ww-set-icon] with @racket['svg] and @racket['png].
}

@defproc[(html-file-exists? [f path-or-string?]) boolean?]{

Checks whether @racket[f] refers to an existing file, either directly
or resolved relative to @racket[ww-cwd]:

@racketblock[
  (define (html-file-exists? f)
    (if (file-exists? f)
        #t
        (let* ((cwd (ww-cwd))
               (full-file (build-path cwd f)))
          (ww-debug (format "file-exists? '~a'" full-file))
          (file-exists? full-file)))
    )
]
}

@defproc[(html-or-file? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is either an existing file or a
string:

@racketblock[
  (define (html-or-file? v)
    (if (file-exists? v)
        #t
        (string? v)))
]
}

@defproc[(symbol-or-string? [s any/c]) boolean?]{

True if @racket[s] is a symbol or a string:

@racketblock[
  (define (symbol-or-string? s)
    (or (symbol? s) (string? s)))
]
}

@defproc[(any? [v any/c]) boolean?]{

Predicate that always returns @racket[#t]; used in contracts when no
extra checking is required.
}

@defproc[(path-or-string? [s any/c]) boolean?]{

True if @racket[s] is a path or string:

@racketblock[
  (define (path-or-string? s)
    (or (path? s) (string? s)))
]
}

@defproc[(selector? [s any/c]) boolean?]{

Predicate for element selectors:

@itemlist[
 @item{@racket[symbol?] — a single symbolic selector;}
 @item{@racket[string?] — a string selector;}
 @item{a non-empty list of symbols/strings.}
]

Implementation sketch:

@racketblock[
  (define (selector? s)
    (or (symbol? s) (string? s)
        (and (list? s)
             (not (null? s))
             (letrec ([f (λ (l)
                           (if (null? l)
                               #t
                               (and (or (symbol? (car l))
                                        (string? (car l)))
                                    (f (cdr l)))))]
               (f s)))))
    )
]
}
