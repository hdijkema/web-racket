#lang scribble/manual

@(require scribble/manual
          ;scribble/class
          (for-label racket/base
                     racket/class
                     racket/gui/base
		     net/sendurl
                     json
                     ;gregor
                     ;gregor/time
                     net/sendurl
                     racket/path
                     "../private/web-wire.rkt"
                     "../private/webui-wire-download.rkt"
                     "../private/webui-wire-ipc.rkt"
                     "../private/css.rkt"
                     "../private/menu.rkt"
                     "../private/web-racket.rkt"))

@title{High-Level Web Racket GUI API}
@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[web-racket]

The @racket[web-racket] module provides a higher-level GUI layer on top
of the low-level @racketmodname[web-wire] backend. It is intended as the
main entry point for desktop applications that embed a WebUI.

The module exports:

@itemlist[
 @item{OO wrappers for DOM elements and form inputs
       (@racket[ww-element%], @racket[ww-input%]);}
 @item{classes for main windows and dialogs
       (@racket[ww-webview%], @racket[ww-webview-dialog%],
       @racket[ww-webview-message%]);}
 @item{an abstract settings interface (@racket[ww-settings%]);}
 @item{backend control and logging helpers re-exported from
       @racketmodname[web-wire];}
 @item{backend installation/override helpers re-exported from
       @racketmodname[webui-wire-download];}
 @item{CSS and menu construction helpers re-exported from
       @racket["css.rkt"] and @racket["menu.rkt"].}
]

The goal of this layer is:

@itemlist[
 @item{You rarely call @racket[web-wire] directly;}
 @item{you work with windows, elements, and inputs as Racket objects;}
 @item{only when you need fine-grained control do you drop to the
       underlying wire API.}
]

@section{Re-exported backend control and logging}

The following bindings are re-exported directly from
@racketmodname[web-wire].  See that module’s documentation for full
details; here is a short summary for convenience.

@defproc[(ww-start [log-level symbol?]) web-rkt?]{

Start the Web Wire backend if it is not running already, and return the
current backend handle.

Most applications do not call this directly: @racket[ww-webview%]
constructors will automatically call @racket[ww-start] when the first
window is created.
}

@defproc[(ww-stop) void?]{

Stop the Web Wire backend.

This is called automatically when the last window created via
@racket[ww-webview%] is closed; you normally do not need to call it
directly, but you can use it for explicit shutdown.
}

@defproc[(ww-set-debug [on? boolean?]) void?]{

Enable or disable internal debug logging in the @racket[web-wire]
layer. This affects @racket[ww-debug] output.
}

@defform*[[(ww-debug msg-expr) #:contracts ([msg-expr any/c])
           (ww-debug id-expr msg-expr) #:contracts ([id-expr any/c] [msg-expr any/c])
         ]]{
Debug logging macros that write to the shared log buffer when debug is
enabled.
}

@defform*[[(ww-error msg-expr) #:contracts ([msg-expr any/c])
          (ww-error id-expr msg-expr) #:contracts ([id-expr any/c] [msg-expr any/c])
         ]]{
Error logging macros that always log, regardless of the debug setting.
}

@defproc[(ww-display-log
          [filter (or/c #f string? regexp? (listof string?)) #f])
         void?]{

Display the in-memory log buffer on @racket[current-output-port],
optionally filtered.
}

@defproc[(ww-tail-log [args any/c] ...) void?]{

Show the tail of the log buffer and follow new entries (like
@tt{tail -f}). Optional arguments control how many lines to show
initially and which lines to filter.
}

@section{Re-exported WebUI helper command control}

The following bindings are re-exported from
@racketmodname[webui-wire-download]; they are documented in more detail
in that module. Briefly:

@defproc[(ww-set-custom-webui-wire-command!
          [cmd string?])
         string?]{

Override the command used to start the @tt{webui-wire} helper. Useful in
development or when the helper is installed in a non-standard location.
}

@defproc[(ww-get-webui-wire-version) string?]{

Return the version string reported by the installed @tt{webui-wire}
helper.
}

@section{Re-exported CSS and menu helpers}

The module also re-exports:

@itemlist[
 @item{@racket[(all-from-out "css.rkt")] — CSS helper functions and
       @racket[css-style] values;}
 @item{@racket[(all-from-out "menu.rkt")] — menu construction helpers
       for building menu structures passed to @racket[ww-webview%].}
]

See the documentation of those modules for the exact API.

@section{Element wrapper classes}

These classes provide OO wrappers around DOM elements managed by
@tt{webui-wire}. They work on top of the lower-level @racket[web-wire]
functions that address elements by id.

@subsection{Generic element}

@defclass[ww-element% object% ()]{

Base class for element wrappers.

@defconstructor/auto-super[([win-id ww-win?]
                            [id (or/c symbol? string?)])]

The constructor normally isn’t called directly by user code. Objects are
created automatically by @racket[ww-webview%] when you bind inputs and
buttons or when you explicitly request an element.

Fields:

@itemlist[
 @item{@racket[win-id] — a @racket[ww-win] handle identifying the WebUI
       window that owns this element}
 @item{@racket[id] — the element id (symbol or string), as used in the
       DOM.}
]

Public methods:

@defmethod[(get-win-id) ww-win?]{
Return the window handle used for this element.}

@defmethod[(get-id) (or/c symbol? string?)]{
Return the element id.}

@defmethod[(win) (or/c ww-webview% #f)]{
Look up the @racket[ww-webview%] instance for @racket[win-id] in the
global window table. Returns @racket[#f] if the window no longer
exists.}

@defmethod[(callback [evt symbol?] [args any/c] ...) any]{

Call a previously registered callback for @racket[evt] with the
given @racket[args]. This is used internally when DOM events
arrive from @tt{webui-wire}.}

@defmethod[(connect [evt symbol?] [func (-> any)]) void?]{

Register @racket[func] as the handler for event kind @racket[evt] on
this element.}

@defmethod[(disconnect [evt symbol?]) void?]{

Remove any callback registered for @racket[evt].}

@defmethod[(add-style! [st css-style?]) void?]{
Merge the given style into the element’s existing style.}

@defmethod[(set-style! [st css-style?]) void?]{
Replace the element’s style with @racket[st].}

@defmethod[(style) css-style?]{
Return the current style as a @racket[css-style] value.}

@defmethod[(get-attr [a (or/c symbol? string?)]) jsexpr?]{
Get a single attribute value as JSON.}

@defmethod[(set-attr! [a (or/c symbol? string?)] [val any/c]) void?]{
Set an attribute value.}

@defmethod[(del-attr [a (or/c symbol? string?)]) void?]{
Remove an attribute.}

@defmethod[(get-attrs) (hash/c symbol? any/c)]{
Return all attributes as a hash table (symbol → value).}

@defmethod[(add-class! [cl (or/c symbol? string?)]) void?]{
Add a CSS class to the element.}

@defmethod[(remove-class! [cl (or/c symbol? string?)]) void?]{
Remove a CSS class.}

@defmethod[(has-class? [cl (or/c symbol? string?)]) boolean?]{
Test whether the element has a given CSS class.}

@defmethod[(enable) void?]{
Remove the @tt{disabled} state from the element
(by manipulating its CSS/attributes).}

@defmethod[(enabled?) boolean?]{
Return @racket[#t] when the element is not disabled.}

@defmethod[(disable) void?]{
Mark the element as disabled (e.g. by adding a @tt{disabled} attribute
and/or class).}

@defmethod[(disabled?) boolean?]{
Return @racket[#t] when the element is disabled.}

@defmethod[(display [d string? "block"]) void?]{
Set the CSS @tt{display} style; default is @tt{"block"}.}

@defmethod[(hide) void?]{
Convenience for @racket[(display "none")].}

@defmethod[(show) void?]{
Show the element again (typically restoring display to a non-@tt{"none"}
value).}

}

@subsection{Form input element}

@defclass[ww-input% ww-element% ()]{

Wrapper for input-type elements (fields that have a “value” and that
emit change/input events). Instances are usually created by
@racket[ww-webview%] when you call @racket[bind-inputs].

Internally, a @racket[ww-input%] keeps track of:

@itemlist[
 @item{the current value (mirrored from the DOM);}
 @item{an optional change callback.}
]

Public methods:

@defmethod[(get) any/c]{
Return the last known value of the input.}

@defmethod[(on-change! [callback (-> any/c any)]) void?]{

Register a change callback:

@itemlist[
 @item{The callback is stored;}
 @item{It is immediately called once with the current value;}
 @item{Later, when an @tt{input} event with a @tt{value} field arrives
       from the browser, the stored callback is called with the new
       value.}
]
}

@defmethod[(set! [v any/c]) void?]{

Set the input’s value both locally and in the DOM. This will normally
update the form control on screen and may also trigger change/input
events in the browser.
}

@defmethod[(disable) void?]{Disable the input (overrides base behaviour).}

@defmethod[(enable) void?]{Enable the input again (overrides base
behaviour).}
}

@section{Webview windows}

@subsection{Main windows}

@defclass[ww-webview% object% ()]{

Represents a WebUI window driven by the @tt{webui-wire} backend.

@defconstructor/auto-super[
  ([profile symbol? 'default-profile]
   [settings (or/c #f ww-settings%) #f]
   [use-browser boolean? #f]
   [parent-id (or/c #f ww-win?) #f]
   [parent (or/c #f ww-webview%) #f]
   [title string? "Racket HTML Window"]
   [x number?]
   [y number?]
   [width number?]
   [height number?]
   [icon (or/c #f path? string?) #f]
   [menu any/c #f]
   [html-file (or/c #f path? string?) #f])
]{

The numeric geometry defaults (@racket[x], @racket[y], @racket[width],
@racket[height]) are taken either from @racket[settings] or from builtin
defaults.

If @racket[parent] or @racket[parent-id] is provided, a child window
is created and positioned relative to the parent; otherwise a normal
top-level window is created.

If @racket[menu], @racket[icon], or @racket[html-file] are provided,
they are applied after window creation.
}

Important behaviour during construction:

@itemlist[
 @item{when the first window is created, @racket[ww-start] is called;}
 @item{the new window is registered in the global @racket[windows]
       table;}
 @item{an event handler is installed so that incoming events from
       @tt{webui-wire} are dispatched to methods like
       @racket[handle-click] and @racket[handle-change];}
 @item{when the last window is destroyed, @racket[ww-stop] is called.}
]

Key public methods (grouped by responsibility):

@bold{Settings and cloning}

@defmethod[(clone-settings [section symbol?]) (or/c #f ww-settings%)]{

If @racket[settings] is non-@racket[#f], call @racket[clone] on it
to obtain a section-specific settings object, otherwise return
@racket[#f].
}

@bold{Event handling hooks}

These are normally invoked internally when events arrive from
@tt{webui-wire}, but can be overridden in subclasses.

@defmethod[(handle-click [element-id symbol?] [data jsexpr?]) void?]{
Find the element object for @racket[element-id] (if present) and call
its @racket[callback] method with the @racket['click] event.}

@defmethod[(handle-change [element-id symbol?] [data jsexpr?]) void?]{
Find the element object and delegate a @racket['change] event.}

@defmethod[(handle-input [element-id symbol?] [data jsexpr?]) void?]{
Find the element object and delegate an @tt{input} event.}

@defmethod[(handle-navigate [url string?]
                            [type symbol?]
                            [kind symbol?])
           void?]{

Handle navigation events (for example link clicks) as reported by
@tt{webui-wire}.  The default implementation may call
@racket[send-url] depending on @racket[type] and @racket[kind].}

@bold{Element management}

@defmethod[(get-win-id) ww-win?]{
Return the underlying @racket[ww-win] handle.}

@defmethod[(element [id (or/c symbol? string?)]) ww-element%]{

Return (and lazily create) an element wrapper for a given DOM id.
The concrete class is chosen based on the element’s tag and type
(@racket[ww-input%] for input fields, or @racket[ww-element%]
otherwise).
}

@defmethod[(get-elements [selector selector?])
           (listof ww-element%)]{

Query elements matching @racket[selector] via @racket[ww-get-elements]
and return a list of element wrapper objects.
}

@defmethod[(bind [event symbol?]
                 [selector selector?]
                 [forced-cl (or/c #f (is-a?/c ww-element%)) #f])
           void?]{

Bind @racket[event] to all elements matching @racket[selector].

A suitable element class is chosen automatically based on tag/type,
unless @racket[forced-cl] is provided; in that case the given class
is used for all matched elements.
}

@defmethod[(bind-inputs) boolean?]{
Convenience: bind @racket['change] events for @tt{input} and
@tt{textarea} elements and create @racket[ww-input%] wrappers.}

@defmethod[(bind-buttons) void?]{
Convenience: bind @racket['click] events for @tt{button} elements.}

@bold{Window geometry and title}

@defmethod[(move [x number?] [y number?]) void?]{
Move the window to (@racket[x], @racket[y]).}

@defmethod[(resize [x number?] [y number?]) void?]{
Resize the window to width @racket[x], height @racket[y].}

@defmethod[(get-x) number?]{Current x position (cached).}
@defmethod[(get-y) number?]{Current y position (cached).}
@defmethod[(get-width) number?]{Current width (cached).}
@defmethod[(get-height) number?]{Current height (cached).}

@defmethod[(geom) (list number? number? number? number?)]{
Return @racket[(list x y width height)].}

@defmethod[(set-title! [t string?]) void?]{
Set the window title (cached locally and sent to the backend).}

@defmethod[(get-title) string?]{
Return the last title set.}

@bold{Show/hide and lifetime}

@defmethod[(show) void?]{
Show the window (set show state to @racket['show]).}

@defmethod[(hide) void?]{
Hide the window.}

@defmethod[(maximize) void?]{
Maximise the window.}

@defmethod[(normalize) void?]{
Restore the window to its normal state.}

@defmethod[(minimize) void?]{
Minimise the window.}

@defmethod[(fullscreen) void?]{
Request a fullscreen display (if supported).}

@defmethod[(show-state) symbol?]{
Return the current show state as reported by the backend.}

@defmethod[(can-close?) boolean?]{
Return @racket[#t] if the window may be closed; subclasses can
override this to veto close requests.}

@defmethod[(close) void?]{

Close the window, unregister it from the global tables, and if this is
the last window, stop the backend by calling @racket[ww-stop].
}

@bold{Menus}

@defmethod[(set-menu! [menu-def is-menu?]) void?]{
Set the window menu using a structure created with the helpers from
@racket["menu.rkt"].}

@defmethod[(connect-menu! [id symbol?] [cb (-> any)]) void?]{

Associate a callback @racket[cb] with a menu item id. When the menu
item is activated, the callback is called.
}

@defmethod[(popup-menu [menu-def is-menu?]) void?]{
Pops up a contextmenu at the current cursor position.
The menu is created using a structure created with the helpers from
@racket["menu.rkt"].}


@bold{HTML and navigation}

@defmethod[(set-icon! [icn (or/c path? string?)]) void?]{
Set the window icon; the file is validated by @racket[ww-set-icon].}

@defmethod[(set-html-file! [file (or/c path? string?)]) void?]{

Load the given HTML file. The directory part is used to adjust
@racket[ww-cwd], and the file name is passed to
@racket[ww-set-html-file].}
  
@defmethod[(set-html [html string?]) void?]{

Write @racket[html] to a temporary file and call
@racket[set-html-file!] with it. Intended for simple dynamic content.}

@defmethod[(set-url [url string?]) void?]{
Open the given URL using @racket[send-url].}

@defmethod[(html-loaded) void?]{

Hook that is called when a page is fully loaded and matches the
current HTML handle. The default implementation binds buttons and
inputs by calling @racket[bind-buttons] and @racket[bind-inputs].
}

@bold{File and directory dialogs}

@defmethod[(file-open [caption string?]
                      [base-dir string?]
                      [filters string?])
           (or/c #f path?)]{

Show a file-open dialog and return the selected path as a @racket[path],
or @racket[#f] if the operation failed or was cancelled.
}

@defmethod[(file-save [caption string?]
                      [base-dir string?]
                      [filters string?]
                      [overwrite boolean? #f])
           (or/c #f path?)]{

Show a file-save dialog. The @racket[overwrite] flag controls whether
overwriting existing files is allowed. Returns the chosen path, or
@racket[#f] if cancelled or if the command failed.
}

@defmethod[(choose-dir [caption string?]
                       [base-dir string?])
           (or/c #f path?)]{

Show a directory chooser dialog; return the chosen directory, or
@racket[#f] on cancel/failure.
}

@bold{Hook for subclasses}

@defmethod[(inherit-checks) boolean?]{

Called early during construction to allow subclasses to enforce
preconditions (for example: “dialog windows must have a parent”).
The default implementation simply returns @racket[#t].
}
}




@subsection{Dialog windows}

@defclass[ww-webview-dialog% ww-webview% ()]{

Subclass of @racket[ww-webview%] representing dialog-style windows.

The constructor is the same as @racket[ww-webview%], but
@racket[inherit-checks] is overridden to enforce that a @racket[parent]
was supplied:

@racketblock[
  (define/override (inherit-checks)
    (when (eq? parent #f)
      (error "A parent must be given")))
]

So, to create a dialog, always pass a parent window:

@racketblock[
  (define main (new ww-webview%))
  (define dlg  (new ww-webview-dialog% [parent main]))
]
}



@subsection{Message dialogs}

@defclass[ww-webview-message% ww-webview-dialog% ()]{

Simple message-dialog subclass.

The constructor:

@itemlist[
 @item{calls the @racket[ww-webview-dialog%] constructor;}
 @item{sets a minimal HTML page containing a message header and a
       submessage, with known element ids (e.g. @tt{"msg"} and
       @tt{"submsg"}).}
]

You can then obtain element wrappers for those ids via
@racket[send] @racket[this] @racket[element] and update their inner
HTML or text using the usual element methods.
}



@section{Settings abstraction}

@defclass[ww-settings% object% ()]{

Abstract base class for storing and retrieving configuration values
used by @racket[ww-webview%] (for example window geometry).

The default implementation only signals errors; you are expected to
subclass @racket[ww-settings%] and override the methods.

Public methods:

@defmethod[(set [key symbol?] [value any/c]) void?]{
Set a configuration value for @racket[key]. Default implementation
raises an error.}

@defmethod[(get [key symbol?] [default any/c]) any/c]{

Look up a configuration value for @racket[key]. If the key is not
present, return @racket[default] (if provided) or raise an error. The
default implementation always raises an error.
}

@defmethod[(clone [new-section symbol?]) ww-settings%]{

Return a “cloned” settings object for a given section or profile.
Used by @racket[ww-webview%] to derive per-window settings from a
shared base. Default implementation raises an error.
}

@defmethod[(set! [key symbol?] [value any/c]) void?]{

Convenience that forwards to @racket[set]. Provided for symmetry with
Racket’s @tt{set!} naming style.
}


}
