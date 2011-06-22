/*  File:    autocomplete.pl
    Author:  Jan Wielemaker,,,
    Created: Jun 22 2011
    Purpose: Implement autocompletion for PceEmacs
*/

:- module(emacs_autocomplete, []).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(library(hyper)).

:- debug(autocomplete).

:- pce_global(@emacs_completion_browser,
	      new(autocomplete_browser)).

:- pce_begin_class(autocomplete_browser, frame,
		   "Browser to show and select autocomplete results").

initialise(F) :->
	send_super(F, initialise, 'Autocomplete', popup,
		   application := @emacs),
	send(F, append, new(B, emacs_autocomplete_browser)),
	send(B, name, browser).

editor(F, E:editor) :<-
	"Find the editor I'm associated to"::
	get(F, hypered, editor, E).

members(F, Members:chain) :->
	"Fill browser with members"::
	get(F, member, browser, B),
	send(B, members, Members).

:- pce_end_class.


:- pce_begin_class(emacs_autocomplete_browser, browser,
		   "Browse autocompletions").

:- pce_global(@emacs_autocomplete_browser_recogniser,
	      make_autocomplete_recogniser).

make_autocomplete_recogniser(G) :-
	new(G, key_binding),
	send(G, function, q, destroy).

initialise(B) :->
	"Setup the browser"::
	send_super(B, initialise),
	send(B, select_message,
	     message(@receiver, insert_selection)),
	send(B, recogniser, @emacs_autocomplete_browser_recogniser).

insert_selection(B) :->
	"Insert current selection in <-editor"::
	get(B, selection, DI),
	get(DI, key, Text),
	send(B?editor, insert_autocompletion, Text),
	send(B, destroy).

:- pce_end_class.


:- emacs_extend_mode(fundamental,
		     [ autocomplete = key('\\C-c\\C-p')
		     ]).

autocomplete(M) :->
	"Open auto-completer based on current word"::
	get(M, editor, E),
	get(M, autocomplete_range, tuple(SOW,EOW)),
	get(M, contents, SOW, EOW-SOW, String),
	get(String, value, Prefix),
	completions(Prefix, Completions),
	debug(autocomplete, '~q --> ~p', [Prefix, Completions]),
	Completions \== [],
	send(@emacs_completion_browser, members, Completions),
	get(M?image, character_position, SOW, point(CX,CY)),
	get(M?image, display_position, point(IX,IY)),
	X is IX+CX,
	Y is IY+CY,
	debug(autocomplete, 'Pos = ~w,~w', [X,Y]),
	new(_, partof_hyper(E, @emacs_completion_browser, autocompleter, editor)),
	send(@emacs_completion_browser, transient_for, E?frame),
	send(@emacs_completion_browser, kind, popup),
	send(@emacs_completion_browser, modal, transient),
	send(@emacs_completion_browser, open, point(X,Y)).

autocomplete_range(M, T:tuple) :<-
	"Range for text autocompletion"::
	get(M, caret, Caret),
	get(M, scan, Caret, word, 0, start, SOW),
	get(M, scan, SOW, word, 0, end, EOW),
	EOW > SOW,
	EOW == Caret,
	new(T, tuple(SOW, EOW)).

insert_autocompletion(M, Text:char_array) :->
	"Insert completion from completion browser"::
	get(M, autocomplete_range, tuple(SOW,EOW)),
	get(M, text_buffer, TB),
	send(TB, delete, SOW, EOW-SOW),
	send(TB, insert, SOW, Text),
	send(M, caret, SOW+Text?size).

completions(Prefix, Completions) :-
	'$atom_completions'(Prefix, Completions).

:- emacs_end_mode.
