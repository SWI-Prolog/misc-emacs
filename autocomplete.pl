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

:- pce_begin_class(autocomplete_browser, frame,
		   "Browser to show and select autocomplete results").

variable(editor, editor*, both, "Associated editor").

initialise(F, Editor:editor) :->
	send(F, slot, editor, Editor),
	send_super(F, initialise, 'Autocomplete', popup,
		   application := @emacs),
	send(F, append, new(B, emacs_autocomplete_browser)),
	send(F?tile, border, 0),
	send(B, name, browser).

unlink(F) :->
	(   get(F, editor, Editor), Editor \== @nil
	->  get(Editor, window, Window),
	    send(Window, focus, Editor),
	    send(Window, keyboard_focus, Editor)
	;   true
	),
	send_super(F, unlink).

browser(F, B:emacs_autocomplete_browser) :<-
	get(F, member, browser, B).

complete_from(F, Prefix:char_array, Members:chain) :->
	"Fill browser with members"::
	get(F, member, browser, B),
	send(B, members, Members),
	forall(sub_atom(Prefix, _, 1, _, Char),
	       send(B, insert_self, 1, Char)).

:- pce_end_class.


:- pce_begin_class(emacs_autocomplete_browser, browser,
		   "Browse autocompletions").

:- pce_global(@emacs_autocomplete_browser_recogniser,
	      make_autocomplete_recogniser).

make_autocomplete_recogniser(G) :-
	new(G, key_binding),
	send(G, function, '\\e', destroy),
	send(G, function, '\\C-g', destroy),
	send(G, function, 'RET', enter),
	send(G, function, 'TAB', enter),
	send(G, function, backspace, backspace).

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

extend(B, Code:int) :->
	"Extend current word with char"::
	send(B?editor, extend_completion, Code),
	send(B?list_browser, insert_self, 1, Code).

backspace(B) :->
	send(B?editor, backspace_completion),
	send(B?list_browser, backward_delete_char).

event(B, Ev:event) :->
	"Process an event"::
	(   send(@emacs_autocomplete_browser_recogniser, event, Ev)
	->  true
	;   get(Ev, id, Code),
	    integer(Code),
	    (	code_type(Code, csym)
	    ->  send(B, extend, Code)
	    ;	send(B?editor, extend_completion, Code),
	        send(B, destroy)
	    )
	;   get(B, list_browser, LB),
	    send(LB, event, Ev)
	).

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
	get(M, completions, SOW, Prefix, Completions),
	debug(autocomplete, '~q --> ~p', [Prefix, Completions]),
	Completions \== [],
	new(F, autocomplete_browser(E)),
	send(F, complete_from, Prefix, Completions),
	get(M?image, character_position, SOW, point(CX,CY)),
	get(M?image, display_position, point(IX,IY)),
	X is IX+CX,
	Y is IY+CY+7,
	debug(autocomplete, 'Pos = ~w,~w', [X,Y]),
	new(_, partof_hyper(E, F)),
	send(F, open, point(X,Y)),
	get(E, window, Window),
	get(F, browser, Browser),
	send(Window, focus, Browser),
	send(Window, keyboard_focus, Browser).

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
	send(TB, mark_undo),
	send(M, caret, SOW+Text?size).

extend_completion(M, Char:int) :->
	"Add a character"::
	send(M, typed, Char, M?editor),
	send(M, mark_undo).

backspace_completion(M) :->
	"Backward delete character in completion"::
	send(M, backward_delete_char),
	send(M, mark_undo).

completions(_M, _SOW:int, Prefix:name, Completions:chain) :<-
	"Completions is the list of completions for Prefix"::
	'$atom_completions'(Prefix, List),
	chain_list(Completions, List).

:- emacs_end_mode.
