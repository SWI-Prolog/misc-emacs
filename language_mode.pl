/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(my_language_mode, []).
:- use_module(library(pce)).

:- emacs_extend_mode(language,
		     [ increment_number = key('\\e+')
		     ]).

increment_number(M) :->
	"Increment number at caret and down caret"::
	get(M, column, C0),
	get(M, word, W0),
	get(@pce, convert, W0, int, I0),
	I is I0 + 1,
	send(M, kill_word),
	send(M, insert, I),
	send(M, next_line),
	send(M, column, C0).

highlight_lines(M, File:file) :->
	"Highlight <Path>:<Line> lines matching buffer"::
	get(M, text_buffer, TB),
	send(TB?editors, for_all,
	     message(@arg1, style, highlight,
		     style(background := light_blue))),
	send(TB, for_all_fragments,
	     if(@arg1?style == highlight,
		message(@arg1, destroy))),
	get(TB, file, TBFile),
	get(TBFile, name, Name),
	absolute_file_name(Name, Path),
	atom_codes(Path, Codes),
	get(File, name, FileName),
	setup_call_cleanup(open(FileName, read, In),
			   highlight(M, In, Codes, 0),
			   close(In)).

highlight(Mode, In, Path, C0) :-
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  send(Mode, report, status, 'Highlighted %d lines', C0)
	;   (   debug(highlight, 'Line: ~s', [Line]),
	        append(Path, [0':|More], Pattern),
	        append(_, Pattern, Line),
		digits(More, Digits, Rest),
		number_codes(LineNo, Digits),
		highlight_line(Mode, LineNo, Rest)
	    ->  C1 is C0 + 1
	    ;   C1 = C0
	    ),
	    highlight(Mode, In, Path, C1)
	).

digits([H|T0], [H|T], Rest) :-
	code_type(H, digit), !,
	digits(T0, T, Rest).
digits(Rest, [], Rest).

highlight_line(Mode, LineNo, Message) :-
	get(Mode, text_buffer, TB),
	get(TB, scan, 0, line, LineNo-1, start, SOL),
	get(TB, scan, SOL, line, 0, end, EOL),
	new(F, emacs_colour_fragment(TB, SOL, EOL+1-SOL, highlight)),
	send(F, message, string(Message)),
	(   callers_popup(Message, Mode, Popup)
	->  send(F, popup, Popup)
	;   true
	).

callers_popup(Message, Mode, Popup) :-
	new(Popup, popup('Callers',
			 message(Mode, find_tag, @arg1))),
	phrase(callers(Popup), Message).

callers(Popup) -->
	string(_),
	"Calls ",
	string(Callers),
	" [", !,
	{ atom_codes(Atom, Callers),
	  atomic_list_concat(List, '<-', Atom),
	  forall(member(F, List),
		 send(Popup, append,
		      menu_item(F, @default, F)))
	},
	string(_).

string([]) --> [].
string([H|T]) --> [H], string(T).

:- emacs_end_mode.

:- use_module(table).
