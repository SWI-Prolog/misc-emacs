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
			   highlight(M, In, Codes),
			   close(In)).

highlight(Mode, In, Path) :-
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   (   debug(highlight, 'Line: ~s', [Line]),
	        append(Path, [0':|More], Pattern),
	        append(_, Pattern, Line),
		digits(More, Digits),
		number_codes(LineNo, Digits),
		highlight_line(Mode, LineNo)
	    ->  true
	    ;   true
	    ),
	    highlight(Mode, In, Path)
	).

digits([H|T0], [H|T]) :-
	code_type(H, digit), !,
	digits(T0, T).
digits(_, []).

highlight_line(Mode, LineNo) :-
	get(Mode, text_buffer, TB),
	get(TB, scan, 0, line, LineNo-1, start, SOL),
	get(TB, scan, SOL, line, 0, end, EOL),
	new(_, fragment(TB, SOL, EOL+1-SOL, highlight)).

:- emacs_end_mode.

:- use_module(table).
