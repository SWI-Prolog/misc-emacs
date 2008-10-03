/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(emacs_tabular, []).
:- use_module(library(pce)).
:- use_module(library(lists)).

% Debugging statements
%:- send(@emacs, kind, user).
%:- use_module('/oorlam/jan/src/boot/lib/pretty_print').

:- emacs_extend_mode(language,
		     [
		     ]).

:- pce_global(@emacs_white_regex,     new(regex(string('[ \t]*')))).
:- pce_global(@emacs_non_white_regex, new(regex(string('[^ \t]*')))).

tabularise_selection(M) :->
	"Tabularise terms in the selection"::
	get(M, selection, point(From, To)),
	send(M, tabularise, From, To).

tabularise(M, From:[int], To:[int], Sep:[regex]) :->
	"Tabularise terms in region"::
	get(M, line_region, From, To, tuple(S, E)),
	send(M, style, term, style(background := grey80)),
	identify_terms(M, S, E, Sep, TermList),
%	nl, pretty_print(TermList),
	send(M, unshow_terms),
	create_fragments(TermList),
	align_columns(TermList),
	free_fragments(TermList),
	send(M, align_comment, From, To).
	
tabularise_regex(M, F0:[int], T0:[int], Sep:regex) :->
	"Tabularise based on separator regex"::
	send(M, tabularise, F0, T0, Sep).

unshow_terms(M) :->
	get(M, text_buffer, TB),
	send(TB, for_all_fragments,
	     if(@arg1?style == term,
		message(@arg1, free))).

identify_terms(_, S, E, _, []) :-
	S >= E, !.
identify_terms(M, S, E, Sep, [L0|L]) :-
	Sep == @default, !,		% Use terms
	get(M, scan, S, line, 0, end, EOL),
	get(M, end_of_code_on_line, S, EOC),
	(   line_terms(M, S, EOC, L0),
	    \+ L0 = [_]			% just one, break up
	->  true
	;   new(Re, regex('[({[]')),	% go into the term
	    send(Re, search, M, S, EOL),
	    get(Re, register_end, 0, S2),
	    line_terms(M, S2, EOC, L0)
	),
	SOL is EOL+1,
	identify_terms(M, SOL, E, Sep, L).
identify_terms(M, S, E, Sep, [L0|L]) :-
	get(M, scan, S, line, 0, end, EOL),
	get(M, end_of_code_on_line, S, EOC),
	line_columns(M, S, EOC, Sep, L0),
	SOL is EOL+1,
	identify_terms(M, SOL, E, Sep, L).

%	line_terms(+M, +S, +E, -Terms)
%
%	Return a list of fragments indicating the starts and ends if the
%	terms in the character-range.

line_terms(M, S, EOL, [T0|T]) :-
	get_term(M, S, EOL, E, T0),
	E =< EOL, !,
	line_terms(M, E, EOL, T).
line_terms(_, _, _, []).

get_term(M, S, EOL, E, new(_, fragment(M, TS, FL, term))) :-
	get(M, text_buffer, TB),
	get(TB, scan, S, term, 1, end, E0),
	get(@emacs_non_white_regex, match, TB, E0, EOL, L),
	E is E0+L,
	get(TB, scan, S, term, 1, start, TS),
	FL is E-TS.
	
%	line_columns(+M, +S, +E, +Sep, -FragmentList)

line_columns(M, S, E, Sep,
	     [ new(_, fragment(M, FS1, FL1, term)),
	       new(_, fragment(M, FS2, FL2, term))
	     | L
	     ]) :-
	get(M, text_buffer, TB),
	get(TB, skip_layout, S, forward, @off, FS1),
	send(Sep, search, TB, FS1, E), !,
	get(Sep, register_start, 0, FS2),
	get(Sep, register_end, 0, FE2),
	get(TB, skip_layout, FS2, backward, @off, FE1),
	FL1 is FE1-FS1,
	FL2 is FE2-FS2,
	line_columns(M, FE2, E, Sep, L).
line_columns(M, S, E, _, [new(_, fragment(M, S1, FL, term))]) :-
	get(M, text_buffer, TB),
	get(TB, skip_layout, S, forward, @off, S1),
	get(TB, skip_layout, E-1, backward, E1),
	E1 > S, !,
%	format('E=~w, E=~w~n', [E, E1]),
	FL is E1+1-S1.
line_columns(_, _, _, _, []).
	
%	align_columns(TermList)
%
%	Align all columns from th given termlist

align_columns(TermList) :-
	align_columns(TermList, 1).

align_columns(TermList, N) :-
	align_column(TermList, N), !,
	NN is N + 1,
	align_columns(TermList, NN).
align_columns(_, _).

align_column(TermList, N) :-
	(   N > 1
	->  do_align_column(TermList, N, 0) 	% minimise white space
	;   true				% but not for the first line
	),
	col_start(TermList, N, -1, Start),
	do_align_column(TermList, N, Start).

col_start([], _, S, S) :- !,
	S >= 0.
col_start([L0|T], C, S0, S) :-
	nth1(C, L0, new(F, fragment(M, _, _, _))), !,
	get(F, start, FS),
	get(M, column, FS, S1),
	S2 is max(S0, S1),
	col_start(T, C, S2, S).
col_start([_|T], C, S0, S) :-
	col_start(T, C, S0, S).

do_align_column([], _, _) :- !.
do_align_column([L0|T], C, Start) :-
	nth1(C, L0, new(F, fragment(M, _, _, _))), !,
	get(F, start, S),
	align_if_loose(M, S, Start),
	do_align_column(T, C, Start).
do_align_column([_|T], C, Start) :-
	do_align_column(T, C, Start).

%	->align, but only if the target is at the start of a line
%	or preceded by blank space.

align_if_loose(M, S, Col) :-
	(   S == 0
	;   get(M, text_buffer, TB),
	    get(TB, character, S-1, C),
	    blank(C)
	), !,
	send(M, align, Col, S).
align_if_loose(_, _, _).

blank(32).				% should use syntax table
blank(9).

create_fragments([]).
create_fragments([H|T]) :-
	create_fragments(H),
	create_fragments(T).
create_fragments(new(Ref, Term)) :-
	new(Ref, Term).
	
free_fragments([]).
free_fragments([H|T]) :-
	free_fragments(H),
	free_fragments(T).
free_fragments(new(Ref, _)) :-
	free(Ref).

	
		 /*******************************
		 *	      COMMENT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Comment at the end of a line looks better if all comment is right of all
code. This function finds all line-comment in  the region as well as all
code in the region and places  the   comment  aligned  ansd right of the
code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

align_comment(M, From:[int], To:[int]) :->
	"Align comment in region"::
	(   get(M, line_comment, _)
	->  get(M, comment_column, C),
	    new(Col, number(C)),
	    send(M, for_lines_in_region, From, To,
		 message(M, max_content_line_column, @arg1, Col)),
	    get(Col, value, C0),
	    get(M, tab_distance, TD),
	    CommentCol is ((C0+TD-1)//TD)*TD, 		% round to tab
	    send(M, for_lines_in_region, From, To,
		 message(M, align_line_comment, CommentCol, @arg1))
	;   true
	).
	    

align_line_comment(M, Column:int, From:[int]) :->
	"Align line-comment (if any) to column"::
	(   From == @default
	->  get(M, caret, Caret)
	;   Caret = From
	),
	(   get(M, find_line_comment, Caret, Start)
	->  send(M, align, Column, Start)
	;   true
	).

find_line_comment(M, From:int, Start:int) :<-
	"Find start of line-comment (if any)"::
	get(M, line_comment, CS),
	get(M, text_buffer, TB),
	get(TB, scan, From, line, 0, start, SOL),
	get(TB, scan, From, line, 0, end,   EOL),
	get(regex(?(regex(''), quote, CS)), search, TB, SOL, EOL, Start).

end_of_code_on_line(M, From:int, EndOfCode:int) :<-
	"Find end of code (before line-comment) on line"::
	(   get(M, find_line_comment, From, EOL)
	;   get(M, scan, From, line, 0, end, EOL)
	),
	get(M, skip_layout, EOL, backward, @off, EndOfCode).

max_content_line_column(M, From:int, Max:number) :->
	"Determine maximum length of content"::
	get(M, end_of_code_on_line, From, EOC),
	get(M, column, EOC, Column),
	send(Max, maximum, Column).

:- emacs_end_mode.
