/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(my_prolog_mode, []).
:- use_module(library(pce)).

:- emacs_extend_mode(prolog,
		     [ show_syntax = key('\\C-\\')
		     ]).

show_syntax(M) :->
	"Show syntactical category at point"::
	get(M, caret, Caret),
	get(M, scan_syntax, 0, Caret, tuple(Syntax, Start)),
	send(M, report, inform,
	     'Syntax at %d is %s, started at %d', Caret, Syntax, Start).


		 /*******************************
		 *	 SYNTAX CHECKING	*
		 *******************************/

check_region(M) :->
	"Check syntax in region"::
	get(M, region, tuple(Start, End)),
	get(M, scan, Start, line, 0, start, SOL),
	get(M, scan, End, line, 0, start, EOL),
	check_region(M, SOL, EOL, Checked),
	send(M, report, status, 'Checked %d clauses', Checked).

check_region(_, SOL, EOL, 0) :-
	SOL >= EOL, !.
check_region(M, SOL, EOL, N) :-
	get(M, check_clause, SOL, EOC),
	check_region(M, EOC, EOL, NN),
	N is NN + 1.

:- emacs_end_mode.
