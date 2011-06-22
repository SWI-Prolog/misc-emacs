/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(my_prolog_mode, []).
:- use_module(library(pce)).
:- use_module(library(prolog_xref)).
:- use_module(autocomplete).

:- emacs_extend_mode(prolog,
		     [ show_syntax = key('\\C-\\'),
		       autocomplete = key('\\C-c\\C-p')
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


		 /*******************************
		 *	 AUTO-COMPLETION	*
		 *******************************/

completions(M, _SOW:int, Prefix:name, Completions:chain) :<-
	"Autocomplete predicate"::
	get(M, text_buffer, TB),
	findall(DI, completion(TB, Prefix, DI), List),
	chain_list(Completions, List),
	send(Completions, sort).

completion(_TB, Prefix, DI) :-
	predicate_property(system:Head, built_in),
	functor(Head, Name, Arity),
	sub_atom(Name, 0, _, _, Prefix),
	new(DI, dict_item(Name, string('%s/%d', Name, Arity))),
	send(DI, style, built_in).
completion(TB, Prefix, DI) :-
	xref_defined(TB, Callable, How),
	functor(Callable, Name, Arity),
	sub_atom(Name, 0, _, _, Prefix),
	new(DI, dict_item(Name, string('%s/%d', Name, Arity))),
	functor(How, Style, _),
	send(DI, style, Style).

setup_completion_styles(M, F:autocomplete_browser) :->
	"Setup the styles for candidate predicates"::
	get(F, browser, Browser),
	get(M, styles, Sheet),
	(   copy_style(Name, Class),
	    emacs_prolog_colours:style(goal(Class,_), StyleName, _),
	    get(Sheet, value, StyleName, Style),
	    send(Browser, style, Name, Style),
	    fail
	;   true
	).

copy_style(built_in,	 built_in).
copy_style(local,	 local(_)).
copy_style(imported,	 imported(_)).
copy_style(dynamic,	 dynamic(_)).
copy_style(thread_local, thread_local(_)).

insert_autocompletion(M, Text:char_array) :->
	send_super(M, insert_autocompletion, Text),
	send(M, mark_variable, @on).

:- emacs_end_mode.
