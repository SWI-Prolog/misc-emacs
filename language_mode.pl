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

/*
convert_class(M) :->
	"Convert old-stype XPCE makeClassFunction"::
	new(Tmp, file),
	send(M, write_region, Tmp),
	get(Tmp, name, FileName),
	concat(FileName, '.out', OutFile),
	convert_class(FileName, OutFile),
	send(M, insert_file, OutFile),
	send(file(OutFile), remove),
	send(Tmp, remove).
*/

:- emacs_end_mode.

:- use_module(table).
