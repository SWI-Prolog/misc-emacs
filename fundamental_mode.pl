/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(my_fundamental_mode, []).
:- use_module(library(pce)).

:- emacs_extend_mode(fundamental,
		     [ dos2unix = button(file)
		     ]).

dos2unix(M) :->
	"Convert DOS CR/LF to Unix LF"::
	get(M, text_buffer, TB),
	new(Re, regex(string('\r'))),
	send(Re, for_all, TB, message(Re, replace, TB, '')).

:- emacs_end_mode.
