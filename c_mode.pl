/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(my_c_mode, []).
:- use_module(library(pce)).

:- emacs_extend_mode(c,
		     [ find_elipse = button(pce)
		     ]).

elipse_call(getValueExpression).
elipse_call('XPCE_send').
elipse_call('XPCE_get').
elipse_call('XPCE_new').
elipse_call('XPCE_call').
elipse_call('XPCE_funcall').
elipse_call(send).
elipse_call(get).
elipse_call(newObject).
elipse_call(tempObject).
elipse_call(globalObject).
elipse_call(answerObject).
elipse_call(changedObject).
elipse_call(exceptionPce).
elipse_call(forwardCode).
elipse_call(forwardReceiverCode).
elipse_call(getForwardFunction).
elipse_call(getForwardReceiverFunction).


find_elipse(M) :->
	"Find calls to send(..., 0), etc"::
	get(M, text_buffer, TB),
	new(Re, regex('\\(\\w+\\)(')),
	send(Re, for_all, TB,
	     if(message(M, elipse_hit, @arg1))).

elipse_hit(M, Re:regex) :->
	get(M, text_buffer, TB),
	get(Re, register_value, TB, 1, name, Func),
	elipse_call(Func),		% got one
	get(Re, register_end, Open),
	format('Match at ~w~n', [Open]),
	get(TB, matching_bracket, Open-1, Close),
	format('Close at ~w~n', [Close]),
	new(Re2, regex(',\\s *0\\s *')),
	send(Re2, match, TB, Close, 0),	% ends in 0
	send(M, selection, Close-1, Close),
	send(M, normalise),
	send(@display, inform, 'Found one').

:- emacs_end_mode.
