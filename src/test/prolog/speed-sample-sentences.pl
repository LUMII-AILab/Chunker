/* $Revision: 548 $, $Date: 2009-05-18 16:45:44 +0300 (Mon, 18 May 2009) $ */

:- style_check(-discontiguous).

:- ensure_loaded('config').
:- ensure_loaded('parser').
:- ensure_loaded('sample-sentences').

main :- 
	Repeat = 3,
	get_time(STime),
	forall(between(1, Repeat, _X), forall(sample_sentence(_SentenceId, Sentence), parse_all(Sentence, _Chunks))), 
 	get_time(ETime),
	DTotalTime is ETime - STime,
	DTime is DTotalTime / Repeat,
	format('Izpildes laiks: ~w sec [cikls: ~w sec]~n', [DTotalTime, DTime]).

:- main.