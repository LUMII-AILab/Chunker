/* $Revision: 548 $, $Date: 2009-05-18 16:45:44 +0300 (Mon, 18 May 2009) $ */

:- style_check(-discontiguous).

:- ensure_loaded('config').
:- ensure_loaded('parser').
:- ensure_loaded('sample-sentences').

main :- 
	Repeat = 1,
	config(parse_all, ParseAll),
	config(parse_time_limit, TimeLimit),
	get_time(STime),
	profile(forall(between(1, Repeat, _X), forall(sample_sentence(_SentenceId, Sentence), parse_by_config(ParseAll, TimeLimit, Sentence, _Results)))), 
 	get_time(ETime),
	DTotalTime is ETime - STime,
	DTime is DTotalTime / Repeat,
	format('Izpildes laiks: ~w sec [cikls: ~w sec]~n', [DTotalTime, DTime]).

:- main.