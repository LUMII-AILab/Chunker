/* $Revision: 548 $, $Date: 2009-05-18 16:45:44 +0300 (Mon, 18 May 2009) $ */

:- style_check(-discontiguous).

:- ensure_loaded('config').
:- ensure_loaded('parser').
:- ensure_loaded('job-sentences').
:- ensure_loaded('tools').

main :- 
	sentence_group_id(SentenceGroupId),
	sformat(File, 'job-output-~w.pl', [SentenceGroupId]),
	open(File, write, Output, [encoding(utf8),bom(true)]),
	set_prolog_IO(user_input, Output, user_error),

	config(parse_all, ParseAll),
	config(parse_time_limit, TimeLimit),

	forall(sentence(SentenceId, Sentence),
	(
		parse_by_config(ParseAll, TimeLimit, Sentence, Results),
		numbervars(Results, 0, _, [singletons(true)]),
		format('parsed_sentence(~w, ~q, ~p).~n', [SentenceId, Sentence, Results])
	)),

	close(Output),
	halt.

:- main.
