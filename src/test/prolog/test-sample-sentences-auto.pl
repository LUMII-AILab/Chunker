﻿/* $Revision: 548 $, $Date: 2009-05-18 16:45:44 +0300 (Mon, 18 May 2009) $ */

:- style_check(-discontiguous).

:- ensure_loaded('config').
:- ensure_loaded('parser').
:- ensure_loaded('semti-html').
:- ensure_loaded('sample-sentences-auto').
:- ensure_loaded('tools').

main(File, MinId, MaxId) :- 
	open(File, write, Output, [encoding(utf8)]),
	set_prolog_IO(user_input, Output, user_error),

	format('<?xml version="1.0" encoding="utf-8"?>~n'),
	format('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">~n'),
	format('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">~n'),
	format('<head>~n'),
	format('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />~n'),
	format('<title>Piemēra teikumu parsēšanas rezultāti</title>~n'),
	format('<link rel="stylesheet" type="text/css" href="style-box.css" />~n'),
	format('</head>~n'),
	format('<body>~n'),

	config(parse_all, ParseAll),
	config(parse_time_limit, TimeLimit),

	get_time(STime),

	forall(sample_sentence(SentenceId, Sentence),
	(
		between(MinId, MaxId, SentenceId),
		counter_next(sentence, SentenceNumber),
		(SentenceNumber mod 2 =:= 0 -> SentenceClass = 'sample-even'; SentenceClass = 'sample-odd'),
		format('<div class="~w">', [SentenceClass]),
		format('<p class="sentence">Teikums: ~w. <span class="sentence">~w</span></p>~n', [SentenceId, Sentence]),
		format(user_error, 'Processing: ~w. ~w~n', [SentenceId, Sentence]),
		parse_by_config(ParseAll, TimeLimit, Sentence, Results),
		(nonvar(Results) -> parse_results_html_write(Results) ; format('<p class="parse-error">Pārsniegts izpildes laika limits ~d sekundes</p>', [TimeLimit])),
		format('</div>'),
		format('<hr class="sample-separator" />~n')
	); true),

	get_time(ETime),

	DTime is ETime - STime,

	format('<p>Izpildes laiks: ~w sec</p>~n', [DTime]),
	format(user_error, 'Izpildes laiks: ~w sec~n', [DTime]),

	format('</body>~n'),
	format('</html>~n'),
	close(Output).

:-
	main('test-sample-sentences-auto-100.html', 1, 100),
	main('test-sample-sentences-auto-200.html', 101, 200),
	main('test-sample-sentences-auto-300.html', 201, 300),
	main('test-sample-sentences-auto-400.html', 301, 400),
	main('test-sample-sentences-auto-500.html', 401, 500),
	main('test-sample-sentences-auto-600.html', 501, 600),
	main('test-sample-sentences-auto-700.html', 601, 700),
	main('test-sample-sentences-auto-800.html', 701, 800),
	main('test-sample-sentences-auto-900.html', 801, 900),
	main('test-sample-sentences-auto-1000.html', 901, 1000),
	main('test-sample-sentences-auto-1100.html', 1001, 1100),
	main('test-sample-sentences-auto-1200.html', 1101, 1200),
	main('test-sample-sentences-auto-1300.html', 1201, 1300).