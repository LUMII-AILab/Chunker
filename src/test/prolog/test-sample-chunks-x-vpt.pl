/* $Revision: 548 $, $Date: 2009-05-18 16:45:44 +0300 (Pr, 18 Mai 2009) $ */

:- style_check(-discontiguous).

:- ensure_loaded('config').
:- ensure_loaded('parser').
:- ensure_loaded('semti-html').
:- ensure_loaded('sample-chunks-x-vpt').
:- ensure_loaded('tools').

main :- 
	File = 'test-sample-chunks-x-vpt.html',
	open(File, write, Output, [encoding(utf8)]),
	set_prolog_IO(user_input, Output, user_error),

	format('<?xml version="1.0" encoding="utf-8"?>~n'),
	format('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">~n'),
	format('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">~n'),
	format('<head>~n'),
	format('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />~n'),
	format('<title>Piemēra analītisko formu parsēšanas rezultāti</title>~n'),
	format('<link rel="stylesheet" type="text/css" href="style-box.css" />~n'),
	format('</head>~n'),
	format('<body>~n'),

	config(parse_all, ParseAll),
	config(parse_time_limit, TimeLimit),

	get_time(STime),

	forall(sample_sentence(SentenceId, Sentence),
	(
		counter_next(sentence, SentenceNumber),
		(SentenceNumber mod 2 =:= 0 -> SentenceClass = 'sample-even'; SentenceClass = 'sample-odd'),
		format('<div class="~w">', [SentenceClass]),
		format('<p class="sentence">Teikums: ~w. <span class="sentence">~w</span></p>~n', [SentenceId, Sentence]),
		format(user_error, 'Processing: ~w. ~w~n', [SentenceId, Sentence]),
		parse_by_config(ParseAll, TimeLimit, Sentence, Results),
		(nonvar(Results) -> parse_results_html_write(Results) ; format('<p class="parse-error">Pārsniegts izpildes laika limits ~d sekundes</p>', [TimeLimit])),
		format('</div>'),
		format('<hr class="sample-separator" />~n')
	)),

	get_time(ETime),

	DTime is ETime - STime,

	format('<p>Izpildes laiks: ~w sec</p>~n', [DTime]),
	format(user_error, 'Izpildes laiks: ~w sec~n', [DTime]),

	format('</body>~n'),
	format('</html>~n'),
	close(Output),
	win_shell(open, File),
	halt.

:- main.
