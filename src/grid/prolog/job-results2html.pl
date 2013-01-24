/* $Revision: 550 $, $Date: 2009-05-18 18:16:34 +0300 (Mon, 18 May 2009) $ */

:- style_check(-discontiguous).

:- ensure_loaded('config').
:- ensure_loaded('semti-core').
:- ensure_loaded('semti-html').
:- ensure_loaded('tools').

:- dynamic parsed_sentence/2.


process_file(ResultFile) :- 

	file_name_extension(FileBase, 'pl', ResultFile),
	sformat(File, '~w.html', [FileBase]),
	open(File, write, Output, [encoding(utf8)]),
	set_prolog_IO(user_input, Output, user_error),

	format(user_error, 'Processing: ~w~n', [ResultFile]),

	format('<?xml version="1.0" encoding="utf-8"?>~n'),
	format('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">~n'),
	format('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">~n'),
	format('<head>~n'),
	format('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />~n'),
	format('<title>~w parsēšanas rezultāti</title>~n', [ResultFile]),
	format('<link rel="stylesheet" type="text/css" href="style-box.css" />~n'),
	format('</head>~n'),
	format('<body>~n'),

	consult(ResultFile),

	findall(SentenceResult, parsed_sentence(_, _, SentenceResult), AllResults),
	parse_stats_html_write(AllResults, 'sentence'),

	forall(parsed_sentence(SentenceId, Sentence, SentenceResults),
	(
		counter_next(sentence, SentenceNumber),
		(SentenceNumber mod 2 =:= 0 -> SentenceClass = 'sample-even'; SentenceClass = 'sample-odd'),
		format('<div class="~w">', [SentenceClass]),
		format('<p class="sentence">Teikums: ~w. <span class="sentence">~w</span></p>~n', [SentenceId, Sentence]),
		parse_stats_html_write(SentenceResults),
		(nonvar(SentenceResults) -> parse_results_html_write(SentenceResults) ; format('<p class="parse-error">Pārsniegts izpildes laika limits</p>', [])),
		format('</div>'),
		format('<hr class="sample-separator" />~n')
	)),

	counter_reset(sentence),

	abolish(parsed_sentence, 3),

	format('</body>~n'),
	format('</html>~n'),

	close(Output), !.


main :- expand_file_name('job-output-*.pl', Files),
	checklist(process_file, Files),
 	halt.


:- main.
