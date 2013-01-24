
:- dynamic unknown_word/2.
:- dynamic parsed_sentence/3.
:- multifile parsed_sentence/3.

:- ensure_loaded('semti-core').


add_unknown_word(A, B) :-
	unknown_word(A, B), !.

add_unknown_word(A, B) :-
%	format('~w~n', [A]),
	assert(unknown_word(A, B)), !.

add_unknown_word(Word) :-
	add_unknown_word(Word, unknown).

process_result([Result]) :-
	chunk_list_filter_words(Result, Words),
	length(Words,L),
%	format('Words: ~w~n', [L]),
	checklist(add_unknown_word, Words), !.

process_result(_).



process_file(ResultFile) :- 
	retractall(parsed_sentence(_,_,_)),
	consult(ResultFile),
	findall(Result,parsed_sentence(_,_,Result),Results),
	length(Results,L),
	format('Results: ~w~n', [L]),
	checklist(process_result, Results),
	retractall(parsed_sentence(_,_,_)).


main :-	expand_file_name('pl-outputs/*.pl', Files),
	checklist(process_file, Files),
	findall(unknown_word(A, B), unknown_word(A, B), Words),
	sort(Words, SortedWords),
	open('job-unknownwords.txt', write, Output, [encoding(utf8)]),
	set_prolog_IO(user_input, Output, user_error),
	checklist(writeln, SortedWords),
	close(Output),
 	halt.

:- main.
