/* $Revision: 548 $, $Date: 2009-05-18 16:45:44 +0300 (Mon, 18 May 2009) $ */

:- ensure_loaded(defs).
:- ensure_loaded(morphology).
:- ensure_loaded(tools).

validate :- 
	
	File = 'morphology-bugs.html',
	open(File, write, Output, [encoding(utf8)]),
	set_prolog_IO(user_input, Output, user_error),

	format('<?xml version="1.0" encoding="utf-8"?>~n'),
	format('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">~n'),
	format('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">~n'),
	format('<head>~n'),
	format('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />~n'),
	format('<title>Morfoloģiskā marķējuma neatbilstība specifikācijai</title>~n'),
	format('</head>~n'),
	format('<body style="font-family: monospace;">~n'),
	format('---[BEGIN]---~n<br /><br />~n'),
	
	format('---[A]---~n'),
	format('<ol>~n'),
	validate_a,
	format('</ol>~n'),

	format('---[X: morphology]---~n'),
	format('<ol>~n'),
	validate_x_morphology,
	format('</ol>~n'),

	format('---[B]---~n'),
	format('<ol>~n'),
	validate_b,
	format('</ol>~n'),

	format('---[X: words]---~n'),
	format('<ol>~n'),
	validate_x_wordinfo_used,
	format('</ol>~n'),

	format('---[END]---~n'),
	format('</body>~n'),
	format('</html>~n'),
	close(Output),
	win_shell(open, File),
	halt.

validate_a :-
	forall(a(Word, Morphology, Lemma), 
		(not((ground(Morphology), morphology_word_info(Morphology, lvsh, _))) ->  
			format('<li>Unknown markup <span style="color: red;">~w</span> in ~w (~w)</li>~n', [Morphology, Word, Lemma]) ; true)).

validate_b :-
	forall(b(SyntaxRole, Morphology1, Morphology2, Position), 
		(not(morphology_all_used([Morphology1, Morphology2])) -> ( 
			B = b(SyntaxRole, Morphology1, Morphology2, Position),
			numbervars(B, 0, _, [singletons(true)]),
			format('<li>Unmatched b/4 rule<br/><span style="color: red;">~p</span></li>~n', [B])
		) ; true)).

validate_x_morphology :-
	forall(x(WordList, Tree, XWord),
		(not(wordinfo_x_morphology(XWord)) -> (
			X = x(WordList, Tree, XWord),
			numbervars(X, 0, _, [singletons(true)]),
			XWord = [_, XMorphology | _],
			format('<li>Unknown x-word markup <span style="color: red;">~p</span> in x/3 rule<br/><span style="color: blue;">~p</span></li>~n', [XMorphology, X])
		) ; true)).

validate_x_wordinfo_used :-
	forall(x(WordList, Tree, XWord),
		(not(wordinfo_all_used(WordList)) -> (
			X = x(WordList, Tree, XWord),
			numbervars(X, 0, _, [singletons(true)]),
			format('<li>Unmatched x/3 rule<br/><span style="color: red;">~p</span></li>~n', [X])
		) ; true)).

wordinfo_x_morphology(WordInfo) :-
	copy_term(WordInfo, CWordInfo), !,
	CWordInfo = [_, Morphology, _, _], !,
	morphology_word_info(Morphology, lvsh, _), !.

wordinfo_all_used([]) :- !.

wordinfo_all_used([H|T]) :-
	wordinfo_used(H), !,
	wordinfo_all_used(T), !.

wordinfo_used(WordInfo) :-
	copy_term(WordInfo, CWordInfo), !,
	CWordInfo = [Word, Morphology | _], !,
	(a(Word, Morphology, _); x(_, _, [Word, Morphology, _, _])), !.

morphology_all_used([]) :- !.

morphology_all_used([H|T]) :-
	morphology_used(H), !,
	morphology_all_used(T), !.

morphology_used(Morphology) :-
	copy_term(Morphology, CMorphology), !,
	(a(_, CMorphology, _); x(_, _, [_, CMorphology, _, _])), !.

:- validate.
