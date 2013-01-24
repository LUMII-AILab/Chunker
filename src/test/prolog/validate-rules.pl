/* $Revision: 339 $, $Date: 2009-04-04 03:37:11 +0300 (Se, 04 Apr 2009) $ */

:- ensure_loaded(defs).

validate :- 
	
	File = 'validate-rules.html',
	open(File, write, Output, [encoding(utf8)]),
	set_prolog_IO(user_input, Output, user_error),

	format('<?xml version="1.0" encoding="utf-8"?>~n'),
	format('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">~n'),
	format('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">~n'),
	format('<head>~n'),
	format('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />~n'),
	format('<title>Grammar rules validation results</title>~n'),
	format('</head>~n'),
	format('<body style="font-family: monospace;">~n'),
	format('---[BEGIN]---~n<br /><br />~n'),
	
	format('---[B]---~n'),
	format('<ol>~n'),
	validate_b,
	format('</ol>~n'),

	format('---[X]---~n'),
	format('<ol>~n'),
	validate_x,
	format('</ol>~n'),

	format('---[END]---~n'),
	format('</body>~n'),
	format('</html>~n'),
	close(Output),
	win_shell(open, File).
%	halt.

print_rule(Rule :- true, Color) :-
	print_rule(Rule, Color), !.

print_rule(Rule, Color) :-
	numbervars(Rule, 0, _, [singletons(true)]),
	format('<span style="color: ~w;">~p</span>', [Color, Rule]), !.

print_rules([], _) :- !.

print_rules([Rule | Rules], Color) :-
	print_rule(Rule, Color),
	format('<br/>~n'),
	print_rules(Rules, Color), !.

validate_b :-
	forall(clause(b(SyntaxRole, Morphology1, Morphology2, Position), Body),
	(
		B = (b(SyntaxRole, Morphology1, Morphology2, Position) :- Body),
		validate_b(B)
	)).

validate_b(BClause) :-
	BClause = (b(SyntaxRole, _, _, _) :- _),
	format('<li>~@</li><br/>~n', [print_rule(BClause, green)]), 
	forall(b(SyntaxRole, Morphology1, Morphology2, Position), 
	(
		B = b(SyntaxRole, Morphology1, Morphology2, Position),
		match_similar_rules(B, SimilarRules),
		(not(SimilarRules = []) -> (
			format('<ul>~n<li>Morphology in rule<br/>~n~@ matches morphology in rule(s):<br/>~n~@<br/></li>~n</ul>~n', [print_rule(B, red), print_rules(SimilarRules, blue)])
		) ; true)
	)).

validate_x :-
	forall(clause(x(WordList, Tree, [Word, Morphology, ID, Source, BaseForm]), Body), 
	(
		X = (x(WordList, Tree, [Word, Morphology, ID, Source, BaseForm]) :- Body),
		validate_x(X)
	)).

validate_x(XClause) :-
	XClause = (x(_, _, [_, _, ID, _, _]) :- _),
	format('<li>~@</li><br/>~n', [print_rule(XClause, green)]), 
	forall(x(WordList, Tree, [Word, Morphology, ID, Source, BaseForm]), 
	(
		X = x(WordList, Tree, [Word, Morphology, ID, Source, BaseForm]),
		match_similar_rules(X, SimilarRules),
		(not(SimilarRules = []) -> ( 
			format('<ul>~n<li>Word list in rule<br/>~n~@ matches word list in rule(s):<br/>~n~@<br/></li>~n</ul>~n', [print_rule(X, red), print_rules(SimilarRules, blue)])
		) ; true)
	)).

match_similar_rules(Rule, SimilarRules) :-
	findall(SimilarRule, match_similar_rule(Rule, SimilarRule), SimilarRules).

match_similar_rule(b(SyntaxRole, Morphology1, Morphology2, _), SimilarRule) :-
	copy_term(Morphology1, CMorphology1),
	copy_term(Morphology2, CMorphology2),
	b(RSyntaxRole, RMorphology1, RMorphology2, RPosition),
	not(RSyntaxRole = SyntaxRole),   
	copy_term(RMorphology1, CRMorphology1),
	copy_term(RMorphology2, CRMorphology2),
	CRMorphology1 = CMorphology1,
	CRMorphology2 = CMorphology2,
	SimilarRule = b(RSyntaxRole, RMorphology1, RMorphology2, RPosition).
	
match_similar_rule(x(WordList, _, [_, _, ID, _, _]), SimilarRule) :-
	copy_term(WordList, CWordList),
	x(RWordList, RTree, [RWord, RMorphology, RID, RSource, RBaseForm]),
	not(RID = ID),   
	copy_term(RWordList, CRWordList),
	CRWordList = CWordList,
	SimilarRule = x(RWordList, RTree, [RWord, RMorphology, RID, RSource, RBaseForm]).

:- validate.
