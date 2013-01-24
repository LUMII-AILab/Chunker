
:- ensure_loaded('etu2a').

main :- 
	File= 'job-sentences.pl',
	open(File, write, Output, [encoding(utf8),bom(true)]),
	set_prolog_IO(user_input, Output, user_error),
        tokenize_file('job-input.txt',Tokens),tokens_words(Tokens,WordList),
        writeln('sentence_group_id(''129'').'), nl,
        pp(1,WordList),
        SentenceId=1,
	close(Output),
	halt.

pp(N,L) :- write('sentence('),write(N),write(','''),rec(N,L),writeln(''').').
rec(N,[]).
rec(N,[A|B]) :- N2 is N+1, A='.',!,writeln(''').'),write('sentence('),write(N2),write(','''),rec(N2,B).
rec(N,[A|B]) :- write(A),write(' '),rec(N,B).


:- main.
