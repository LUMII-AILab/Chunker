:- use_module(library(jpl)).

:- ensure_loaded(config).

add_to_classpath(JavaLibs) :-
    (getenv('CLASSPATH', CPOld) -> true ; CPOld = '.'),
    (current_prolog_flag(windows, true) -> Separator = ';' ; Separator = ':'),
    flatten([JavaLibs, CPOld], ConcatJavaLibs),
    concat_atom(ConcatJavaLibs, Separator, CPNew),
    setenv('CLASSPATH', CPNew).

load_jpl:-
    ensure_loaded(library('jpl')),
    jpl_set_default_jvm_opts(['-Xmx128M']).

load_jpl.

:- config(parse_java_libs, JavaLibs), add_to_classpath(JavaLibs).
:- load_jpl.

main :-
	current_input(In),
	read_line_to_codes(In,TextCodes),
	string_to_atom(TextCodes, Text),
	jpl_call('lv.semti.PrologaInterfeiss.PrologWrapper', analyze, [Text], R),
	format('Result: ~w~n', [R]),
	atom_to_term(R, T, B),
	format('Term: ~w~n', [T]).
