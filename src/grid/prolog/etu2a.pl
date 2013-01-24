% et.pl - M. Covington      2003 February 12

% etu.pl - Modified for Unicode - Donald Rogers     2006 July 17
%          email: dero9753@ihug.co.nz
%          Modified to cope with comma in numbers   2006 July 20

% ET the Efficient Tokenizer

% Measured speed: On a 1-GHz Pentium III,
% about 6 seconds per megabyte of text tokenized,
% or about three lines of text per millisecond.

% New in this version: Special handling of apostrophes.
% The apostrophe is a whitespace character except in
% the sequence 't which is treated as just t,
% making morphological analysis easier.


%%
%% User-callable routines
%%

% tokens_words(+Tokens,-Words)
%  From the output of the other routines, extracts just
%  the word tokens and converts them to atoms.

tokens_words([],[]).

tokens_words([w(Chars)|Tokens],[Atom|Atoms]) :-
   !,
   atom_chars(Atom,Chars),
   tokens_words(Tokens,Atoms).

tokens_words([s(Atom)|Tokens],[Atom|Atoms]) :-
   (Atom=',';Atom='.'), !,
   tokens_words(Tokens,Atoms).

tokens_words([n(Chars)|Tokens],[Atom|Atoms]) :-
   !,
   atom_chars(Atom,Chars),
   tokens_words(Tokens,Atoms).

tokens_words([_|Tokens],Atoms) :-
   % skip non-word tokens
   tokens_words(Tokens,Atoms).



% tokenize_file(+Filename,-Tokens)
%  Reads an entire file and tokenizes it.

tokenize_file(Filename,Tokens) :-
   open(Filename,read,Stream),
   tokenize_stream(Stream,Tokens),
   close(Stream).


% tokenize_stream(+Stream,-Tokens)
%  Reads an entire stream and tokenizes it.

tokenize_stream(Stream,[]) :-
   at_end_of_stream(Stream),
   !.

tokenize_stream(Stream,Tokens) :-
   tokenize_line_dl(Stream,Tokens/Tail),
   tokenize_stream(Stream,Tail).
   

% tokenize_line(+Stream,-Tokens)
%  Reads a line of input and returns a list of tokens.

tokenize_line(Stream,Tokens) :-
   tokenize_line_dl(Stream,Tokens/[]).


% tokenize_line_dl(+Stream,-Tokens/Tail)
%  Like tokenize_line, but uses a difference list.
%  This makes it easier to append the results of successive calls.

tokenize_line_dl(Stream,Tail/Tail) :-
   at_end_of_stream(Stream),                        % unnecessary test?
   !.

tokenize_line_dl(Stream,Dlist) :-
   get_char_and_type(Stream,Char,Type),
   tokenize_line_x(Type,Char,Stream,Dlist).


%%
%% Auxiliary predicates for tokenization
%%

% get_char_and_type(+Stream,-Char,-Type)
%  Reads a character, determines its type, and translates
%  it as specified in char_type_char.

get_char_and_type(Stream,Char,Type) :-
   get_char(Stream,C),
   char_type_char(C,Type,Char).


% tokenize_line_x(+Type,+Char,+Stream,-Tokens/Tail)
%  Tokenizes (the rest of) a line of input.
%  Type and Char describe the character that has been read ahead.

tokenize_line_x(eol,_,_,Tail/Tail) :-               % end of line mark; terminate
   !.

tokenize_line_x(whitespace,_,Stream,Dlist) :-       % whitespace, skip it
   !,
   tokenize_line_dl(Stream,Dlist).


% Word tokens and number tokens have to be completed,
% maintaining 1 character of read-ahead as this is done.
% NewChar and NewType are the character read ahead
% after completing the token.

tokenize_line_x(letter,Char,Stream,[w(T)|Tokens]/Tail) :-
   !,
   tokenize_letters(letter,Char,Stream,T,NewType,NewChar),
   tokenize_line_x(NewType,NewChar,Stream,Tokens/Tail).

tokenize_line_x(digit,Char,Stream,[n(T)|Tokens]/Tail) :-
   !,
   tokenize_digits(digit,Char,Stream,T,NewType,NewChar),
   tokenize_line_x(NewType,NewChar,Stream,Tokens/Tail).


% A period is handled like a digit if it is followed by a digit.
% This handles numbers that are written with the decimal point first.

tokenize_line_x(_, '.', Stream,Dlist) :-
   peek_char(Stream,P),
   char_type_char(P,digit,_),
   !,
   % Start over, classifying '.' as a digit
   tokenize_line_x(digit, '.', Stream,Dlist).

% Donald: A comma is also handled like a digit if it is followed by a digit.
% For those locales where a comma is used as a decimal separator.
% Users may want to allow a decimal separator (say ',') but remove 
% the (ten)thousands separator (say '.'). Further coding is required for this.

tokenize_line_x(_, ',', Stream,Dlist) :-
   peek_char(Stream,P),
   char_type_char(P,digit,_),
   !,
   % Start over, classifying ',' as a digit
   tokenize_line_x(digit, ',', Stream,Dlist).


% Special characters and unidentified characters are easy:
% they stand by themselves, and the next token begins with
% the very next character.

tokenize_line_x(special,Char,Stream,[s(Char)|Tokens]/Tail) :-   % special char
   !,
   tokenize_line_dl(Stream,Tokens/Tail).

tokenize_line_x(_,Char,Stream,[other(Char)|Tokens]/Tail) :-     % unidentified char
   !,
   tokenize_line_dl(Stream,Tokens/Tail).



% tokenize_letters(+Type,+Char,+Stream,-Token,-NewChar,-NewType)
%   Completes a word token beginning with Char, which has
%   been read ahead and identified as type Type.
%   When the process ends, NewChar and NewType are the
%   character that was read ahead after the token.

tokenize_letters(letter,Char,Stream,[Char|Rest],NewType,NewChar) :-
   % It's a letter, so process it, read another character ahead, and recurse.
   !,
   get_char_and_type(Stream,Char2,Type2),
   tokenize_letters(Type2,Char2,Stream,Rest,NewType,NewChar).

tokenize_letters(_,'''',Stream,Rest,NewType,NewChar) :-
   %
   % Absorb an apostrophe, but only when it precedes t.
   % This keeps words together like doesn't, won't.
   %
   peek_char(Stream,t),
   !,
   get_char(Stream,_),
   tokenize_letters(letter,t,Stream,Rest,NewType,NewChar).

tokenize_letters(Type,Char,_,[],Type,Char).
   % It's not a letter, so don't process it; pass it to the calling procedure.


% tokenize_digits(+Type,+Char,+Stream,-Token,-NewChar,-NewType)
%   Like tokenize_letters, but completes a number token instead.
%   Additional subtleties for commas and decimal points.

tokenize_digits(digit,Char,Stream,[Char|Rest],NewType,NewChar) :-
   % It's a digit, so process it, read another character ahead, and recurse.
   !,
   get_char_and_type(Stream,Char2,Type2),
   tokenize_digits(Type2,Char2,Stream,Rest,NewType,NewChar).

tokenize_digits(_, '.', Stream,['.'|Rest],NewType,NewChar) :-
   peek_char(Stream,P),
   char_type_char(P,digit,Char2),
   !,
   % It's a period followed by a digit, so include it and continue.
   get_char(Stream,_),
   tokenize_digits(digit,Char2,Stream,Rest,NewType,NewChar).

% Donald added this clause for commas in numbers:

tokenize_digits(_, ',', Stream,[','|Rest],NewType,NewChar) :-
   peek_char(Stream,P),
   char_type_char(P,digit,Char2),
   !,
   % It's a comma followed by a digit, so include it and continue.
   get_char(Stream,_),
   tokenize_digits(digit,Char2,Stream,Rest,NewType,NewChar).

tokenize_digits(Type,Char,_,[],Type,Char).
   % It's not any of those, so don't process it;
   % pass it to the calling procedure.


%%
%% Character classification
%%

% char_type_char(+Char,-Type,-TranslatedChar)
%   Classifies all characters as letter, digit, special, etc.,
%   and also translates each character into the character that
%   will represent it, converting upper to lower case.
% Recognize as letters only UTF-8 and WIN-1257 letters. (Guntis)

char_type_char(Char,letter,Char2) :-
   charsok(ANY,LOW),
   atom_codes(Char,[C1]),
   nth1(N,ANY,C1),
   nth1(N,LOW,C2),
   atom_codes(Char2,[C2]),
   !.

char_type_char(Char,Type,Tr) :-
   char_table(Char,Type,Tr),
   !.

% Anything else is a special character. (Guntis)
char_type_char(Char,special,Char).
 

% End of line marks
char_table(end_of_file, eol, end_of_file).
char_table('\n',        eol, '\n'       ).

% Whitespace characters
char_table(' ',     whitespace,  ' ').     % blank
char_table('\t',    whitespace,  ' ').     % tab
char_table('\r',    whitespace,  ' ').     % return
char_table('''',    whitespace, '''').     % apostrophe does not translate to blank

% Donald removed the letter characters and replaced them by special characters.
% There are too many Unicode letters to put them all in a table.
% The third parameter may be useless, but maybe someone will want to convert
% some of the special characters.
% There may be other Unicode characters that need to be added.

char_table('~',     special,    '~' ).
char_table('`',     special,    '`' ).
char_table('!',     special,    '!' ).
char_table('@',     special,    '@' ).
char_table('#',     special,    '#' ).
char_table('$',     special,    '$' ).
char_table('\u0025',special,    '\u0025' ). %
char_table('^',     special,    '^' ).
char_table('&',     special,    '&' ).
char_table('*',     special,    '*' ).
char_table('(',     special,    '(' ).
char_table(')',     special,    ')' ).
char_table('_',     special,    '_' ).
char_table('-',     special,    '-' ).
char_table('+',     special,    '+' ).
char_table('=',     special,    '=' ).
char_table('{',     special,    '{' ).
char_table('[',     special,    '[' ).
char_table('}',     special,    '}' ).
char_table(']',     special,    ']' ).
char_table('|',     special,    '|' ).
char_table('\\',    special,    '\\' ).
char_table(':',     special,    ':' ).
char_table(';',     special,    ';' ).
char_table('"',     special,    '"' ).
char_table('<',     special,    '<' ).
char_table(',',     special,    ',' ).
char_table('>',     special,    '>' ).
char_table('.',     special,    '.' ).
char_table('?',     special,    '?' ).
char_table('/',     special,    '/' ).

% Digits
char_table('0',   digit,     '0' ).
char_table('1',   digit,     '1' ).
char_table('2',   digit,     '2' ).
char_table('3',   digit,     '3' ).
char_table('4',   digit,     '4' ).
char_table('5',   digit,     '5' ).
char_table('6',   digit,     '6' ).
char_table('7',   digit,     '7' ).
char_table('8',   digit,     '8' ).
char_table('9',   digit,     '9' ).

% Everything else is a letter character.


charsok(A, B) :-
	C=[65, 194, 66, 67, 200, 68, 69, 199, 70, 71, 204, 72, 73, 206, 74, 75, 205, 76, 207, 77, 78, 210, 79, 80, 81, 82, 83, 208, 84, 85, 219, 86, 90, 222, 88, 89, 90, 87, 97, 226, 98, 99, 232, 100, 101, 231, 102, 103, 236, 104, 105, 238, 106, 107, 237, 108, 239, 109, 110, 242, 111, 112, 113, 114, 115, 240, 116, 117, 251, 118, 122, 254, 120, 121, 122, 119],
	D=[97, 257, 98, 99, 269, 100, 101, 275, 102, 103, 291, 104, 105, 299, 106, 107, 311, 108, 316, 109, 110, 326, 111, 112, 113, 114, 115, 353, 116, 117, 363, 118, 122, 382, 120, 121, 122, 119,97, 257, 98, 99, 269, 100, 101, 275, 102, 103, 291, 104, 105, 299, 106, 107, 311, 108, 316, 109, 110, 326, 111, 112, 113, 114, 115, 353, 116, 117, 363, 118, 122, 382, 120, 121, 122, 119],

	G=[65, 256, 66, 67, 268, 68, 69, 274, 70, 71, 290, 72, 73, 298, 74, 75, 310, 76, 315, 77, 78, 325, 79, 80, 81, 82, 83, 352, 84, 85, 362, 86, 90, 381, 88, 89, 90, 87, 97, 257, 98, 99, 269, 100, 101, 275, 102, 103, 291, 104, 105, 299, 106, 107, 311, 108, 316, 109, 110, 326, 111, 112, 113, 114, 115, 353, 116, 117, 363, 118, 122, 382, 120, 121, 122, 119],
	append(C, G, A),
	append(D, D, B).

