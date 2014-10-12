:- ensure_loaded(library(clpfd)).

% You can use this code to get your started with your fillin puzzle solver.

main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).

print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

%%  doesn't work
solve_puzzle(Puzzle0, [X|Xs], Puzzle) :-
    puzzle_to_vars(Puzzle0, Puzzle0Vars),
    word_into_puzzle(Puzzle0Vars, X, PuzzleWithWord).


%%  doesn't work
word_into_puzzle([], _, _Puzzle).
word_into_puzzle([X|Xs], Word, Puzzle) :-
    (X = Word ->
        Puzzle = [X|Puzzle1]
    ;   Puzzle1 = Puzzle
    ),
    word_into_puzzle(Xs, Word, Puzzle1).
    



puzzle_to_vars([], []).
puzzle_to_vars([X|Xs], [Y|PuzzleWithVars]) :-
    row_to_vars(X, Y),
    puzzle_to_vars(Xs, PuzzleWithVars).


% generate_slots(PuzzleWithVars, Acc, Slots)
%% generate the slots for the Rows, i.e. split on hashes
%% should return list of slots
%% e.g. [['#', X, '#'], [A,B,C], ['#', Z, '#']] returns [[A,B,C], [X,B,Z]]
%% probably ignores slots of size 1
slots_from_row([], Acc, [Acc]).
slots_from_row([X|Xs], Acc, Slots) :-
    (X == '#' ->
        Slots = [Acc|Slots1],
        Acc1 = []
    ;   append(Acc, [X], Acc1),
        Slots1 = Slots
    ),
    slots_from_row(Xs, Acc1, Slots1).
    


not_small(List) :-
    length(List, Len),
    Len > 1.

prune_small_slots(Slots, Pruned) :-
    include(not_small, Slots, Pruned).
    

% row_to_vars(Row, RowWithVars)
% should hold when Row is a row of an unfilled puzzle.
% RowWithVars should be the same as Row but
% should change all the _ in the puzzle to logical variables.

% Why do I get X = [a, b, c, #, d, e|c] if I have _X as second arg in this?
% test case is row_to_vars(['_','_','_',#,'_','_'], X).
%% Can probably redo this using maplist and writing a predicate? to replace '_' with vars
row_to_vars([], []).
row_to_vars([X|Xs], RowWithVars) :-
    (X = '_' ->
        RowWithVars = [_A|RowWithVars1]
    ;   RowWithVars = [X|RowWithVars1]
    ),
    row_to_vars(Xs, RowWithVars1).

/*filter(_, [], []).
filter(P, [X|Xs], Filtered) :-
    ( call(P,X) ->
        Filtered = [X|Filtered1]
    ;   Filtered = Filtered1
    ),
    filter(P, Xs, Filtered1).*/