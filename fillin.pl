%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File   : fillin.pl
% Author : Maxim Lobanov - mlobanov (587697)
% Date   : 13/10/14
% Purpose: Program for Fillin Prolog Project (COMP30020 Declarative
%          Programming, Semester 2 2014). Attempts to solve a fillin
%          crossword puzzle. A fillin puzzle (or fill-it-in) is similar
%          to a crossword puzzle. The player is given a list of all the 
%          words to be placed into the puzzle but not told where they
%          should go. The puzzle itself is a grid of square cells which
%          may be empty, filled with a letter, or solid. An empty cell
%          is able to be filled in with a letter and a solid cell may not
%          be filled in. For more information, read the project specification
%          (fillin.pdf).
%          This file's main predicate is solve_puzzle/3 which takes
%          a puzzle file and a word list and attempts to solve the puzzle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

solve_puzzle(Puzzle0, WordList, Puzzle0WithVars) :-
    puzzle_with_vars(Puzzle0, Puzzle0WithVars),
    slots_from_puzzle(Puzzle0WithVars, Slots),
    fillin_all_words(Slots, WordList).

% fillin_all_words(Slots, WordList)
% should hold when all of the Slots are able to be filled in
% by a word from WordList. Slots is a list of slots in the puzzle
% where words can be placed. WordList is a list of the words
% that are trying to be filled into the puzzle.
fillin_all_words(_, []).
fillin_all_words([], _).
fillin_all_words(Slots, WordList) :-
    best_next_slot(Slots, WordList, BestSlot),
    exclude(\=(BestSlot), WordList, MatchingSlots),
    member(Word, MatchingSlots),
    BestSlot = Word,
    exclude(==(Word), WordList, RemainingWords),
    exclude(==(BestSlot), Slots, RemainingSlots),
    fillin_all_words(RemainingSlots, RemainingWords).

% best_next_slot(Slots, WordList, BestSlot)
% should hold when BestSlot is the slot with the least
% amount of matching words that can be used to fill it.
% Slots is a list of slots in the puzzle where words may be
% used to fill them in. 
% WordList is a list of words attempting to be filled into
% the slots of the puzzle.
best_next_slot([Slot|Slots], WordList, BestSlot) :-
    words_matching_slot(Slot, WordList, Count),
    best_next_slot(Slots, WordList, Count, Slot, BestSlot).

best_next_slot([], _, _, BestSlot, BestSlot).
best_next_slot([Slot|Slots], WordList, LowestMatches, CurrentBestSlot, BestSlot) :-
    words_matching_slot(Slot, WordList, Count),
    (Count < LowestMatches ->
        CurrentBestSlot1 = Slot,
        LowestMatches1 = Count
    ;   CurrentBestSlot1 = CurrentBestSlot,
        LowestMatches1 = LowestMatches
    ),
    best_next_slot(Slots, WordList, LowestMatches1, CurrentBestSlot1, BestSlot).


% words_matching_slot(Slot, WordList, Count)
% should hold when Count is the number of words
% in WordList which can fit in Slot.
% Slot is a single slot in the puzzle.
% WordList is a list of words in the puzzle which
% are attempting to be filled into the slots of the puzzle.
words_matching_slot(Slot, WordList, Count) :-
    words_matching_slot(Slot, WordList, 0, Count).

words_matching_slot(_, [], Acc, Acc).
words_matching_slot(Slot, [Word|Words], Acc, Count) :-
    (Slot \= Word ->
        Acc1 is Acc
    ;   Acc1 is Acc + 1
    ), words_matching_slot(Slot, Words, Acc1, Count).


% puzzle_with_vars(Puzzle, PuzzleWithVars)
% should hold when PuzzleWithVars is identical to Puzzle
% except all of the _ characters have been replaced with
% logical variables.
puzzle_with_vars([], []).
puzzle_with_vars([Row|Rows], [RowWithVars|PuzzleWithVars]) :-
    row_to_vars(Row, RowWithVars),
    puzzle_with_vars(Rows, PuzzleWithVars).

% slots_from_puzzle(Puzzle, Slots)
% should hold when Slots is a list of all the slots in both the
% rows and columns of the Puzzle.
% Puzzle is a list of lists of characters, one list per puzzle row.
slots_from_puzzle(Puzzle, Slots) :-
    slots_from_all_rows(Puzzle, RowSlots),
    include(not_small, RowSlots, PrunedRowSlots),
    transpose(Puzzle, TransposedPuzzle),
    slots_from_all_rows(TransposedPuzzle, ColumnSlots),
    include(not_small, ColumnSlots, PrunedColumnSlots),
    append(PrunedRowSlots, PrunedColumnSlots, Slots).


% slots_from_all_rows(Rows, Slots)
% should hold when Slots is a list of all the slots
% in all of the Rows. Rows is a list of lists of characters,
% one list per row in the puzzle.
slots_from_all_rows([], []).
slots_from_all_rows([Row|Rows], Slots) :-
    slots_from_row(Row, RowSlots),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Do I have to use append? Using it because using cons
    %% resulted in [[[adasds]].[[asdasdas]]] rather than 
    %% [[asdadasd],[adadadsa]] which is what I want.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    append(RowSlots, Slots1, Slots),
    slots_from_all_rows(Rows, Slots1).

% slots_from_rows(Row, Slots)
% should hold when Slots contains a list of slots present in the Row.
% i.e. Slots is the result of splitting the Row on hashes.
% e.g. [['#', X, '#'], [A,B,C], ['#', Z, '#']] returns [[A,B,C], [X,B,Z]]
slots_from_row(Row, Slots) :-
    slots_from_row(Row, [], Slots).

% any better ways to do this? have to make final accumulator into list because
% there isn't another # to signal the end of the slot at the end of the row
slots_from_row([], Acc, Slots) :-
    length(Acc, Len),
    (Len < 1 ->
        Slots = []
    ;   Slots = [Acc]).
slots_from_row([X|Xs], Acc, Slots) :-
    (X == '#' ->
        Slots = [Acc|Slots1],
        Acc1 = []
    ;   append(Acc, [X], Acc1),
        Slots1 = Slots
    ),
    slots_from_row(Xs, Acc1, Slots1).

% not_small(List)
% holds when the length of the list is greater than 1.
not_small(List) :-
    length(List, Len),
    Len > 1.

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