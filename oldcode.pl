best_next_word([], _, _).
best_next_word([Word|Words], Slots, BestWord) :-
    slots_matching_word(Word, Slots, Matches),
    best_next_word(Words, Slots, Matches, Word, BestWord).

best_next_word([], _, _, BestWord, BestWord).
best_next_word([Word|Words], Slots, LowestMatches, CurrentBestWord, BestWord) :-
    slots_matching_word(Word, Slots, Count),
    (Count < LowestMatches ->
        CurrentBestWord1 = Word,
        LowestMatches1 = Count
    ;   CurrentBestWord1 = CurrentBestWord,
        LowestMatches1 = LowestMatches
    ),
    best_next_word(Words, Slots, LowestMatches1, CurrentBestWord1, BestWord).

slots_matching_word(Word, Slots, Matches) :-
    slots_matching_word(Word, Slots, 0, Matches).

slots_matching_word(_, [], Acc, Acc).
slots_matching_word(Word, [Slot|Slots], Acc, Count) :-
    (Word \= Slot ->
        Acc1 is Acc
    ;   Acc1 is Acc + 1
    ), 
    slots_matching_word(Word, Slots, Acc1, Count).

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


% any better ways to do this? have to make final accumulator into list because
% there isn't another # to signal the end of the slot at the end of the row
% Note: this causes issues if row starts with a #
% E.g. [#,X,#] returns [[], [X]]

slots_from_row([], Acc, Slots) :-
    Acc \= [],
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


puzzle_with_vars([], []).
puzzle_with_vars([Row|Rows], [RowWithVars|PuzzleWithVars]) :-
    /*row_to_vars(Row, RowWithVars),
    puzzle_with_vars(Rows, PuzzleWithVars).*/


% slots_from_all_rows(Rows, Slots)
% should hold when Slots is a list of all the slots
% in all of the Rows. Rows is a list of lists of characters,
% one list per row in the puzzle.
% e.g. [['#', X, '#'], [A,B,C], ['#', Z, '#']] returns [[A,B,C], [X,B,Z]]
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
