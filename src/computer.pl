/*
% This module represents the computer player in the Yahtzee game, including predicates for making decisions about dice rolls, keeping dice, and whether to stand, interacting with other modules like player, 
scorecard, helpers, dice, reason, and scorecategory.
*/

% filepath: /d:/OPL/Prolog/computer.pl



:- module(computer, [
    get_dice_roll/3,
    
    get_dice_to_keep/4,
    
    wants_to_stand/4,
   
    display_help/3
]).

:- use_module(player).
:- use_module(scorecard).
:- use_module(helpers).
:- use_module(dice).
:- use_module(reason).
:- use_module(scorecategory).

:- discontiguous computer:get_dice_roll/3.
:- discontiguous computer:get_dice_to_keep/4.
:- discontiguous computer:wants_to_stand/4.


/* *********************************************************************
   Predicate Name: computer/1
   Purpose: Identifies the computer player.
   Parameters:
               Player - Output parameter, unified with the atom 'computer' to represent the computer player.
 
   Algorithm:
               1) Unifies the Player variable with the atom 'computer'.
   Reference: None
********************************************************************* */
computer(computer).


/* *********************************************************************
   Predicate Name: get_dice_roll/3
   Purpose: Gets the dice roll for the computer player. Allows the human 
            player to roll for the computer if they choose to.
   Parameters:
               Player - The computer player.
               NumDice - The number of dice to roll.
               Roll - Output parameter unified with a list of dice roll results.
   
   Algorithm:
               1) Asks the human player if they want to roll for the computer 
                  using `iofunctions:human_wants_to_roll_for_computer/1`.
               2) If the human wants to roll:
                  a) Gets the dice roll from the human player using 
                     `player:get_dice_roll/3`.
               3) Otherwise:
                  a) Generates a random dice roll using `dice:roll_dice/2`.
   Reference: None
********************************************************************* */
get_dice_roll(Player, NumDice, Roll) :-
    
    iofunctions:human_wants_to_roll_for_computer(WantsToRoll),
   
    writeln(WantsToRoll),
    ( WantsToRoll ->
        player:get_dice_roll(Player, NumDice, Roll)
    ; dice:roll_dice(NumDice, Roll)
    ).



/* *********************************************************************
   Predicate Name: check_yahtzee_and_derivatives/4
   Purpose: Checks the current dice rolls and scorecard to determine which 
            dice to keep for maximizing the score in Yahtzee and its 
            derivatives (Four of a Kind, Three of a Kind, and Numbers).
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept (not used in this predicate).
               DiceToKeep - Output parameter unified with a list of the dice to keep.
 
   Algorithm:
               1) Checks if Yahtzee category is not filled in the ScoreCard. 
                  If not filled, keeps all dice to score in Yahtzee.
               2) If Yahtzee is filled, checks if Four of a Kind is not filled. 
                  If not filled, keeps all dice to score in Four of a Kind.
               3) If Yahtzee and Four of a Kind are filled, checks if Three of a 
                  Kind is not filled. If not filled, keeps all dice to score in 
                  Three of a Kind.
               4) If Yahtzee, Four of a Kind, and Three of a Kind are filled, 
                  checks if any Number categories (Ones, Twos, etc.) are not 
                  filled. If any are not filled, keeps all dice to score in 
                  the corresponding Number category.
               5) If all other categories are filled, keeps no dice.
   Reference: None
********************************************************************* */
check_yahtzee_and_derivatives(ScoreCard, DiceRolls, _KeptDice,  DiceToKeep) :-
    writeln('DiceRolls in check_yahtzee_and_derivatives: '),
    writeln(DiceRolls),
    ( \+ scorecard:is_category_filled(ScoreCard, yahtzee) ->
        writeln('Since the current dice Values fulfill the Yahtzee Category: Can earn 50 points'),
        writeln('Thus keeping all the dice, standing, and filling the Yahtzee category.'),
        
        DiceToKeep = DiceRolls
      
    ; \+ scorecard:is_category_filled(ScoreCard, four_of_a_kind) ->
        writeln('The dice values fulfill the Yahtzee category.However, the yahtzee category is already filled.'),
        writeln('Thus keeping all the dice, standing, and filling Four of a Kind.'),
        
        DiceToKeep = DiceRolls
        
    ; \+ scorecard:is_category_filled(ScoreCard, three_of_a_kind) ->
        writeln('The dice values fulfill the Yahtzee category.However, the yahtzee category is already filled.'),
        writeln('Yahtzee and Four of a Kind categories are filled. Filling Three of a Kind.'),
        
        DiceToKeep = DiceRolls
    ; findall(N, (member(N, [1,2,3,4,5,6]), helpers:contains_n(DiceRolls, N), \+ scorecard:is_category_filled(ScoreCard, N)), Numbers),
      Numbers \= [] ->
        writeln('Yahtzee, Four of a Kind, and Three of a Kind categories are filled. Filling Numbers category.'),
        DiceToKeep = DiceRolls
    ; writeln('Yahtzee, Four of a Kind, Three of a Kind, and Numbers categories are filled.'),
      DiceToKeep = []
    ).

/* *********************************************************************
   Predicate Name: check_five_straight/4
   Purpose: Checks if there is a five-straight (a sequence of five 
            consecutive numbers) in the given dice rolls. If a five-straight 
            is found, it determines which dice to keep for the best possible 
            score.
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept (not used in this predicate).
               DiceToKeep - Output parameter unified with a list of the dice to keep.
 
   Algorithm:
               1) Checks if the Five Straight category is not filled in the ScoreCard. 
                  If not filled, keeps all dice to score in Five Straight.
               2) If Five Straight is filled, checks if Four Straight is not filled. 
                  If not filled, keeps all dice to score in Four Straight.
               3) If both Five Straight and Four Straight are filled, keeps no dice.
   Reference: None
********************************************************************* */
check_five_straight(ScoreCard, DiceRolls, _KeptDice, DiceToKeep) :-
    ( \+ scorecard:is_category_filled(ScoreCard, five_straight) ->
        writeln('Dice Values fulfill the Five_Straight Category: Can earn 40 points'),
        writeln('Thus, filling the five_straight category.'),
        
        DiceToKeep = DiceRolls
    ; \+ scorecard:is_category_filled(ScoreCard, four_straight) ->
        writeln('Five_Straight category is filled. Filling Four_Straight to earn 30 points'),
        
        DiceToKeep = DiceRolls
    ; writeln('Both Five_Straight and Four_Straight categories are filled.'),
      DiceToKeep = []
    ).

/* *********************************************************************
   Predicate Name: check_full_house/4
   Purpose: Checks if there is a full house (a combination of three of a kind 
            and a pair) in the given dice rolls. If a full house is found, it 
            determines which dice to keep for the best possible score.
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept (not used in this predicate).
               DiceToKeep - Output parameter unified with a list of the dice to keep.
  
   Algorithm:
               1) Checks if the Full House category is not filled in the ScoreCard. 
                  If not filled, keeps all dice to score in Full House.
               2) If Full House is filled, checks if Three of a Kind is not filled. 
                  If not filled, keeps all dice to score in Three of a Kind.
               3) If both Full House and Three of a Kind are filled, keeps no dice.
   Reference: None
********************************************************************* */
check_full_house(ScoreCard, DiceRolls, _KeptDice, DiceToKeep) :-
    ( \+ scorecard:is_category_filled(ScoreCard, full_house) ->
        writeln('Dice Values fulfill the Full house Category: Can earn 25 points'),
        
        DiceToKeep = DiceRolls
    ; \+ scorecard:is_category_filled(ScoreCard, three_of_a_kind) ->
        writeln('Full house category is filled. Filling Three of a Kind.'),
        
        DiceToKeep = DiceRolls
    ; writeln('Full house and Three of a Kind categories are filled.'),
      DiceToKeep = []
    ).

/* *********************************************************************
   Predicate Name: are_sequences_filled/1
   Purpose: Checks if both 5-straight and 4-straight categories are filled 
            in the scorecard.
   Parameters:
               ScoreCard - The current scorecard.

   Algorithm:
               1) Uses `scorecard:is_category_filled/2` to check if both 
                  `five_straight` and `four_straight` categories are filled.
   Reference: None
********************************************************************* */
are_sequences_filled(ScoreCard) :-
    scorecard:is_category_filled(ScoreCard, five_straight),
    scorecard:is_category_filled(ScoreCard, four_straight).

/* *********************************************************************
   Predicate Name: has_sequence_and_duplicates_categories/1
   Purpose: Checks if the scorecard has one or more sequence or duplicate 
            categories available (not filled).
   Parameters:
               ScoreCard - The current scorecard.
 
   Algorithm:
               1) Uses `are_sequences_filled/1` to check if any sequence 
                  categories are available.
               2) Uses `are_yahtzee_derivatives_filled/1` to check if any 
                  duplicate categories are available.
   Reference:  None
********************************************************************* */
has_sequence_and_duplicates_categories(ScoreCard) :-
    \+ are_sequences_filled(ScoreCard);
    \+ are_yahtzee_derivatives_filled(ScoreCard).

/* *********************************************************************
   Predicate Name: are_yahtzee_derivatives_filled/1
   Purpose: Checks if Yahtzee, 4 of a kind, 3 of a kind, Full House, and 
            all Number categories are filled in the scorecard.
   Parameters:
               ScoreCard - The current scorecard.

   Algorithm:
               1) Uses `scorecard:is_category_filled/2` to check if each of 
                  the specified categories is filled.
   Reference: None
********************************************************************* */
are_yahtzee_derivatives_filled(ScoreCard) :-
    scorecard:is_category_filled(ScoreCard, yahtzee),
    scorecard:is_category_filled(ScoreCard, four_of_a_kind),
    scorecard:is_category_filled(ScoreCard, three_of_a_kind),
    scorecard:is_category_filled(ScoreCard, full_house),
    scorecard:is_category_filled(ScoreCard, ones),
    scorecard:is_category_filled(ScoreCard, twos),
    scorecard:is_category_filled(ScoreCard, threes),
    scorecard:is_category_filled(ScoreCard, fours),
    scorecard:is_category_filled(ScoreCard, fives),
    scorecard:is_category_filled(ScoreCard, sixes).


/* *********************************************************************
   Predicate Name: isOnlyFullHouse/1
   Purpose: Checks if the given ScoreCard has all categories filled except 
            for the Full House category.
   Parameters:
               ScoreCard - The current scorecard.
  
   Algorithm:
               1) Uses `scorecard:is_category_filled/2` to check if all 
                  categories except Full House are filled.
   Reference: None
********************************************************************* */
isOnlyFullHouse(ScoreCard) :-
    scorecard:is_category_filled(ScoreCard, yahtzee),
    scorecard:is_category_filled(ScoreCard, four_of_a_kind),
    scorecard:is_category_filled(ScoreCard, three_of_a_kind),
    scorecard:is_category_filled(ScoreCard, five_straight),
    scorecard:is_category_filled(ScoreCard, four_straight),
    scorecard:is_category_filled(ScoreCard, ones),
    scorecard:is_category_filled(ScoreCard, twos),
    scorecard:is_category_filled(ScoreCard, threes),
    scorecard:is_category_filled(ScoreCard, fours),
    scorecard:is_category_filled(ScoreCard, fives),
    scorecard:is_category_filled(ScoreCard, sixes),
    \+ scorecard:is_category_filled(ScoreCard, full_house).


/* *********************************************************************
   Predicate Name: get_dice_to_keep/4
   Purpose: This is the main decision-making predicate for the computer 
            player. It determines which dice to keep based on the current 
            scorecard, the dice rolls, and the dice already kept. It aims 
            to suggest the best dice to keep for the next roll to maximize 
            the computer's score.
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept.
               DiceToKeep - Output parameter unified with a list of the dice to keep.
   
   Algorithm:
               1) Checks for Yahtzee (5 of a kind) using `helpers:atleast_n_same/2`. 
                  If found, calls `check_yahtzee_and_derivatives/4` to decide 
                  which dice to keep.
               2) Checks for a Five Straight using `helpers:five_sequence/1`. If 
                  found, calls `check_five_straight/4`.
               3) Checks for a Full House using `helpers:full_house/1`. If found, 
                  calls `check_full_house/4`.
               4) Checks if only the Full House category is available using 
                  `isOnlyFullHouse/1`. If true, keeps the highest duplicates using 
                  `keep_fullhouse_highest_duplicates/2`.
               5) Checks if all sequence categories (Five Straight and Four Straight) 
                  are filled using `are_sequences_filled/1`. If true, focuses on 
                  keeping duplicates using `keep_highest_duplicates/3`.
               6) Checks if all Yahtzee derivatives (Yahtzee, Four of a Kind, Three 
                  of a Kind, etc.) are filled using `are_yahtzee_derivatives_filled/1`. 
                  If true, focuses on keeping sequence dice using `keep_sequence_dice/3`.
               7) Checks if there are available sequence or duplicate categories using 
                  `has_sequence_and_duplicates_categories/1`. If true, checks for 
                  sequences in the combined kept and rolled dice. If a sequence is 
                  found, keeps sequence dice; otherwise, keeps highest duplicates.
               8) If none of the above conditions are met, keeps no dice.
   Reference: None
********************************************************************* */
get_dice_to_keep(ScoreCard, DiceRolls, KeptDice, DiceToKeep) :-
    ( helpers:atleast_n_same(DiceRolls, 5) ->
        check_yahtzee_and_derivatives(ScoreCard, DiceRolls, KeptDice, DiceToKeep)
    ; helpers:five_sequence(DiceRolls) ->
        check_five_straight(ScoreCard, DiceRolls, KeptDice, DiceToKeep)
    ; helpers:full_house(DiceRolls) ->
        check_full_house(ScoreCard, DiceRolls, KeptDice, DiceToKeep)
    ; isOnlyFullHouse(ScoreCard) ->
        writeln('Only Full House category is available. Keeping highest duplicates.'),
        keep_fullhouse_highest_duplicates(DiceRolls, DiceToKeep)
    ; are_sequences_filled(ScoreCard) ->
        writeln('All sequence categories: Four_Straight and Five_Straight are filled.'),
        writeln('The computer will focus on keeping the duplicates in order to fill the remaining categories.'),
        keep_highest_duplicates(DiceRolls, KeptDice, DiceToKeep)
    ; are_yahtzee_derivatives_filled(ScoreCard) ->
        writeln('Only Sequence categories remain. The computer will focus on keeping the sequence dice.'),
        keep_sequence_dice(DiceRolls, KeptDice, DiceToKeep)
    ; has_sequence_and_duplicates_categories(ScoreCard) ->
        writeln('Checking for sequences or duplicates.'),
        append(KeptDice, DiceRolls, NewDice),
        ( has_sequence(NewDice) ->
        
            writeln('Sequence found. Keeping sequence dice, if any.'),
            writeln('Pursuing sequence categories.'),
            keep_sequence_dice(DiceRolls, KeptDice, DiceToKeep)
        ; writeln('No sequence found. Keeping highest duplicates.'),
            writeln('Pursuing duplicate categories.'),
            keep_highest_duplicates(DiceRolls, KeptDice, DiceToKeep)
        )
    ;
        writeln('Neither sequences nor Yahtzee derivatives are fully filled for now.'),
        DiceToKeep = []
    ).



/* *********************************************************************
   Predicate Name: has_sequence/1
   Purpose: Checks if the given list of dice rolls contains a sequence of at 
            least 3 consecutive numbers.
   Parameters:
               DiceRolls - A list of dice rolls.
   
   Algorithm:
               1) Sorts the DiceRolls to make sequence detection easier.
               2) Uses `find_sequences/2` to find all sequences in the sorted list.
               3) Uses `find_longest_sequence/2` to find the longest sequence.
               4) Checks if the length of the longest sequence is greater than 
                  or equal to 3.
   Reference: None
********************************************************************* */
has_sequence(DiceRolls) :-
    
    sort(DiceRolls, SortedDice),
    
    find_sequences(SortedDice, Sequences),
   
    find_longest_sequence(Sequences, LongestSequence),
    
    length(LongestSequence, Length),
    
    Length >= 3.

/* *********************************************************************
   Predicate Name: keep_sequence_dice/3
   Purpose:  Determines which dice to keep to maximize the chances of forming 
             a sequence (straight) in the Yahtzee game.
   Parameters:
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept.
               DiceToKeep - Output parameter unified with a list of the dice to keep.
   
   Algorithm:
               1) If KeptDice is empty:
                  a) Sorts the DiceRolls.
                  b) Finds all sequences in the sorted list using `find_sequences/2`.
                  c) Finds the longest sequence using `find_longest_sequence/2`.
                  d) If the longest sequence has at least 3 dice, keeps those dice; 
                     otherwise, keeps none.
               2) If KeptDice is not empty:
                  a) Finds the dice in DiceRolls that are not in KeptDice.
                  b) Combines KeptDice with the unique dice from DiceRolls.
                  c) Sorts the combined list.
                  d) Finds all sequences in the sorted combined list.
                  e) Finds the longest sequence in the combined list.
                  f) If the longest sequence has at least 3 dice, keeps the dice 
                     from that sequence that are not already in KeptDice; 
                     otherwise, keeps none.
   Reference: None
********************************************************************* */
keep_sequence_dice(DiceRolls, KeptDice, DiceToKeep) :-
    ( KeptDice = [] ->
        sort(DiceRolls, SortedDice),
        
        find_sequences(SortedDice, Sequences),
       
        find_longest_sequence(Sequences, LongestSequence),
        
        ( length(LongestSequence, Length), Length >= 3 ->
            DiceToKeep = LongestSequence
        ; DiceToKeep = []
        )
    ; findall(X, (member(X, DiceRolls), \+ member(X, KeptDice)), UniqueDiceRoll),
        append(KeptDice, UniqueDiceRoll, ConcatenatedDice),
        sort(ConcatenatedDice, SortedConcatenatedDice),
        
        find_sequences(SortedConcatenatedDice, Sequences),
        
        find_longest_sequence(Sequences, LongestSequence),
        
        ( length(LongestSequence, Length), Length >= 3 ->
            subtract(LongestSequence, KeptDice, DiceToKeep)
        ; DiceToKeep = []
        )
    ).

/* *********************************************************************
   Predicate Name: find_sequences/2
   Purpose: Finds all sequences of consecutive numbers within a list of sorted 
            dice values.
   Parameters:
               SortedDice - A list of sorted dice values.
               Sequences - Output parameter unified with a list of lists, where 
                           each inner list represents a sequence of consecutive 
                           numbers found in SortedDice.
  
   Algorithm:
               1) Uses `findall/3` to find all sequences by calling `find_sequence/2` 
                  for each possible sequence in the SortedDice.
   Reference: None
********************************************************************* */
find_sequences(SortedDice, Sequences) :-
    findall(Sequence, find_sequence(SortedDice, Sequence), Sequences).

/* *********************************************************************
   Predicate Name: find_sequence/2
   Purpose: This predicate, when called with a list of sorted dice values, 
            finds a single sequence of consecutive numbers within that list.
   Parameters:
               SortedDice - A list of sorted dice values.
               Sequence - Output parameter unified with a list representing 
                          a sequence of consecutive numbers found in SortedDice.
   
   Algorithm:
               1) Initializes an accumulator (CurrentSequence) to an empty list 
                  and calls `find_sequence/3`.
   Reference: None
********************************************************************* */
find_sequence(SortedDice, Sequence) :-
    find_sequence(SortedDice, [], Sequence).

/* *********************************************************************
   Predicate Name: find_sequence/3
   Purpose:  This predicate recursively traverses a list of sorted dice values 
             to identify a sequence of consecutive numbers.
   Parameters:
               SortedDice -  A list of sorted dice values.
               CurrentSequence - An accumulator that keeps track of the current 
                                 sequence being built.
               Sequence - Output parameter unified with a list representing a 
                          sequence of consecutive numbers found in SortedDice.
   
   Algorithm:
               1) Base Case: If SortedDice is empty, unify the CurrentSequence with 
                  the Sequence.
               2) Recursive Case:
                  a) If CurrentSequence is empty, start a new sequence with the 
                     head of SortedDice.
                  b) If the head of SortedDice is consecutive to the last element 
                     in CurrentSequence, add it to the CurrentSequence.
                  c) If the head is not consecutive but CurrentSequence has at 
                     least 3 elements, unify CurrentSequence with Sequence and 
                     recursively call `find_sequence/3` with the tail of SortedDice 
                     and an empty list as the new CurrentSequence.
                  d) If the head is not consecutive and CurrentSequence has less 
                     than 3 elements, start a new sequence with the head of SortedDice.
                  e) Recursively call `find_sequence/3` with the tail of SortedDice 
                     and the updated CurrentSequence.
   Reference: None
********************************************************************* */
find_sequence([], CurrentSequence, CurrentSequence).
find_sequence([H|T], CurrentSequence, Sequence) :-
    ( CurrentSequence = [] ->
        NewSequence = [H]
    ; last(CurrentSequence, Last),
      H =:= Last + 1 ->
        append(CurrentSequence, [H], NewSequence)
    ; length(CurrentSequence, Length), Length >= 3 ->
        Sequence = CurrentSequence,
        find_sequence(T, [], _)
    ; NewSequence = [H]
    ),
    find_sequence(T, NewSequence, Sequence).


/* *********************************************************************
   Predicate Name: find_longest_sequence/2
   Purpose: Finds the longest sequence from a list of sequences.
   Parameters:
               Sequences - A list of sequences (lists of numbers).
               LongestSequence - Output parameter unified with the longest sequence 
                                 in the input list.
   
   Algorithm:
               1) Sorts the Sequences list in descending order of length using 
                  `sort/4` with the `@>=` comparison operator.
               2) Unifies LongestSequence with the head of the sorted list, which 
                  is the longest sequence.
   Reference: None
********************************************************************* */
find_longest_sequence(Sequences, LongestSequence) :-
    sort(2, @>=, Sequences, [LongestSequence|_]).



/* *********************************************************************
   Predicate Name: find_common/3
   Purpose: Finds all occurrences of the first value in KeptDice within 
            DiceRolls and puts them into the Common list.
   Parameters:
               KeptDice - A list of kept dice values.
               DiceRolls - A list of rolled dice values.
               Common - Output parameter unified with a list containing all 
                        occurrences of the first value in KeptDice within DiceRolls.
  
   Algorithm:
               1) Gets the first value from KeptDice.
               2) Counts the occurrences of that value in DiceRolls using 
                  `count_occurrences/3`.
               3) Creates a list (Common) of the same length as the count, 
                  filled with the value from KeptDice.
   Reference: None
********************************************************************* */
find_common([Value|_], DiceRolls, Common) :-
    count_occurrences(DiceRolls, Value, Count),
    length(Common, Count),
    maplist(=(Value), Common).

/* *********************************************************************
   Predicate Name: keep_fullhouse_highest_duplicates/2
   Purpose: Keeps the highest duplicate dice values from a list of dice rolls.
   Parameters:
               DiceRolls - A list of dice rolls.
               DiceToKeep - Output parameter unified with a list containing 
                            the highest duplicate dice values.
   
   Algorithm:
               1) Finds all values in DiceRolls that occur more than once using 
                  `findall/3` and `count_occurrences/3`.
               2) Sorts the duplicates in descending order.
               3) If there are any duplicates:
                  a) Takes the highest duplicate value.
                  b) Collects all dice in DiceRolls that are equal to the highest 
                     duplicate value and unifies them with DiceToKeep.
               4) If there are no duplicates, unifies DiceToKeep with an empty list.
   Reference: None
********************************************************************* */
keep_fullhouse_highest_duplicates(DiceRolls, DiceToKeep) :-
    findall(Value, (member(Value, DiceRolls), count_occurrences(DiceRolls, Value, Count), Count > 1), Duplicates),
    sort(0, @>=, Duplicates, SortedDuplicates),
    ( SortedDuplicates = [Highest|_] ->
        findall(Value, (member(Value, DiceRolls), Value == Highest), DiceToKeep)
    ; DiceToKeep = []
    ).

/* *********************************************************************
   Predicate Name: keep_highest_duplicates/3
   Purpose: Identifies and keeps the highest duplicates in the given dice rolls, 
            considering the dice that have already been kept.
   Parameters:
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept.
               DiceToKeep - Output parameter unified with a list of the dice to keep.
   
   Algorithm:
               1) If KeptDice is empty:
                  a) Finds all values in DiceRolls that occur more than once.
                  b) Sorts the duplicates in descending order.
                  c) Keeps all dice in DiceRolls that are equal to the highest 
                     duplicate value.
               2) If KeptDice is not empty:
                  a) Finds all occurrences of values in KeptDice within DiceRolls 
                     using `find_common/3`.
                  b) Keeps the common dice found.
   Reference: None
********************************************************************* */
keep_highest_duplicates(DiceRolls, KeptDice, DiceToKeep) :-
    ( KeptDice = [] ->
        % If KeptDice is empty, proceed as usual
        findall(Value, (member(Value, DiceRolls), count_occurrences(DiceRolls, Value, Count), Count > 1), Duplicates),
        sort(0, @>=, Duplicates, SortedDuplicates),
        ( SortedDuplicates = [Highest|_] ->
            findall(Value, (member(Value, DiceRolls), Value == Highest), DiceToKeep)
        ; DiceToKeep = []
        )
    ; % If KeptDice is not empty, update it
        find_common(KeptDice, DiceRolls, Common),
        
        
        DiceToKeep = Common
    ).


/* *********************************************************************
   Predicate Name: count_occurrences/3
   Purpose: Counts the number of times an element occurs in a list.
   Parameters:
               List - The input list.
               Element - The element to count.
               Count - Output parameter unified with the number of times Element 
                       appears in List.
   
   Algorithm:
               1) Uses `include/3` to filter the List, keeping only the elements 
                  that are equal to Element.
               2) Gets the length of the filtered list and unifies it with Count.
   Reference: None
********************************************************************* */
count_occurrences(List, Element, Count) :-
    include(=(Element), List, Filtered),
    length(Filtered, Count).




/* *********************************************************************
   Predicate Name: wants_to_stand/4
   Purpose: Determines whether the computer player wants to stand (stop 
            rolling) based on the current state of the game.
   Parameters:
               ScoreCard - The current scorecard.
               KeptDice - A list of the dice already kept.
               DiceRolls - A list of the current dice rolls.
               WantsToStand - Output parameter unified with true if the 
                              computer wants to stand, false otherwise.
   
   Algorithm:
               1) Calls `get_dice_to_keep/4` to determine the best dice to keep 
                  based on the ScoreCard, DiceRolls, and KeptDice.
               2) If the dice to keep are the same as the current DiceRolls 
                  (meaning no further improvement is likely), set WantsToStand 
                  to true.
               3) Otherwise, set WantsToStand to false.
   Reference: None
********************************************************************* */
wants_to_stand(ScoreCard, KeptDice, DiceRolls, WantsToStand) :-
    get_dice_to_keep(ScoreCard, DiceRolls, KeptDice, DiceToKeep),
    ( helpers:unordered_equal(DiceToKeep, DiceRolls) ->
        WantsToStand = true
    ; WantsToStand = false
    ).



/* *********************************************************************
   Predicate Name: display_help/3
   Purpose: Provides help messages to the human player based on the current 
            state of the game, suggesting which dice to keep or which 
            categories to consider.
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept.
   
   Algorithm:
               1) Checks for Yahtzee using `helpers:atleast_n_same/2`. If found, 
                  calls `help_yahtzee_and_derivatives/3` to provide relevant help.
               2) Checks for Five Straight using `helpers:five_sequence/1`. If found, 
                  calls `help_five_straight/3`.
               3) Checks for Full House using `helpers:full_house/1`. If found, 
                  calls `help_full_house/3`.
               4) Checks if all sequence categories are filled using 
                  `are_sequences_filled/1`. If true, suggests focusing on duplicates.
               5) Checks if all Yahtzee derivatives are filled using 
                  `are_yahtzee_derivatives_filled/1`. If true, suggests focusing 
                  on sequence dice.
               6) Checks if there are available sequence or duplicate categories 
                  using `has_sequence_and_duplicates_categories/1`. If true, 
                  provides advice on whether to pursue sequence or duplicate 
                  categories based on the presence of a sequence in the combined 
                  kept and rolled dice.
               7) If none of the above conditions are met, provides a general 
                  message to focus on filling available categories.
   Reference: None
********************************************************************* */
display_help(ScoreCard, DiceRolls, KeptDice) :-
    ( helpers:atleast_n_same(DiceRolls, 5) ->
        help_yahtzee_and_derivatives(ScoreCard, DiceRolls, KeptDice)
    ; helpers:five_sequence(DiceRolls) ->
        help_five_straight(ScoreCard, DiceRolls, KeptDice)
    ; helpers:full_house(DiceRolls) ->
        help_full_house(ScoreCard, DiceRolls, KeptDice)
    ; are_sequences_filled(ScoreCard) ->
        writeln('All sequence categories: Four_Straight and Five_Straight are filled. Focus on keeping duplicates to fill remaining categories.')
    ; are_yahtzee_derivatives_filled(ScoreCard) ->
        writeln('Only sequence categories remain. Focus on keeping sequence dice.')
    ; has_sequence_and_duplicates_categories(ScoreCard) ->
        writeln('Checking for sequences or duplicates.'),
        append(KeptDice, DiceRolls, NewDice),
        ( has_sequence(NewDice) ->
            writeln('Sequence found. Keep sequence dice to pursue sequence categories.')
        ; writeln('No sequence found. Keep highest duplicates to pursue duplicate categories.')
        )
    ;
        writeln('Neither sequences nor Yahtzee derivatives are fully filled for now. Focus on filling available categories.')
    ).

/* *********************************************************************
   Predicate Name: help_yahtzee_and_derivatives/3
   Purpose: Provides specific help messages related to Yahtzee and its 
            derivative categories (Four of a Kind, Three of a Kind, Numbers).
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept (not used in this predicate).
 
   Algorithm:
               1) Checks if the Yahtzee category is not filled. If not, suggests 
                  keeping all the dice to score in Yahtzee.
               2) If Yahtzee is filled, checks if Four of a Kind is not filled. 
                  If not, suggests keeping all the dice to score in Four of a Kind.
               3) If Yahtzee and Four of a Kind are filled, checks if Three of a 
                  Kind is not filled. If not, suggests keeping all the dice to 
                  score in Three of a Kind.
               4) If Yahtzee, Four of a Kind, and Three of a Kind are filled, 
                  checks if any Number categories are not filled. If any are 
                  not filled, suggests keeping all the dice to score in the 
                  corresponding Number category.
               5) If all other categories are filled, provides a message 
                  indicating that all relevant categories are filled.
   Reference: None
********************************************************************* */
help_yahtzee_and_derivatives(ScoreCard, DiceRolls, _KeptDice) :-
    ( \+ scorecard:is_category_filled(ScoreCard, yahtzee) ->
        writeln('Since the current dice values fulfill the Yahtzee category, you can earn 50 points. Keep all the dice and fill the Yahtzee category.')
    ; \+ scorecard:is_category_filled(ScoreCard, four_of_a_kind) ->
        writeln('The dice values fulfill the Yahtzee category, but the Yahtzee category is already filled. Keep all the dice and fill Four of a Kind.')
    ; \+ scorecard:is_category_filled(ScoreCard, three_of_a_kind) ->
        writeln('The dice values fulfill the Yahtzee category, but Yahtzee and Four of a Kind categories are filled. Fill Three of a Kind.')
    ; findall(N, (member(N, [1,2,3,4,5,6]), helpers:contains_n(DiceRolls, N), \+ scorecard:is_category_filled(ScoreCard, N)), Numbers),
      Numbers \= [] ->
        writeln('Yahtzee, Four of a Kind, and Three of a Kind categories are filled. Fill Numbers category.')
    ; writeln('Yahtzee, Four of a Kind, Three of a Kind, and Numbers categories are filled.')
    ).

/* *********************************************************************
   Predicate Name: help_five_straight/3
   Purpose: Provides help messages related to the Five Straight category.
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls.
               KeptDice - A list of the dice already kept (not used in this predicate).
   
   Algorithm:
               1) Checks if the Five Straight category is not filled. If not, 
                  suggests keeping all the dice to score in Five Straight.
               2) If Five Straight is filled, checks if Four Straight is not 
                  filled. If not, suggests keeping all the dice to score in 
                  Four Straight.
               3) If both Five Straight and Four Straight are filled, provides 
                  a message indicating that both categories are filled.
   Reference: None
********************************************************************* */
help_five_straight(ScoreCard, DiceRolls, _KeptDice) :-
    ( \+ scorecard:is_category_filled(ScoreCard, five_straight) ->
        writeln('Dice values fulfill the Five Straight category. You can earn 40 points. Fill the Five Straight category.')
    ; \+ scorecard:is_category_filled(ScoreCard, four_straight) ->
        writeln('Five Straight category is filled. Fill Four Straight to earn 30 points.')
    ; writeln('Both Five Straight and Four Straight categories are filled.')
    ).


/* *********************************************************************
   Predicate Name: help_full_house/3
   Purpose: Provides help messages related to the Full House category.
   Parameters:
               ScoreCard - The current scorecard.
               DiceRolls - A list of the current dice rolls (not used in this predicate).
               KeptDice - A list of the dice already kept (not used in this predicate).
   
   Algorithm:
               1) Checks if the Full House category is not filled. If not, 
                  suggests keeping all the dice to score in Full House.
               2) If Full House is filled, checks if Three of a Kind is not 
                  filled. If not, suggests keeping all the dice to score in 
                  Three of a Kind.
               3) If both Full House and Three of a Kind are filled, provides 
                  a message indicating that both categories are filled.
   Reference: None
********************************************************************* */
help_full_house(ScoreCard, _DiceRolls, _KeptDice) :-
    ( \+ scorecard:is_category_filled(ScoreCard, full_house) ->
        writeln('Dice values fulfill the Full House category. You can earn 25 points. Fill the Full House category.')
    ; \+ scorecard:is_category_filled(ScoreCard, three_of_a_kind) ->
        writeln('Full House category is filled. Fill Three of a Kind.')
    ; writeln('Full House and Three of a Kind categories are filled.')
    ).

