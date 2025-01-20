/*
% This module handles the logic for playing a turn in the Yahtzee game, including rolling dice, updating kept dice, and determining the final kept dice.
*/

% filepath: /d:/OPL/Prolog/turn.pl


:- module(turn, [
  play_turn/3
]).

:- use_module(scorecard).
:- use_module(player).
:- use_module(iofunctions).
:- use_module(helpers).
:- use_module(computer).

:- discontiguous turn:wants_help/2.


/* *********************************************************************
Predicate Name: play_turn
Purpose: To simulate the player's turn by rolling dice, updating kept dice, and determining the final kept dice.
Parameters:
            Player, the player taking the turn.
            ScoreCard, the current scorecard of the game.
            FinalKeptDice, the final state of the kept dice after the turn.
Return Value: None
Algorithm:
            1) Initialize the turn with an empty list of kept dice and start with the first roll.
            2) Roll the dice, update the kept dice, and determine the final kept dice.
Reference: none
********************************************************************* */
play_turn(Player, ScoreCard, FinalKeptDice) :-
  play_turn(Player, ScoreCard, [], 1, FinalKeptDice).

/* *********************************************************************
Predicate Name: play_turn
Purpose: Helper predicate to simulate the player's turn with the current roll number, updating kept dice and final state.
Parameters:
            Player, the player taking the turn.
            ScoreCard, the current scorecard of the game.
            KeptDice, the dice that have been kept so far.
            CurrentRoll, the current roll number.
            FinalKeptDice, the final state of the kept dice after the turn.

Algorithm:
            1) If the current roll number is 4, end the turn.
            2) Otherwise, roll the dice, update the kept dice, and determine the final kept dice.
Reference: none
********************************************************************* */
play_turn(_, _, KeptDice, 4, KeptDice) :-
  % End of turn after 3 rolls
  writeln('End of turn.'),
  !.
play_turn(Player, ScoreCard, KeptDice, CurrentRoll, FinalKeptDice) :-
  format('\nRoll ~w of 3~n', [CurrentRoll]),

  % Show potential categories based on kept dice
  writeln('\nAvailable Categories: '),
  scorecard:get_possible_categories(ScoreCard, KeptDice, PotentialCategories),
  iofunctions:show_categories(PotentialCategories),

  player:get_name(Player, PlayerName),
  helpers:to_string_vector(KeptDice, KeptDiceString),
  format("~ws current dice: ~w~n", [PlayerName, KeptDiceString]),

  % Get new dice rolls from the player (roll only the dice not kept)
  length(KeptDice, KeptDiceLength),
  NumDiceToRoll is 5 - KeptDiceLength,
  ( player:is_human(Player) ->
      player:get_dice_roll(Player, NumDiceToRoll, DiceRolls)
  ; computer:get_dice_roll(Player, NumDiceToRoll, DiceRolls)
  ),
  helpers:to_string_vector(DiceRolls, DiceRollsString),
  format("~w rolled: ~w~n", [PlayerName, DiceRollsString]),

  % Recalculate potential categories with the newly rolled dice
  append(KeptDice, DiceRolls, NewKeptDice),
  scorecard:get_possible_categories(ScoreCard, NewKeptDice, UpdatedPotentialCategories),
  writeln('Potential categories:'),
  iofunctions:show_categories(UpdatedPotentialCategories),

 % Check if this is the third roll
 ( CurrentRoll =:= 3 ->
    writeln('End of turn.'),
    append(KeptDice, DiceRolls, FinalKeptDice),
    format("Final kept dice: ~w~n", [FinalKeptDice]) 
; 
    % Check if the player is human and ask if they need help
    ( player:is_human(Player) ->
        % Check if the player wants help
        % Check if the player wants help
        iofunctions:get_yes_no('Do you need Help?', Response),
        ( Response ->
            computer:display_help(ScoreCard, DiceRolls, KeptDice)
        ; true
        ),

        
        % Check if the player decides to "stand" and keep their dice (ending the turn early)
        player:wants_to_stand(Player, ScoreCard, KeptDice, DiceRolls, WantsToStand),
        ( WantsToStand ->
            format("~w chose to stand.~n", [PlayerName]),
            append(KeptDice, DiceRolls, FinalKeptDice),
            format("Final kept dice: ~w~n", [FinalKeptDice]) 
        ; % Ask the player which dice they want to keep
            iofunctions:get_dice_to_keep(Player, ScoreCard, DiceRolls, DiceToKeep),
            helpers:to_string_vector(DiceToKeep, DiceToKeepString),
            format("~w kept: ~w~n", [PlayerName, DiceToKeepString]),
            append(KeptDice, DiceToKeep, NewKeptDice2),
            % If all 5 dice are kept, the turn ends
            writeln('Checking if all dice are kept...'),
            ( length(NewKeptDice2, 5) ->
                writeln('All dice kept. End of turn.'),
                FinalKeptDice = NewKeptDice2,
                format("Final kept dice: ~w~n", [FinalKeptDice]) 
            ; % Continue to the next roll
                NextRoll is CurrentRoll + 1,
                format("Next roll: ~w~n", [NextRoll]),
                play_turn(Player, ScoreCard, NewKeptDice2, NextRoll, FinalKeptDice)
            )
        )
    ; % If the player is not human, handle the computer player logic
        computer:wants_to_stand(ScoreCard, KeptDice, DiceRolls, WantsToStand),
        ( WantsToStand ->
            format('Computer chose to stand.~n'),
            append(KeptDice, DiceRolls, FinalKeptDice),
            format("Final kept dice: ~w~n", [FinalKeptDice]) 
        ; % Ask the computer which dice to keep
            computer:get_dice_to_keep(ScoreCard, DiceRolls, KeptDice, DiceToKeep),
            writeln('dice to keep after get_dice_to_keep:'),
            writeln(DiceToKeep),
            helpers:to_string_vector(DiceToKeep, DiceToKeepString),
            format("~w kept: ~w~n", [PlayerName, DiceToKeepString]),
            append(KeptDice, DiceToKeep, NewKeptDice2),
            % If all 5 dice are kept, the turn ends
            writeln('Checking if all dice are kept...'),
            ( length(NewKeptDice2, 5) ->
                writeln('All dice kept. End of turn.'),
                FinalKeptDice = NewKeptDice2,
                format("Final kept dice: ~w~n", [FinalKeptDice]) 
            ; % Continue to the next roll
                NextRoll is CurrentRoll + 1,
                format("Next roll: ~w~n", [NextRoll]),
                play_turn(Player, ScoreCard, NewKeptDice2, NextRoll, FinalKeptDice)
            )
        )
    )
).

/* *********************************************************************
Predicate Name: wants_help
Purpose: To check if the given Player wants help and unify the result with WantsHelp.
Parameters:
            Player, the player in question.
            WantsHelp, the result indicating if the player wants help.

Algorithm:
            1) Use the iofunctions:wants_help predicate to determine if the player wants help.
Reference: none
********************************************************************* */ 
wants_help(_, WantsHelp) :-
    iofunctions:wants_help(WantsHelp).