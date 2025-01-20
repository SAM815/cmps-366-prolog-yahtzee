/*
    % This module provides predicates for rolling dice and handling dice-related 
    operations in the Yahtzee game.
*/

% filepath: /d:/OPL/Prolog/dice.pl
:- module(dice, [
    roll_die/1,
    roll_dice/2
]).

% Import the random library
:- use_module(library(random)).

% /* *********************************************************************
% Predicate Name: roll_die/1
% Purpose: Rolls a single die and returns the result as an integer between 1 and 6.
% Parameters:
%     Result: An integer variable to store the rolled value (output).

% Algorithm:
%     1. Use random_between/3 to generate a random integer between 1 and 6.
%     2. Assign the generated value to the Result variable.
% Reference: None
% ********************************************************************* */
roll_die(Result) :-
    random_between(1, 6, Result).

% /* *********************************************************************
% Predicate Name: roll_dice/2
% Purpose: Rolls a specified number of dice and returns the results in a list.
% Parameters:
%     NumDice: An integer representing the number of dice to roll (input).
%     Results: A list containing the rolled values for each die (output).
% 
% Algorithm:
%     1. Base case: If NumDice is 0, return an empty list.
%     2. Recursive case:
%         - Generate a random value for one die using roll_die/1 and store it in Roll.
%         - Decrement NumDice by 1 to represent the remaining dice to roll.
%         - Recursively call roll_dice with the new NumDice and an empty results list.
%         - Prepend the current Roll value to the list returned by the recursive call.
% Reference: None
% ********************************************************************* */
roll_dice(0, []).
roll_dice(NumDice, [Roll|Rolls]) :-
    NumDice > 0,
    roll_die(Roll),
    NewNumDice is NumDice - 1,
    roll_dice(NewNumDice, Rolls).