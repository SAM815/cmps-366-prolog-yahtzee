/*
% This module represents the human player in the Yahtzee game. It includes predicates for identifying the human player and interacting with the player module.*/

% filepath: /d:/OPL/Prolog/human.pl
:- module(human, [
    human/1
]).

:- use_module(player).

/* *********************************************************************
   Predicate Name: human/1
   Purpose: Identifies the human player.
   Parameters:
               Player - Output parameter, unified with the atom 'human' to represent the human player.
  
   Algorithm:
               1) Unifies the Player variable with the atom 'human'.
   Reference: None
********************************************************************* */
human(human).

