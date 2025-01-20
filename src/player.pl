/*
 This predicate checks if the given Player wants help and unifies the result with WantsHelp.
*/

% filepath: /d:/OPL/Prolog/player.pl


:- module(player, [
    initialize_players/1,
    get_name/2,
    toss_die/2,
    get_dice_roll/3,
    get_dice_to_keep/4,
   
    wants_to_stand/4,
    wants_help/2,
    inform/2,
    is_human/1
]).

:- use_module(library(lists)).
:- use_module(iofunctions).
:- use_module(scorecategory).
:- use_module(iofunctions).
:- use_module(helpers).



/* *********************************************************************
   Predicate Name: initialize_players/1
   Purpose: Initializes the list of players (human and computer).
   Parameters:
               Players -  Output parameter unified with a list containing the players ( [human, computer] ).
   
   Algorithm:
               1) Unifies the Players variable with the list [human, computer].
   Reference: None
********************************************************************* */
initialize_players([human, computer]).


/* *********************************************************************
   Predicate Name: get_name/2
   Purpose: Gets the name of the player.
   Parameters:
               Player - The player (human or computer).
               Name - Output parameter unified with the name of the player ("Human" or "Computer").
   
   Algorithm:
               1) If the Player is human, unifies Name with "Human".
               2) If the Player is computer, unifies Name with "Computer".
   Reference: None
********************************************************************* */
get_name(human, "Human").
get_name(computer, "Computer").

/* *********************************************************************
   Predicate Name: toss_die/2
   Purpose: Simulates tossing a die with a given prompt.
   Parameters:
               Prompt - The prompt to display to the user before rolling the die.
               Roll - Output parameter unified with the result of the die roll (an integer between 1 and 6).
   
   Algorithm:
               1) Displays the Prompt to the user.
               2) Calls the iofunctions:get_die_roll/1 predicate to get a random die roll.
   Reference: None
********************************************************************* */
toss_die(Prompt, Roll) :-
    writeln(Prompt),
    iofunctions:get_die_roll(Roll).

/* *********************************************************************
   Predicate Name: get_dice_roll/3
   Purpose: Retrieves the dice rolls for the specified player.
   Parameters:
               Player - The player (human or computer).
               NumDice -  The number of dice to roll.
               DiceRolls - Output parameter unified with a list of the dice roll results.
   
   Algorithm:
               1) Calls the iofunctions:get_dice_roll/2 predicate to generate the dice rolls.
   Reference: None
********************************************************************* */
get_dice_roll(_, NumDice, DiceRolls) :-
    iofunctions:get_dice_roll(NumDice, DiceRolls).

/* *********************************************************************
   Predicate Name: get_dice_to_keep/4
   Purpose: Determines which dice to keep for the player based on the current scorecard and dice rolls.
   Parameters:
               Player - The player (human or computer).
               ScoreCard - The player's current scorecard. 
               DiceRolls - A list of the current dice rolls.
               DiceToKeep - Output parameter unified with a list of the dice to keep.
   
   Algorithm:
               1) Displays the current dice rolls to the user.
               2) Calls the iofunctions:get_dice_to_keep/2 predicate to get the dice to keep from the user.
   Reference: None
********************************************************************* */
get_dice_to_keep(_, _, DiceRolls, DiceToKeep) :-
    format("Current dice rolls: ~w~n", [DiceRolls]),
    iofunctions:get_dice_to_keep(DiceRolls, DiceToKeep).



/* *********************************************************************
   Predicate Name: wants_to_stand/5
   Purpose: Checks if the player wants to stand based on their current scorecard, kept dice, and dice rolls.
   Parameters:
               Player - The player (human or computer).
               ScoreCard - The player's current scorecard. 
               KeptDice - A list of the dice the player has chosen to keep.
               DiceRolls - A list of the current dice rolls.
               WantsToStand - Output parameter unified with true if the player wants to stand, false otherwise. 
  
   Algorithm:
               1) Displays the kept dice and current dice rolls to the user.
               2) Calls the iofunctions:wants_to_stand/1 predicate to get the user input.
   Reference: None
********************************************************************* */
wants_to_stand(_, _, KeptDice, DiceRolls, WantsToStand) :-
    format("Kept dice: ~w~n", [KeptDice]),
    format("Current dice rolls: ~w~n", [DiceRolls]),
    iofunctions:wants_to_stand(WantsToStand).

/* *********************************************************************
   Predicate Name: wants_help/2
   Purpose: Checks if the given Player wants help and unifies the result with WantsHelp.
   Parameters:
               Player - The player (human or computer).
               WantsHelp - Output parameter unified with true if the player wants help, false otherwise. 
 
   Algorithm:
               1) Calls the iofunctions:wants_help/1 predicate to get the user input.
   Reference: None
********************************************************************* */
wants_help(Player, WantsHelp) :-
    writeln('Inside wants_help in player.pl'),
    iofunctions:wants_help(WantsHelp).

/* *********************************************************************
   Predicate Name: inform/2
   Purpose: Informs the specified player with the given message.
   Parameters:
               Player - The player (human or computer).
               Message - The message to display to the player. 
   
   Algorithm:
               1) Gets the name of the Player using the get_name/2 predicate.
               2) Displays the Message to the player with their name.
   Reference: None
********************************************************************* */
inform(Player, Message) :-
    get_name(Player, Name),
    format("~w: ~w~n", [Name, Message]).

/* *********************************************************************
   Predicate Name: is_human/1
   Purpose: Checks if the given player is human.
   Parameters:
               Player - The player (human or computer).
   
   Algorithm:
               1) If the Player is human, succeeds immediately using the cut (!).
               2) Otherwise, fails.
   Reference: None
********************************************************************* */
is_human(human) :- !.
is_human(_) :- false.