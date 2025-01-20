/*
% This module provides input/output functions for the Yahtzee game, including manual and automatic dice roll handling.
*/

% filepath: /d:/OPL/Prolog/iofunctions.pl



:- module(iofunctions, [
    get_yes_no/2,
    get_manual_die_roll/1,
    get_auto_die_roll/1,
    get_die_roll/1,
    get_manual_dice_roll/2,
    get_auto_dice_roll/2,
    get_dice_roll/2,
    show_message/1,
    human_won_tie_breaker/1,
    get_dice_to_keep/4,
    to_string_vector/2,
    show_categories/1,
    user_wants_to_load_game/1,
    get_serial/1,
    save_game_procedure/1,
    wants_to_stand/1,
    show_category_pursuits/1,
    wants_help/1,
    human_wants_to_roll_for_computer/1
]).

:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(scorecategory).
:- use_module(player).
:-use_module(game).

:- discontiguous iofunctions:get_dice_to_keep/4.


/* *********************************************************************
   Predicate Name: get_yes_no/2
   Purpose: Prompts the user with a yes/no question and gets their response.
   Parameters:
               Prompt - The question to ask the user.
               Response - Output parameter unified with true if the user 
                          enters 'y', false if the user enters 'n'.

   Algorithm:
               1) Displays the Prompt to the user.
               2) Reads the user input.
               3) If the input is 'y', unifies Response with true.
               4) If the input is 'n', unifies Response with false.
               5) If the input is invalid, displays an error message and 
                  recursively calls `get_yes_no/2`.
   Reference: None
********************************************************************* */
get_yes_no(Prompt, Response) :-
    format("~w (y/n): ", [Prompt]),
    read_line_to_string(user_input, Input),
    ( Input = "y" -> Response = true
    ; Input = "n" -> Response = false
    ; writeln("Invalid response. Please enter 'y' or 'n'."),
      get_yes_no(Prompt, Response)
    ).

/* *********************************************************************
   Predicate Name: get_manual_die_roll/1
   Purpose: Prompts the user to manually enter a die roll value.
   Parameters:
               Roll - Output parameter unified with the entered die roll value.
   
   Algorithm:
               1) Prompts the user to enter the die roll value.
               2) Reads the user input.
               3) If the input is a valid integer between 1 and 6, unifies 
                  it with Roll.
               4) If the input is invalid, displays an error message and 
                  recursively calls `get_manual_die_roll/1`.
   Reference: None
********************************************************************* */
get_manual_die_roll(Roll) :-
    format("Enter the value of the die roll: "),
    read_line_to_string(user_input, Response),
    ( number_string(Roll, Response), Roll >= 1, Roll =< 6 ->
        true
    ; writeln("Invalid response. Please enter an integer between 1 and 6."),
      get_manual_die_roll(Roll)
    ).

/* *********************************************************************
   Predicate Name: get_auto_die_roll/1
   Purpose: Generates an automatic die roll value.
   Parameters:
               Roll - Output parameter unified with the generated die roll value.
  
   Algorithm:
               1) Uses `random_between/3` to generate a random integer between 1 and 6.
   Reference: None
********************************************************************* */
get_auto_die_roll(Roll) :-
    random_between(1, 6, Roll).

/* *********************************************************************
   Predicate Name: get_die_roll/1
   Purpose: Gets a die roll value, either manually or automatically, 
            based on user preference.
   Parameters:
               Roll - Output parameter unified with the die roll value.
  
   Algorithm:
               1) Asks the user if they want to roll manually using `get_yes_no/2`.
               2) If the user wants to roll manually, calls `get_manual_die_roll/1`.
               3) Otherwise, calls `get_auto_die_roll/1`.
   Reference: None
********************************************************************* */
get_die_roll(Roll) :-
    get_yes_no("Would you like to manually roll the die?", ManualRoll),
    ( ManualRoll ->
        get_manual_die_roll(Roll)
    ; get_auto_die_roll(Roll)
    ).


/* *********************************************************************
   Predicate Name: get_manual_dice_roll/2
   Purpose: Prompts the user to manually enter multiple die roll values.
   Parameters:
               NumDice - The number of dice to roll.
               DiceRolls - Output parameter unified with a list of the entered 
                           die roll values.
   
   Algorithm:
               1) Prompts the user to enter the die roll values.
               2) Reads the user input.
               3) Splits the input string into a list of tokens.
               4) Converts the tokens to numbers.
               5) If the input is valid (correct number of dice, all values 
                  between 1 and 6), unifies the list of numbers with DiceRolls.
               6) If the input is invalid, displays an error message and 
                  recursively calls `get_manual_dice_roll/2`.
   Reference: None
********************************************************************* */
get_manual_dice_roll(NumDice, DiceRolls) :-
    format("Enter ~w dice rolls separated by spaces: ", [NumDice]),
    read_line_to_string(user_input, Response),
    split_string(Response, " ", "", Tokens),
    ( maplist(atom_number, Tokens, DiceRolls), % Ensure all tokens are numeric
      length(DiceRolls, NumDice),
      forall(member(Roll, DiceRolls), (integer(Roll), Roll >= 1, Roll =< 6)) ->
        true
    ; writeln("Invalid response. Please enter integers between 1 and 6."),
      get_manual_dice_roll(NumDice, DiceRolls)
    ).

/* *********************************************************************
   Predicate Name: get_auto_dice_roll/2
   Purpose: Generates multiple automatic die roll values.
   Parameters:
               NumDice - The number of dice to roll.
               DiceRolls - Output parameter unified with a list of the 
                           generated die roll values.
   
   Algorithm:
               1) Uses `findall/3`, `between/3`, and `get_auto_die_roll/1` 
                  to generate the required number of die rolls.
   Reference: None
********************************************************************* */
get_auto_dice_roll(NumDice, DiceRolls) :-
    findall(Roll, (between(1, NumDice, _), get_auto_die_roll(Roll)), DiceRolls).

/* *********************************************************************
   Predicate Name: get_dice_roll/2
   Purpose: Gets multiple die roll values, either manually or automatically, 
            based on user preference.
   Parameters:
               NumDice - The number of dice to roll.
               DiceRolls - Output parameter unified with a list of the die 
                           roll values.
   
   Algorithm:
               1) Asks the user if they want to roll manually using `get_yes_no/2`.
               2) If the user wants to roll manually, calls `get_manual_dice_roll/2`.
               3) Otherwise, calls `get_auto_dice_roll/2`.
   Reference: None
********************************************************************* */
get_dice_roll(NumDice, DiceRolls) :-
    get_yes_no('Would you like to manually roll the dice?', ManualRoll),
    ( ManualRoll ->
        get_manual_dice_roll(NumDice, DiceRolls)
    ; get_auto_dice_roll(NumDice, DiceRolls)
    ).

/* *********************************************************************
   Predicate Name: show_message/1
   Purpose: Displays a message to the user.
   Parameters:
               Message - The message to display.
   
   Algorithm:
               1) Uses `writeln/1` to print the message to the console.
   Reference: None
********************************************************************* */
show_message(Message) :-
    writeln(Message).

/* *********************************************************************
   Predicate Name: human_wants_to_roll_for_computer/1
   Purpose: Asks the user if they want to roll the dice for the computer player.
   Parameters:
               WantsToRoll - Output parameter unified with true if the user 
                              wants to roll for the computer, false otherwise.
   
   Algorithm:
               1) Uses `get_yes_no/2` to get the user's response.
   Reference: None
********************************************************************* */
human_wants_to_roll_for_computer(WantsToRoll) :-
    get_yes_no("Would you like to roll for the computer?", WantsToRoll).

/* *********************************************************************
   Predicate Name: human_won_tie_breaker/1
   Purpose: Simulates a tie-breaking die roll between the human and computer 
            players to determine who goes first.
   Parameters:
               HumanWon - Output parameter unified with true if the human 
                          wins the tie-breaker, false otherwise.
   
   Algorithm:
               1) Gets a die roll for the human player using `get_die_roll/1`.
               2) Asks the user if they want to roll for the computer using 
                  `human_wants_to_roll_for_computer/1`. If they do, gets a 
                  manual die roll; otherwise, generates an automatic roll.
               3) Displays the die rolls for both players.
               4) If the human's roll is higher, the human wins (HumanWon = true).
               5) If the computer's roll is higher, the computer wins 
                  (HumanWon = false).
               6) If it's a tie, displays a message and recursively calls 
                  `human_won_tie_breaker/1` to roll again.
   Reference: None
********************************************************************* */
human_won_tie_breaker(HumanWon) :-
    get_die_roll(HumanRoll),
    ( human_wants_to_roll_for_computer(true) ->
        get_manual_die_roll(ComputerRoll)
    ; get_auto_die_roll(ComputerRoll)
    ),
    format("You rolled a ~w~n", [HumanRoll]),
    format("The computer rolled a ~w~n", [ComputerRoll]),
    ( HumanRoll > ComputerRoll ->
        writeln("You go first!"), HumanWon = true
    ; ComputerRoll > HumanRoll ->
        writeln("The computer goes first!"), HumanWon = false
    ; writeln('It is a tie! Rolling again.'),
      human_won_tie_breaker(HumanWon)
    ).

/* *********************************************************************
   Predicate Name: get_dice_to_keep/4
   Purpose: Prompts the user (human player) to select which dice to keep 
            from a list of rolled dice.
   Parameters:
               Player - The player (used to get the player's name).
               ScoreCard - The current scorecard (not used in this predicate).
               DiceRolls - A list of the current dice rolls.
               DiceToKeep - Output parameter unified with a list of the dice 
                            the user chooses to keep.
   
   Algorithm:
               1) Gets the player's name using `player:get_name/2`.
               2) Displays the player's current dice rolls.
               3) Prompts the user to enter the dice they want to keep.
               4) Reads the user input.
               5) If the input is empty, sets DiceToKeep to an empty list.
               6) Otherwise, splits the input string into a list of numbers 
                  and validates the selection using `validate_dice_to_keep/2`.
   Reference: None
********************************************************************* */
get_dice_to_keep(Player, ScoreCard, DiceRolls, DiceToKeep) :-
    player:get_name(Player, PlayerName),
    format("~ws current dice rolls: ~w~n", [PlayerName, DiceRolls]),
    writeln('Enter the dice you would like to keep separated by spaces:'),
    read_line_to_string(user_input, Input),
    % Debug statement to check the user input
    format("User input: ~w~n", [Input]),
    % If the input is empty, don't keep any dice
    (Input = "" -> 
        DiceToKeep = [],
        % Debug statement to check the empty input case
        writeln("No dice to keep entered, DiceToKeep is set to an empty list.")
    ;
        split_string(Input, " ", "", DiceStrings),
        maplist(number_string, DiceToKeep, DiceStrings),
        % Debug statement to check the dice to keep
        format("Dice to keep entered: ~w~n", [DiceToKeep]),
        validate_dice_to_keep(DiceRolls, DiceToKeep)
    ).

/* *********************************************************************
   Predicate Name: validate_dice_to_keep/2
   Purpose: Validates that the dice the user wants to keep are a subset of 
            the dice they rolled.
   Parameters:
               DiceRolls - A list of the current dice rolls.
               DiceToKeep - A list of the dice the user wants to keep.
   
   Algorithm:
               1) Uses `subset/2` to check if DiceToKeep is a subset of DiceRolls.
               2) If not, displays an error message and fails.
   Reference: None
********************************************************************* */
validate_dice_to_keep(DiceRolls, DiceToKeep) :-
    (   subset(DiceToKeep, DiceRolls)
    ->  true
    ;   writeln('Invalid dice selection. Please enter valid dice values from the current roll.'),
        fail
    ).

/* *********************************************************************
   Predicate Name: to_string_vector/2
   Purpose: Converts a list (vector) of anything into a comma-separated 
            string representation enclosed in square brackets.
   Parameters:
               Vector - The input list.
               String - Output parameter unified with the string representation 
                        of the list.
   
   Algorithm:
               1) Uses `atomic_list_concat/3` to concatenate the elements of 
                  the list with commas and spaces.
               2) Formats the result into a string enclosed in square brackets.
   Reference: None
********************************************************************* */
to_string_vector(Vector, String) :-
    atomic_list_concat(Vector, ', ', InnerString),
    format(atom(String), "[~w]", [InnerString]).

/* *********************************************************************
   Predicate Name: show_categories/1
   Purpose: Displays the available scoring categories to the user.
   Parameters:
               Categories - A list of category IDs.
   
   Algorithm:
               1) For each category in the list:
                  a) Gets the category name using `scorecategory:category_name/2`.
                  b) Prints the category name to the console.
   Reference: None
********************************************************************* */
show_categories(Categories) :-
    
    forall(member(Category, Categories),
           ( scorecategory:category_name(Category, CategoryName),
             writeln(CategoryName)
           )),
    nl.
/* *********************************************************************
   Predicate Name: user_wants_to_load_game/1
   Purpose: Asks the user if they want to load a saved game.
   Parameters:
               WantsToLoad - Output parameter unified with true if the user 
                              wants to load a game, false otherwise.
   
   Algorithm:
               1) Uses `get_yes_no/2` to get the user's response.
   Reference: None
********************************************************************* */
user_wants_to_load_game(WantsToLoad) :-
    get_yes_no("Would you like to load a saved game?", WantsToLoad).

/* *********************************************************************
   Predicate Name: get_serial/1
   Purpose: Reads serialized game data from a file.
   Parameters:
               GameState - Output parameter unified with the deserialized 
                           game state.
   
   Algorithm:
               1) Prompts the user to enter the file name.
               2) Reads the file name from the user.
               3) Calls `read_game_state/2` (not defined in this code) to 
                  read and deserialize the game state from the file.
   Reference: None
********************************************************************* */
get_serial(GameState) :-
    format('Enter the file name to load the game: ~n', []),
    read_line_to_string(user_input, FileName),
    read_game_state(FileName, GameState).
 
   

/* *********************************************************************
   Predicate Name: wants_to_stand/1
   Purpose: Asks the user if they want to stand (stop rolling).
   Parameters:
               WantsToStand - Output parameter unified with true if the user 
                              wants to stand, false otherwise.
   
   Algorithm:
               1) Uses `get_yes_no/2` to get the user's response.
   Reference: None
********************************************************************* */
wants_to_stand(WantsToStand) :-
    get_yes_no("Would you like to stand?", WantsToStand).

/* *********************************************************************
   Predicate Name: show_category_pursuits/1
   Purpose: Displays a list of possible scoring opportunities (category pursuits) 
            to the user.
   Parameters:
               CategoryPursuits - A list of category pursuits, where each pursuit 
                                  is a term with information about the category, 
                                  minimum score, maximum score, etc.
   
   Algorithm:
               1) Extracts the reasoning behind each pursuit.
               2) Sorts the reasons.
               3) Displays the current dice.
               4) For each reason, displays a formatted message to the user 
                  indicating the category, potential scores, and required rolls.
   Reference: None
********************************************************************* */
show_category_pursuits(CategoryPursuits) :-
    findall(Reason, member(_-Reason, CategoryPursuits), Reasons),
    sort(2, @=<, Reasons, SortedReasons),
    nth0(0, SortedReasons, FirstReason),
    format("Current dice: ~w~n", [FirstReason.current_dice]),
    forall(member(Reason, SortedReasons),
           ( Reason.min_score =:= 0 ->
               format("Can get ~w with a score of ~w by rolling ~w~n",
                      [Reason.pursued_category, Reason.max_score, Reason.roll_to_get_max])
           ; format("Can get ~w with a minimum score of ~w by getting ~w and a maximum score of ~w by rolling ~w~n",
                    [Reason.pursued_category, Reason.min_score, Reason.roll_to_get_min, Reason.max_score, Reason.roll_to_get_max])
           )).

/* *********************************************************************
   Predicate Name: wants_help/1
   Purpose: Asks the user if they want help.
   Parameters:
               WantsHelp - Output parameter unified with true if the user 
                           wants help, false otherwise.
   Return Value: None (The result is unified with the WantsHelp variable)
   Algorithm:
               1) Displays the terms of the game.
               2) Uses `get_yes_no/2` to get the user's response.
   Reference: None
********************************************************************* */
wants_help(WantsHelp) :-
    writeln('Terms'),
    
    get_yes_no("Would you like help?", WantsHelp).

/* *********************************************************************
   Predicate Name: save_game_procedure/1
   Purpose: Prompts the user to save the current game state and handles 
            the saving process.
   Parameters:
               GameState - The current game state.
   Return Value: None
   Algorithm:
               1) Serializes the game state using `game:serialize_game_state/2`.
               2) Asks the user if they want to save the game using `get_yes_no/2`.
               3) If the user wants to save:
                  a) Prompts the user to enter a file name.
                  b) Opens the file for writing.
                  c) Writes the serialized game state to the file.
                  d) Closes the file.
                  e) Displays a success message.
                  f) Halts the program.
   Reference: None
********************************************************************* */
save_game_procedure(GameState) :-
    game:serialize_game_state(GameState, SerializedGameState),
    get_yes_no('Would you like to save the game and exit?', WantsToSave),
    ( WantsToSave ->
        writeln('Enter the name of the file you would like to save:'),
        read_line_to_string(user_input, FileName),
        open(FileName, write, Stream),
        write(Stream, SerializedGameState),
        close(Stream),
        writeln('Game saved successfully.'),
        halt
    ; true
    ).