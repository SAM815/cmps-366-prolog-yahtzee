/*
    % This module handles the main game logic for the Yahtzee game, including initialization,
     game state management, and determining the winner.
*/

% filepath: /d:/OPL/Prolog/game.pl


:- module(game, [
    initialize_game/3,
    is_over/1,
    is_draw/1,
    play_round/2,
    get_player_scores/2,
    show_scores/1,
    get_winner/2,
    serialize_game_state/2,
    read_game_state/2
]).

:- use_module(scorecard).
:- use_module(player).
:- use_module(round).
:- use_module(scorecategory).

:- discontiguous game:is_draw/1.
:- discontiguous game:get_winner/2.


% /* *********************************************************************
% Function Name: initialize_game/3
% Purpose: Initializes the game state with the given scorecard and current round.
% Parameters:
%     ScoreCard: A Prolog term representing the scorecard (input).
%     CurrentRound: An integer representing the current round number (input).
%     GameState: A game state term containing ScoreCard, CurrentRound, and Players (output).
% 
% Algorithm:
%     1. Call player:initialize_players/1 to initialize the player list.
%     2. Print a message indicating game initialization with the provided ScoreCard.
%     3. Unify GameState with a term representing the initial game state.
% Reference: player module for player initialization logic
% ********************************************************************* */
initialize_game(ScoreCard, CurrentRound, game_state(ScoreCard, CurrentRound, Players)) :-
    player:initialize_players(Players),
    writeln('Initializing game with ScoreCard in game.pl initialize_game:'),
    writeln(ScoreCard).

% /* *********************************************************************
% Function Name: is_over/1
% Purpose: Checks if the game is over based on the scorecard being full.
% Parameters:
%     GameState: A game state term containing ScoreCard, CurrentRound, and Players (input).
%
% Algorithm:
%     1. Extract the ScoreCard from the GameState.
%     2. Call scorecard:is_full/1 to determine if the scorecard is full.
% ********************************************************************* */
is_over(game_state(ScoreCard, _, _)) :-
    scorecard:is_full(ScoreCard).

% /* *********************************************************************
% Function Name: is_draw/1 (Discontiguous declaration)
% Purpose: Checks if the game is a draw (implementation details not provided).
% Parameters:
%     GameState: A game state term containing ScoreCard, CurrentRound, and Players (input).
%
% Algorithm:
%     1. (Implementation details for checking draw conditions are missing)
% ********************************************************************* */
is_draw(game_state(ScoreCard, _, _)) :-
    scorecard:is_draw(ScoreCard).

% /* *********************************************************************
% Function Name: play_round/2
% Purpose: Plays a round of Yahtzee and updates the game state.
% Parameters:
%     GameState: The current game state before the round is played (input).
%     NewGameState: The updated game state after the round is played (output).
% 
% Algorithm:
%     1. Check if the game is already over using is_over/1.
%         - If yes, print a message and keep the scorecard and round unchanged.
%     2. Print the current round number.
%     3. Call show_scores/1 to display the scores before the round.
%     4. Call round:play_round/4 to play the round and update the scorecard.
%         - Extract UpdatedScoreCard from the returned term.
%         - Print the updated scorecard.
%     5. Calculate the next round number.
%     6. Unify NewGameState with the updated game state information.
%     7. Call show_scores/1 to display the scores after the round.
% Reference: round module for round playing logic
%********************************************************************* */
play_round(game_state(ScoreCard, CurrentRound, Players), NewGameState) :-
    ( is_over(game_state(ScoreCard, CurrentRound, Players)) ->
        writeln('The game is over!'),
        NewScoreCard = ScoreCard,
        NextRound = CurrentRound
    ; format('Round ~w~n', [CurrentRound]),
      show_scores(game_state(ScoreCard, CurrentRound, Players)),
      round:play_round(CurrentRound, ScoreCard, Players, UpdatedScoreCard),
      
      scorecard:get_scorecard_string(UpdatedScoreCard, ScoreCardString),
      writeln("This is the scorecard in game.pl"),
      writeln(ScoreCardString),

      NextRound is CurrentRound + 1,
      NewGameState = game_state(UpdatedScoreCard, NextRound, Players),
      
      
      show_scores(NewGameState)
    ).

/* *********************************************************************
Predicate Name: get_player_scores
Purpose: To extract and return the scores of all players from the provided GameState.
Parameters:
            GameState, a structured representation of the current state of the game.
            PlayerScores, a list where each element corresponds to the score of a player.

Algorithm:
            1) Extract the ScoreCard and Players from the GameState.
            2) Use the scorecard:get_player_scores predicate to get the PlayerScores.
Reference: none
********************************************************************* */
get_player_scores(game_state(ScoreCard, _, Players), PlayerScores) :-
    scorecard:get_player_scores(ScoreCard, Players, PlayerScores).

/* *********************************************************************
Predicate Name: show_scores
Purpose: To display the scores of each player.
Parameters:
            GameState, the current state of the game.

Algorithm:
            1) Extract the PlayerScores from the GameState using get_player_scores.
            2) Print "Scores:".
            3) For each player and their score in PlayerScores, format and print the player's name and score.
            4) Print a newline.
Reference: none
********************************************************************* */
show_scores(GameState) :-
    get_player_scores(GameState, PlayerScores),
    writeln("Scores:"),
    forall(member(Player-Score, PlayerScores),
           format("~w: ~w~n", [Player, Score])),
    nl.

/* *********************************************************************
Predicate Name: get_winner
Purpose: To determine the winner of the game based on the given game state.
Parameters:
            GameState, the current state of the game.
            Winner, the player who has won.

Algorithm:
            1) Extract the ScoreCard from the GameState.
            2) Use the scorecard:get_winner predicate to determine the Winner.
Reference: none
********************************************************************* */
get_winner(game_state(ScoreCard, _, _), Winner) :-
    scorecard:get_winner(ScoreCard, Winner).

/* *********************************************************************
Predicate Name: read_game_state
Purpose: To read the game state from the specified file.
Parameters:
            GameStateFile, the path to the file containing the game state.
            GameState, the variable that will hold the game state after reading from the file.

Algorithm:
            1) Define the Players as [human, computer].
            2) Open the GameStateFile for reading.
            3) Read the CurrentRound and SerializedScoreCard from the file.
            4) Initialize the ScoreCard from the SerializedScoreCard.
            5) Construct the GameState from the ScoreCard, CurrentRound, and Players.
Reference: none
********************************************************************* */
read_game_state(GameStateFile, GameState) :-
    Players = [human, computer],
    
    open(GameStateFile, read, Stream),
    
    read(Stream, [CurrentRound, SerializedScoreCard]),
    
    initialize_scorecard_from_list(SerializedScoreCard, ScoreCard),
    
    GameState = game_state(ScoreCard, CurrentRound, Players).

/* *********************************************************************
Predicate Name: serialize_game_state
Purpose: To convert the current game state into a format that can be easily stored or transmitted.
Parameters:
            GameState, a term that includes the scorecard and the current round of the game.
            SerializedGameState, the resulting serialized format of the game state.

Algorithm:
            1) Extract the Entries from the ScoreCard.
            2) Serialize the Entries using serialize_scorecard_entries.
            3) Format the Round and SerializedEntries into a string and unify it with SerializedGameState.
Reference: none
********************************************************************* */
serialize_game_state(game_state(ScoreCard, Round, _), SerializedGameState) :-
    ScoreCard = scorecard(Entries),
    serialize_scorecard_entries(Entries, SerializedEntries),
    format(string(SerializedGameState), '[~w,\n\n[~w]\n].', [Round, SerializedEntries]).
