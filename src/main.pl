/************************************************************
 * Name:  Samman Bhetwal                                    *
 * Project:  Yahtzee Prolog                                 *
 * Class:  CMPS-366                                         *
 * Date:  12/11/2024                                        *
 ************************************************************/
% filepath /d/OPL/Prolog/main.pl


:- module(yahtzee, [play_yahtzee/0]).




:- use_module(iofunctions, [
  user_wants_to_load_game/1,
  get_serial/1,
  save_game_procedure/1
]).

:- use_module(scorecard, [initialize_scorecard/1]).
:- use_module(game, [
  initialize_game/3,
  is_over/1,
  show_scores/1,
  is_draw/1,
  get_winner/2,
  play_round/2
]).


/* *********************************************************************
   Predicate Name: play_yahtzee/0
   Purpose: Starts the game of Yahtzee. This predicate initializes and 
            begins the Yahtzee game. It sets up the game environment 
            and handles the game loop.
   Parameters: None
   
   Algorithm:
               1) Displays a welcome message.
               2) Asks the user if they want to load a saved game using 
                  `user_wants_to_load_game/1`.
               3) If they want to load:
                  a) Loads the game state from a file using `get_serial/1`.
                  b) Starts the `game_loop/1` with the loaded state.
               4) If they want a new game:
                  a) Initializes a new scorecard using `initialize_scorecard/1`.
                  b) Initializes a new game state using `initialize_game/3`.
                  c) Starts the `game_loop/1` with the new state.
   Reference: None
********************************************************************* */
play_yahtzee :-
    writeln('Welcome to the Yahtzee Game:'),
    nl,
    iofunctions:user_wants_to_load_game(WantsToLoad),
    ( WantsToLoad ->
        writeln('Load game started'),
        iofunctions:get_serial(GameState),
        
        game_loop(GameState)
    ; writeln('Starting new game'),
      scorecard:initialize_scorecard(ScoreCard),
      game:initialize_game(ScoreCard, 1, GameState),
      game_loop(GameState)
    ).

/* *********************************************************************
   Predicate Name: game_loop/1
   Purpose: This is the main game loop. It continues playing rounds until 
            the game is over, then determines the winner or if it's a draw.
   Parameters:
               GameState - The current state of the game.
   
   Algorithm:
               1) Checks if the game is over using `is_over/1`.
               2) If the game is over:
                  a) Shows the final scores using `show_scores/1`.
                  b) Checks for a draw using `is_draw/1`.
                  c) If it's a draw, displays a draw message.
                  d) Otherwise, gets the winner using `get_winner/2` and 
                     displays the winner.
               3) If the game is not over:
                  a) Plays a round using `play_round/2` to get the new game state.
                  b) Calls the save game procedure using `save_game_procedure/1`.
                  c) Recursively calls `game_loop/1` with the new game state.
   Reference: None
********************************************************************* */
game_loop(GameState) :-
    ( game:is_over(GameState) ->
        game:show_scores(GameState),
        ( game:is_draw(GameState) ->
            writeln('It is a draw!')
        ; game:get_winner(GameState, Winner),
          format("The winner is ~w!~n", [Winner])
        )
    ; 
      
      game:play_round(GameState, NewGameState),
      
      iofunctions:save_game_procedure(NewGameState),
      
     
      
      game_loop(NewGameState)
    ).

:- initialization(play_yahtzee).