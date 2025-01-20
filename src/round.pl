/*
 This module handles the logic for playing a round of Yahtzee, including updating the scorecard with the results.
*/
% filepath: /d:/OPL/Prolog/round.pl



:- module(round, [
    play_round/4
]).

:- use_module(scorecard).
:- use_module(player).
:- use_module(iofunctions).
:- use_module(turn).




/* *********************************************************************
Predicate Name: play_round
Purpose: Plays a round of Yahtzee, updating the scorecard with the results.
Parameters:
            CurrentRound, the current round number.
            ScoreCard, the current scorecard of the game.
            Players, the list of players.
            NewScoreCard, the updated scorecard after the round.

Algorithm:
            1) Get the player scores and create a queue of players which determines the order of players.
            2) Copy the current scorecard.
            3) Check if the round is over (when all players have played or the scorecard is full).
            4) Play the round loop.
Reference: none
********************************************************************* */
play_round(CurrentRound, ScoreCard, Players, NewScoreCard) :-
    % Get the player scores and create a queue of players which determines the order of players
    scorecard:get_player_scores(ScoreCard, Players, PlayerScores),
    get_player_queue(PlayerScores, PlayerQueue),

    % Copy the current scorecard
    CurrentScoreCard = ScoreCard,

    % Check if the round is over (when all players have played or the scorecard is full)
    ( queue_empty(PlayerQueue) ; scorecard:is_full(ScoreCard) ->
        RoundOver = true
    ; RoundOver = false
    ),
    play_round_loop(CurrentRound, CurrentScoreCard, PlayerQueue, RoundOver, NewScoreCard).

/* *********************************************************************
Predicate Name: play_round_loop
Purpose: Helper predicate to play the round loop.
Parameters:
            CurrentRound, the current round number.
            CurrentScoreCard, the current scorecard of the game.
            PlayerQueue, the queue of players.
            RoundOver, a flag indicating if the round is over.
            NewScoreCard, the updated scorecard after the round.

Algorithm:
            1) If the round is over, return the current scorecard.
            2) Otherwise, get the next player from the queue and remove them from the front.
            3) Print the current state of the scorecard.
            4) Announce the player's turn.
Reference: none
********************************************************************* */
play_round_loop(_, CurrentScoreCard, _, true, CurrentScoreCard).
play_round_loop(CurrentRound, CurrentScoreCard, PlayerQueue, false, NewScoreCard) :-
    % Get the next player from the queue and remove them from the front
    dequeue(PlayerQueue, Player, RestQueue),

    % Print the current state of the scorecard
    writeln("This is the scorecard in round.pl"),
    scorecard:get_scorecard_string(CurrentScoreCard, ScoreCardString),
    writeln(ScoreCardString),

    % Announce the player's turn
    player:get_name(Player, PlayerName),
    format("It's ~w's turn.~n", [PlayerName]),

    % Simulate the player's turn by rolling the dice
    turn:play_turn(Player, CurrentScoreCard, Dice),
    writeln('this is the dice in round.pl: '),
    writeln(Dice),

    % Determine the highest scoring category based on the rolled dice
    scorecard:get_max_scoring_category(CurrentScoreCard, Dice, ScoredCategory),
    writeln('this is the scored category in round.pl: '),
    writeln(ScoredCategory),
    

    % If a valid scoring category is found, display the score
    ( ScoredCategory \= null ->
        scorecategory:get_score(Dice, ScoredCategory, Score),
        format("~w scored ~w points in the ~w category.~n~n",
               [PlayerName, Score, ScoredCategory])
    ; true
    ),

    % Update the scorecard with the new scores for this player and round
    ( ScoredCategory \= null ->
        format("Updating scorecard with category: ~w, score: ~w, player: ~w, round: ~w~n", [ScoredCategory, Score, Player, CurrentRound]), % Debug statement
        scorecard:add_entry(CurrentScoreCard, ScoredCategory, Score, Player, CurrentRound, UpdatedScoreCard)
    ; UpdatedScoreCard = CurrentScoreCard
    ),


    % Check again if the round is over (queue is empty or scorecard is full)
    ( queue_empty(RestQueue) ; scorecard:is_full(UpdatedScoreCard) ->
        RoundOver = true
    ; RoundOver = false
    ),
    play_round_loop(CurrentRound, UpdatedScoreCard, RestQueue, RoundOver, NewScoreCard).

/* *********************************************************************
Predicate Name: get_player_queue
Purpose: Gets the player queue based on the player scores.
Parameters:
            PlayerScores, a list of player-score pairs.
            PlayerQueue, the resulting queue of players.

Algorithm:
            1) Extract the players and their scores for comparison.
            2) If both players have the same score, conduct a tie-breaker to determine the turn order.
            3) If the scores differ, enqueue players based on their scores (lowest score goes first).
Reference: none
********************************************************************* */
get_player_queue(PlayerScores, PlayerQueue) :-
    % Extract the players and their scores (for comparison)
    maplist(get_player_score, PlayerScores, Players, Scores),
    ( Scores = [Score1, Score2], Score1 =:= Score2 ->
        % If both players have the same score, conduct a tie-breaker to determine the turn order
        ( Score1 =:= 0 ->
            writeln("Determining who goes first by rolling a die.")
        ; format("Both players have a score of ~w. Conducting a tie breaker.~n", [Score1])
        ),
        queue_from_tie_breaker(Players, PlayerQueue)
    ; % If the scores differ, enqueue players based on their scores (lowest score goes first)
        enqueue_players_by_score(PlayerScores, PlayerQueue)
    ).

/* *********************************************************************
Predicate Name: get_player_score
Purpose: Extracts the Player and Score from a given Player-Score pair.
Parameters:
            PlayerScorePair, a pair of player and score.
            Player, the extracted player.
            Score, the extracted score.

Algorithm:
            1) Unify the Player and Score with the elements of the Player-Score pair.
Reference: none
********************************************************************* */
get_player_score(Player-Score, Player, Score).

/* *********************************************************************
Predicate Name: queue_from_tie_breaker
Purpose: Gets the player queue based on the tie-breaker result.
Parameters:
            Players, a list of players.
            PlayerQueue, the resulting queue of players.

Algorithm:
            1) Identify human and computer players from the list.
            2) Simulate a tie-breaker to decide who plays first (e.g., rolling a die).
            3) Add the winner of the tie-breaker first in the queue.
Reference: none
********************************************************************* */
queue_from_tie_breaker([Player1, Player2], PlayerQueue) :-
    % Identify human and computer players from the list
    player:get_name(Player1, Name1),
    player:get_name(Player2, Name2),
    ( Name1 = "Human" -> HumanPlayer = Player1, ComputerPlayer = Player2
    ; HumanPlayer = Player2, ComputerPlayer = Player1
    ),

    % Simulate a tie-breaker to decide who plays first (e.g., rolling a die)
    ( iofunctions:human_won_tie_breaker(true) ->
        % If the human wins, they are added first in the queue
        PlayerQueue = [HumanPlayer, ComputerPlayer]
    ; % If the computer wins, they are added first
        PlayerQueue = [ComputerPlayer, HumanPlayer]
    ).

/* *********************************************************************
Predicate Name: enqueue_players_by_score
Purpose: Enqueues players based on their scores (lowest score goes first).
Parameters:
            PlayerScores, a list of player-score pairs.
            PlayerQueue, the resulting queue of players.

Algorithm:
            1) Sort the players by their scores in ascending order.
            2) Extract the players from the sorted list to form the queue.
Reference: none
********************************************************************* */
enqueue_players_by_score(PlayerScores, PlayerQueue) :-
    % Sort the players by their scores in ascending order
    keysort(PlayerScores, SortedPlayerScores),
    pairs_keys(SortedPlayerScores, PlayerQueue).

/* *********************************************************************
Predicate Name: queue_empty
Purpose: Checks if the queue is empty.
Parameters:
            Queue, the queue to check.

Algorithm:
            1) Check if the queue is an empty list.
Reference: none
********************************************************************* */
queue_empty([]).

/* *********************************************************************
Predicate Name: dequeue
Purpose: Removes the front element from the Queue and unifies it with Element, 
         and unifies the rest of the Queue with RestQueue.
Parameters:
            Queue, the queue to dequeue from.
            Element, the front element of the queue.
            RestQueue, the remaining elements of the queue.

Algorithm:
            1) Unify the front element with Element.
            2) Unify the rest of the queue with RestQueue.
Reference: none
********************************************************************* */
dequeue([Element | RestQueue], Element, RestQueue).