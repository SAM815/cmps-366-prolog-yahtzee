/*
% This module manages the scorecard for the Yahtzee game, including initialization, updating entries, and retrieving scores and categories.
*/

% filepath: /d:/OPL/Prolog/scorecard.pl
:- module(scorecard, [
    initialize_scorecard/1,
    add_entry/6,
    get_max_scoring_category/3,
    is_full/1,
    get_open_categories/2,
    get_possible_categories/3,
    get_player_score/3,
    get_player_scores/3,
    get_winner/2,
    is_draw/1,
    get_players/2,
    get_scorecard_string/2,
    serialize_scorecard_entries/2,
    initialize_scorecard_from_list/2,
    get_all_player_scores/4,
    is_category_filled/2
]).

:- use_module(library(lists)).
:- use_module(player).
:- use_module(scorecategory).
:- use_module(helpers).

/* *********************************************************************
Predicate Name: initialize_scorecard
Purpose: Initializes the scorecard with null values for all categories.
Parameters:
            ScoreCard, the initialized scorecard.

Algorithm:
            1) Use findall to collect all categories with null values.
Reference: none
********************************************************************* */
initialize_scorecard(scorecard(ScoreCard)) :-
    findall(Category-null, scorecategory:category(Category), ScoreCard).

/* *********************************************************************
Predicate Name: add_entry
Purpose: Adds an entry to the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            Category, the category to add the entry to.
            Points, the points scored in the category.
            Winner, the player who scored the points.
            Round, the round in which the points were scored.
            NewScoreCard, the updated scorecard.

Algorithm:
            1) Check if the category is null in the current scorecard.
            2) If null, replace it with the new entry.
            3) If not null, throw an invalid argument error.
Reference: none
********************************************************************* */
add_entry(scorecard(ScoreCard), Category, Points, Winner, Round, scorecard(NewScoreCard)) :-
    ( memberchk(Category-null, ScoreCard) ->
        select(Category-null, ScoreCard, Category-scorecard_entry(Points, Winner, Round), NewScoreCard)
    ; throw(invalid_argument("Category already has a score card entry"))
    ).

category_priority_order([yahtzee, five_straight, four_straight, full_house, four_of_a_kind, three_of_a_kind, sixes, fives, fours, threes, twos, ones]).

/* *********************************************************************
Predicate Name: get_max_scoring_category
Purpose: Determines the highest scoring category based on the rolled dice.
Parameters:
            ScoreCard, the current scorecard.
            Dice, the rolled dice values.
            MaxCategory, the category with the highest score.

Algorithm:
            1) Get the open categories from the scorecard.
            2) Calculate the score for each open category.
            3) Determine the category with the maximum score.
Reference: none
********************************************************************* */
get_max_scoring_category(scorecard(ScoreCard), Dice, MaxCategory) :-
    get_open_categories(scorecard(ScoreCard), OpenCategories),
    
    scorecategory:get_applicable_categories(Dice, ApplicableCategories),
    
    helpers:intersection_custom(OpenCategories, ApplicableCategories, AssignableCategories),
    
    ( AssignableCategories = [] ->
        MaxCategory = null
    ; findall(Score-Category, (member(Category, AssignableCategories), scorecategory:get_score(Dice, Category, Score)), CategoryScores),
      max_member(MaxScore-_, CategoryScores),
      findall(Category, member(MaxScore-Category, CategoryScores), MaxScoreCategories),
      category_priority_order(PriorityOrder),
      member(MaxCategory, PriorityOrder),
      member(MaxCategory, MaxScoreCategories)
    ).

/* *********************************************************************
Predicate Name: is_full
Purpose: Checks if the scorecard is full (all categories are filled).
Parameters:
            ScoreCard, the current scorecard.

Algorithm:
            1) Check if there are any null entries in the scorecard.
Reference: none
********************************************************************* */
is_full(scorecard(ScoreCard)) :-
    \+ member(_-null, ScoreCard).

/* *********************************************************************
Predicate Name: get_open_categories
Purpose: Retrieves the list of open categories (categories not yet filled) from the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            OpenCategories, the list of open categories.

Algorithm:
            1) Use findall to collect all categories that are null.
Reference: none
********************************************************************* */
get_open_categories(scorecard(ScoreCard), OpenCategories) :-
    findall(Category, member(Category-null, ScoreCard), OpenCategories).

/* *********************************************************************
Predicate Name: get_possible_categories
Purpose: Retrieves the list of possible categories based on the current dice values.
Parameters:
            ScoreCard, the current scorecard.
            Dice, the rolled dice values.
            PossibleCategories, the list of possible categories.

Algorithm:
            1) Get the open categories from the scorecard.
            2) Filter the open categories to find those that are applicable based on the dice values.
Reference: none
********************************************************************* */
get_possible_categories(scorecard(ScoreCard), Dice, PossibleCategories) :-
    get_open_categories(scorecard(ScoreCard), OpenCategories),
    findall(Category, (member(Category, OpenCategories), scorecategory:is_possible_category(Dice, Category)), PossibleCategories).

/* *********************************************************************
Predicate Name: get_player_score
Purpose: Retrieves the score of a specific player from the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            Player, the player whose score is to be retrieved.
            Score, the score of the player.

Algorithm:
            1) Use findall to collect all scores for the player.
            2) Sum the scores to get the total score for the player.
Reference: none
********************************************************************* */
get_player_score(scorecard(ScoreCard), Player, Score) :-
    findall(Points, (member(_-scorecard_entry(Points, Player, _), ScoreCard)), PointsList),
    sum_list(PointsList, Score).

% Helper predicate to accumulate points for a player
get_player_points([], _, Acc, Acc).
get_player_points([_-scorecard_entry(Points, Player, _)|T], Player, Acc, Score) :-
    NewAcc is Acc + Points,
    get_player_points(T, Player, NewAcc, Score).
get_player_points([_|T], Player, Acc, Score) :-
    get_player_points(T, Player, Acc, Score).

/* *********************************************************************
Predicate Name: get_player_scores
Purpose: Retrieves the scores of all players from the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            Players, the list of players.
            PlayerScores, the list of scores for each player.

Algorithm:
            1) Use maplist to get the score for each player.
Reference: none
********************************************************************* */
get_player_scores(scorecard(ScoreCard), Players, PlayerScores) :-
    get_all_player_scores(ScoreCard, Players, [], PlayerScores).

/* *********************************************************************
Predicate Name: get_all_player_scores
Purpose: Retrieves the scores of all players for all categories.
Parameters:
            ScoreCard, the current scorecard.
            Players, the list of players.
            Categories, the list of categories.
            AllPlayerScores, the list of scores for each player and category.

Algorithm:
            1) Use nested maplist to get the score for each player and category.
Reference: none
********************************************************************* */
get_all_player_scores(_, [], Acc, Acc).
get_all_player_scores(ScoreCard, [Player|T], Acc, PlayerScores) :-
    get_player_score(scorecard(ScoreCard), Player, Score),
    get_all_player_scores(ScoreCard, T, [Player-Score|Acc], PlayerScores).

/* *********************************************************************
Predicate Name: get_winner
Purpose: Determines the winner based on the scores in the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            Winner, the player with the highest score.

Algorithm:
            1) Get the scores of all players.
            2) Determine the player with the highest score.
Reference: none
********************************************************************* */
get_winner(scorecard(ScoreCard), Winner) :-
    
    is_full(scorecard(ScoreCard)),

    get_players(scorecard(ScoreCard), Players),

    get_all_player_scores(ScoreCard, Players, [], PlayerScores),

    find_max_score_player(PlayerScores, Winner),
    writeln('Debug: Winner'),
    writeln(Winner).

% Helper predicate to find the player with the maximum score
find_max_score_player([Player-Score], Player) :- !.
find_max_score_player([Player1-Score1, Player2-Score2 | Rest], Winner) :-
    ( Score1 >= Score2 ->
        find_max_score_player([Player1-Score1 | Rest], Winner)
    ; find_max_score_player([Player2-Score2 | Rest], Winner)
    ).

/* *********************************************************************
Predicate Name: is_draw
Purpose: Checks if the game is a draw (all players have the same score).
Parameters:
            ScoreCard, the current scorecard.

Algorithm:
            1) Get the scores of all players.
            2) Check if all scores are equal.
Reference: none
********************************************************************* */
is_draw(scorecard(ScoreCard)) :-
    is_full(scorecard(ScoreCard)),
    get_players(scorecard(ScoreCard), Players),
    get_player_scores(scorecard(ScoreCard), Players, PlayerScores),
    max_member(MaxScore-_, PlayerScores),
    findall(Player, member(Player-MaxScore, PlayerScores), MaxScorePlayers),
    length(MaxScorePlayers, Count),
    Count > 1.

/* *********************************************************************
Predicate Name: get_players
Purpose: Retrieves the list of players from the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            Players, the list of players.

Algorithm:
            1) Use findall to collect all unique players from the scorecard.
Reference: none
********************************************************************* */
get_players(scorecard(ScoreCard), Players) :-
    findall(Winner, member(_-scorecard_entry(_, Winner, _), ScoreCard), PlayersList),
    list_to_set(PlayersList, Players).

/* *********************************************************************
Predicate Name: get_scorecard_string
Purpose: Converts the scorecard to a string representation.
Parameters:
            ScoreCard, the current scorecard.
            ScoreCardString, the string representation of the scorecard.

Algorithm:
            1) Use maplist to convert each entry to a string.
            2) Concatenate the strings to form the final scorecard string.
Reference: none
********************************************************************* */
get_scorecard_string(scorecard(ScoreCard), String) :-
    findall(Line, (member(Category-Entry, ScoreCard), format_entry(Category, Entry, Line)), Lines),
    atomic_list_concat(Lines, "\n", String).

% format_entry(+Category, +Entry, -Line)
% Formats a single scorecard entry into a string representation and unifies it with Line.
format_entry(Category, null, Line) :-
    scorecategory:category_name(Category, CategoryName),
    format(atom(Line), "~w~t~20|~t-~10+~t-~15+~t-~10+", [CategoryName]).
format_entry(Category, scorecard_entry(Points, Winner, Round), Line) :-
    scorecategory:category_name(Category, CategoryName),
    player:get_name(Winner, WinnerName),
    format(atom(Line), "~w~t~20|~t~w~10+~t~w~15+~t~w~10+", [CategoryName, Round, WinnerName, Points]).

categories_in_order([ones, twos, threes, fours, fives, sixes, three_of_a_kind, four_of_a_kind, full_house, four_straight, five_straight, yahtzee]).

/* *********************************************************************
Predicate Name: is_category_filled
Purpose: Checks if a specific category is filled in the scorecard.
Parameters:
            ScoreCard, the current scorecard.
            Category, the category to check.

Algorithm:
            1) Check if the category is not null in the scorecard.
Reference: none
********************************************************************* */
is_category_filled(scorecard(ScoreCard), Category) :-
    memberchk(Category-_, ScoreCard),
    \+ memberchk(Category-null, ScoreCard).


/* *********************************************************************
Predicate Name: initialize_scorecard_from_list
Purpose: Initializes the scorecard from a serialized list of entries.
Parameters:
            SerializedEntries, the serialized list of scorecard entries.
            ScoreCard, the initialized scorecard.

Algorithm:
            1) Use maplist to deserialize each entry.
Reference: none
********************************************************************* */
initialize_scorecard_from_list(ScoreCardList, NewScoreCard):-
    categories_in_order(Categories),
    
    initialize_scorecard(ScoreCard),
    
    add_entries(ScoreCardList, Categories, ScoreCard, NewScoreCard),
    

add_entries([], _, ScoreCard, ScoreCard).
add_entries(_, [], ScoreCard, ScoreCard).
add_entries([[Points, Player, Round]|T], [Category|Categories], ScoreCard, NewScoreCard):-
    
    
    add_entry(ScoreCard, Category, Points, Player, Round, NewScoreCard1),
    
    add_entries(T, Categories, NewScoreCard1, NewScoreCard).

add_entries([[0] | T], [Category|Categories], ScoreCard, NewScoreCard):-
    add_entries(T, Categories, ScoreCard, NewScoreCard).

/* *********************************************************************
Predicate Name: serialize_scorecard_entries
Purpose: Serializes the scorecard entries to a list format.
Parameters:
            ScoreCard, the current scorecard.
            SerializedEntries, the serialized list of scorecard entries.

Algorithm:
            1) Use maplist to serialize each entry.
Reference: none
********************************************************************* */
serialize_scorecard_entries([], '').
serialize_scorecard_entries([Entry-Value|Rest], SerializedEntries) :-
    ( Value = scorecard_entry(Points, Winner, Round) ->
        format(string(SerializedEntry), '[~w, ~w, ~w]', [Points, Winner, Round])
    ; Value = null ->
        SerializedEntry = '[0]'
    ),
    serialize_scorecard_entries(Rest, RestSerializedEntries),
    ( RestSerializedEntries = '' ->
        SerializedEntries = SerializedEntry
    ; format(string(SerializedEntries), '~w,\n       ~w', [SerializedEntry, RestSerializedEntries])
    ).