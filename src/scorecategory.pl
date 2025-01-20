/*
% This module defines and manages the score categories for the Yahtzee game, including predicates for determining applicable categories, calculating scores, and checking category validity.
*/
% filepath: /d:/OPL/Prolog/scorecategory.pl
:- module(scorecategory, [
    category/1,
    get_applicable_categories/2,
    get_score/3,
    is_applicable_category/2,
    is_possible_category/2,
    category_name/2,
    categories/1
]).

:- use_module(helpers).

/* *********************************************************************
Predicate Name: category
Purpose: Defines a valid category for the Yahtzee game.
Parameters:
            Category, a term representing a score category.

Algorithm:
            1) Define each valid category as a fact.
Reference: none
********************************************************************* */
category(ones).
category(twos).
category(threes).
category(fours).
category(fives).
category(sixes).
category(three_of_a_kind).
category(four_of_a_kind).
category(full_house).
category(four_straight).
category(five_straight).
category(yahtzee).

/* *********************************************************************
Predicate Name: category_name
Purpose: Maps a category to its corresponding string name.
Parameters:
            Category, a term representing a score category.
            CategoryName, a string representing the name of the category.

Algorithm:
            1) Define the mapping between each category and its string name.
Reference: none
********************************************************************* */
category_name(yahtzee, 'Yahtzee').
category_name(five_straight, 'Five Straight').
category_name(four_straight, 'Four Straight').
category_name(full_house, 'Full House').
category_name(four_of_a_kind, 'Four of a Kind').
category_name(three_of_a_kind, 'Three of a Kind').
category_name(sixes, 'Sixes').
category_name(fives, 'Fives').
category_name(fours, 'Fours').
category_name(threes, 'Threes').
category_name(twos, 'Twos').
category_name(ones, 'Ones').

/* *********************************************************************
Predicate Name: categories
Purpose: Provides a list of all valid categories.
Parameters:
            CategoriesList, a list of all valid score categories.

Algorithm:
            1) Define the list of all valid categories.
Reference: none
********************************************************************* */
categories([ones, twos, threes, fours, fives, sixes, three_of_a_kind, four_of_a_kind, full_house, four_straight, five_straight, yahtzee]).

/* *********************************************************************
Predicate Name: get_applicable_categories
Purpose: Finds all applicable categories based on the given dice values.
Parameters:
            Dice, a list of dice values.
            ApplicableCategories, a list of categories that are applicable based on the dice values.

Algorithm:
            1) Use findall to collect all categories for which is_applicable_category succeeds.
Reference: none
********************************************************************* */
get_applicable_categories(Dice, ApplicableCategories) :-
    findall(Category, is_applicable_category(Dice, Category), ApplicableCategories).

/* *********************************************************************
Predicate Name: get_score
Purpose: Calculates the score for a specific category based on the given dice values.
Parameters:
            Dice, a list of dice values.
            Category, the category for which the score is to be calculated.
            Score, the calculated score for the given category.

Algorithm:
            1) Check if the category is applicable using is_applicable_category.
            2) If applicable, calculate the score using calculate_score.
            3) If not applicable, set the score to 0.
Reference: none
********************************************************************* */
get_score(Dice, Category, Score) :-
(   is_applicable_category(Dice, Category)
->  calculate_score(Dice, Category, Score)
;   Score = 0
).

/* *********************************************************************
Predicate Name: calculate_score
Purpose: Helper predicate to calculate the score for a specific category.
Parameters:
            Dice, a list of dice values.
            Category, the category for which the score is to be calculated.
            Score, the calculated score for the given category.

Algorithm:
            1) Use different rules to calculate the score based on the category.
Reference: none
********************************************************************* */
calculate_score(Dice, ones, Score) :-
    helpers:count_n(Dice, 1, Score).
calculate_score(Dice, twos, Score) :-
    helpers:count_n(Dice, 2, Count), Score is Count * 2.
calculate_score(Dice, threes, Score) :-
    helpers:count_n(Dice, 3, Count), Score is Count * 3.
calculate_score(Dice, fours, Score) :-
    helpers:count_n(Dice, 4, Count), Score is Count * 4.
calculate_score(Dice, fives, Score) :-
    helpers:count_n(Dice, 5, Count), Score is Count * 5.
calculate_score(Dice, sixes, Score) :-
    helpers:count_n(Dice, 6, Count), Score is Count * 6.
calculate_score(Dice, three_of_a_kind, Score) :-
    (   is_applicable_category(Dice, three_of_a_kind)
    ->  helpers:sum_list_custom(Dice, Score)
    ;   Score = 0
    ).
calculate_score(Dice, four_of_a_kind, Score) :-
    (   is_applicable_category(Dice, four_of_a_kind)
    ->  helpers:sum_list_custom(Dice, Score)
    ;   Score = 0
    ).
calculate_score(Dice, full_house, Score) :-
    (   is_applicable_category(Dice, full_house)
    ->  Score = 25
    ;   Score = 0
    ).
calculate_score(Dice, four_straight, Score) :-
    (   is_applicable_category(Dice, four_straight)
    ->  Score = 30
    ;   Score = 0
    ).
calculate_score(Dice, five_straight, Score) :-
    (   is_applicable_category(Dice, five_straight)
    ->  Score = 40
    ;   Score = 0
    ).
calculate_score(Dice, yahtzee, Score) :-
    (   is_applicable_category(Dice, yahtzee)
    ->  Score = 50
    ;   Score = 0
    ).

/* *********************************************************************
Predicate Name: is_applicable_category
Purpose: Checks if the given category is applicable based on the dice values.
Parameters:
            Dice, a list of dice values.
            Category, the category to check for applicability.

Algorithm:
            1) Use helper predicates to check if the category is applicable based on the dice values.
Reference: none
********************************************************************* */
is_applicable_category(Dice, yahtzee) :-
    helpers:atleast_n_same(Dice, 5).
is_applicable_category(Dice, five_straight) :-
    helpers:five_sequence(Dice).
is_applicable_category(Dice, four_straight) :-
    helpers:four_sequence(Dice).
is_applicable_category(Dice, full_house) :-
    helpers:full_house(Dice).
is_applicable_category(Dice, four_of_a_kind) :-
    helpers:atleast_four_same(Dice).
is_applicable_category(Dice, three_of_a_kind) :-
    helpers:atleast_three_same(Dice).
is_applicable_category(Dice, sixes) :-
    helpers:contains_n(Dice, 6).
is_applicable_category(Dice, fives) :-
    helpers:contains_n(Dice, 5).
is_applicable_category(Dice, fours) :-
    helpers:contains_n(Dice, 4).
is_applicable_category(Dice, threes) :-
    helpers:contains_n(Dice, 3).
is_applicable_category(Dice, twos) :-
    helpers:contains_n(Dice, 2).
is_applicable_category(Dice, ones) :-
    helpers:contains_n(Dice, 1).

/* *********************************************************************
Predicate Name: is_possible_category
Purpose: Checks if the given category is possible with the dice values and remaining slots.
Parameters:
            Dice, a list of dice values.
            Category, the category to check for possibility.

Algorithm:
            1) If Dice is empty, return true.
            2) Otherwise, calculate the number of slots left and use possible_category to check if the category is possible.
Reference: none
********************************************************************* */
is_possible_category(Dice, Category) :-
(   Dice = []
->  true
;   length(Dice, Length),
    SlotsLeft is 5 - Length,
    possible_category(Dice, SlotsLeft, Category)
).

/* *********************************************************************
Predicate Name: possible_category
Purpose: Helper predicate to determine if a category is possible based on dice and slots left.
Parameters:
            Dice, a list of dice values.
            SlotsLeft, the number of slots left to fill.
            Category, the category to check for possibility.

Algorithm:
            1) Use helper predicates to check if the category is possible based on the dice values and slots left.
Reference: none
********************************************************************* */
possible_category(Dice, _, yahtzee) :-
    helpers:all_same(Dice).
possible_category(Dice, _, five_straight) :-
    helpers:num_repeats(Dice, Repeats),
    Repeats < 1,
    \+ (helpers:contains_n(Dice, 1), helpers:contains_n(Dice, 6)).
possible_category(Dice, _, four_straight) :-
    helpers:num_repeats(Dice, Repeats),
    Repeats < 2.
possible_category(Dice, _, full_house) :-
    helpers:count_unique(Dice, UniqueCount),
    UniqueCount =< 2,
    helpers:max_count(Dice, MaxCount),
    MaxCount =< 3.
possible_category(Dice, SlotsLeft, four_of_a_kind) :-
    helpers:max_count(Dice, MaxCount),
    SlotsLeft + MaxCount >= 4.
possible_category(Dice, SlotsLeft, three_of_a_kind) :-
    helpers:max_count(Dice, MaxCount),
    SlotsLeft + MaxCount >= 3.
possible_category(Dice, _, sixes) :-
    helpers:contains_n(Dice, 6);
    length(Dice, Length),
    Length < 5.
possible_category(Dice, _, fives) :-
    helpers:contains_n(Dice, 5);
    length(Dice, Length),
    Length < 5.
possible_category(Dice, _, fours) :-
    helpers:contains_n(Dice, 4);
    length(Dice, Length),
    Length < 5.
possible_category(Dice, _, threes) :-
    helpers:contains_n(Dice, 3);
    length(Dice, Length),
    Length < 5.
possible_category(Dice, _, twos) :-
    helpers:contains_n(Dice, 2);
    length(Dice, Length),
    Length < 5.
possible_category(Dice, _, ones) :-
    helpers:contains_n(Dice, 1);
    length(Dice, Length),
    Length < 5.