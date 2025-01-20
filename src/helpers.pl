/*
This module provides various helper predicates used throughout the Yahtzee game, including predicates for checking sequences, 
counting occurrences, and manipulating lists.
*/
% filepath: /d:/OPL/Prolog/helpers.pl


:- module(helpers, [
    all_same/1,
    all_sequence/1,
    contains_sequence/2,
    five_sequence/1,
    four_sequence/1,
    atleast_n_same/2,
    atleast_four_same/1,
    atleast_three_same/1,
    contains_n/2,
    get_sequence/3,
    sum_list_custom/2,
    count_n/3,
    get_unique/2,
    count_unique/2,
    max_count/2,
    num_repeats/2,
    longest_sequence_length/2,
    full_house/1,
    split/3,
    join/3,
    trim/2,
    get_dice_permutation/2,
    dice_combinations/2,
    random_bool/1,
    reversed/2,
    intersection_custom/3,
    difference/3,
    contains/2,
    concatenate/3,
    unordered_equal/2,
    subset_custom/2,
    to_string_vector/2
]).

:- use_module(library(lists)).
:- use_module(library(random)).



/* *********************************************************************
   Predicate Name: all_same/1
   Purpose: Checks if all elements in a list are the same.
   Parameters:
               List - The input list.
  
   Algorithm:
               1) Succeeds if the list is empty or has only one element.
               2) Otherwise, checks if the head of the list is the same as 
                  all other elements in the tail.
   Reference: None
********************************************************************* */
all_same([X | T]).

/* *********************************************************************
   Predicate Name: all_sequence/1
   Purpose: Checks if the elements in a list form a numerical sequence 
            (e.g., [1, 2, 3]).
   Parameters:
               List - The input list.

   Algorithm:
               1) Sorts the list.
               2) Checks if the sorted list is a sequence using `all_sequence_sorted/1`.
   Reference: None
********************************************************************* */
all_sequence(List) :-
    sort(List, SortedList),
    all_sequence_sorted(SortedList).

/* *********************************************************************
   Predicate Name: all_sequence_sorted/1
   Purpose: (Helper predicate) Checks if a sorted list forms a numerical sequence.
   Parameters:
               SortedList - The sorted input list.
   
   Algorithm:
               1) Succeeds if the list is empty or has one element.
               2) Otherwise, checks if each element is one more than the 
                  previous element.
   Reference: None
********************************************************************* */
all_sequence_sorted([]).
all_sequence_sorted([_]).
all_sequence_sorted([X, Y | T]) :-
    Y is X + 1,
    all_sequence_sorted([Y | T]).

/* *********************************************************************
   Predicate Name: contains_sequence/2
   Purpose: Checks if a list contains a sequence of consecutive numbers 
            of a specified length.
   Parameters:
               List - The input list.
               SequenceLength - The required length of the sequence.
   
   Algorithm:
               1) Sorts the list.
               2) Finds a sublist of the specified length.
               3) Checks if the sublist is consecutive.
   Reference: None
********************************************************************* */
contains_sequence(List, SequenceLength) :-
    sort(List, Sorted),
    sublist_of_length(Sorted, SequenceLength, SubList),
    is_consecutive(SubList).

/* *********************************************************************
   Predicate Name: five_sequence/1
   Purpose: Checks if a list contains a sequence of 5 consecutive numbers.
   Parameters:
               List - The input list.
   
   Algorithm:
               1) Uses `contains_sequence/2` with SequenceLength = 5.
   Reference: None
********************************************************************* */
five_sequence(List) :-
    contains_sequence(List, 5).

/* *********************************************************************
   Predicate Name: four_sequence/1
   Purpose: Checks if a list contains a sequence of 4 consecutive numbers.
   Parameters:
               List - The input list.
  
   Algorithm:
               1) Uses `contains_sequence/2` with SequenceLength = 4.
   Reference: None
********************************************************************* */
four_sequence(List) :-
    contains_sequence(List, 4).

/* *********************************************************************
   Predicate Name: sublist_of_length/3
   Purpose: Checks if a list contains a sublist of a specified length.
   Parameters:
               List - The input list.
               Length - The required length of the sublist.
               SubList - Output parameter unified with the sublist if found.
   
   Algorithm:
               1) Splits the list into two parts.
               2) Checks if the first part has the required length.
   Reference: None
********************************************************************* */
sublist_of_length(List, Length, SubList) :-
    append(_, Rest, List),
    append(SubList, _, Rest),
    length(SubList, Length).

/* *********************************************************************
   Predicate Name: is_consecutive/1
   Purpose: Checks if the elements in a list are consecutive numbers.
   Parameters:
               List - The input list.
   
   Algorithm:
               1) Calls `is_consecutive/2` with the tail of the list and 
                  the head of the list as the initial previous value.
   Reference: None
********************************************************************* */
is_consecutive([H|T]) :-
    is_consecutive(T, H).

is_consecutive([], _).
is_consecutive([H|T], Prev) :-
    H is Prev + 1,
    is_consecutive(T, H).

/* *********************************************************************
   Predicate Name: atleast_n_same/2
   Purpose: Checks if a list contains at least N identical elements.
   Parameters:
               List - The input list.
               N - The minimum number of identical elements required.
   
   Algorithm:
               1) Sorts the list.
               2) Groups identical elements together.
               3) Checks if any group has a length greater than or equal to N.
   Reference: None
********************************************************************* */
atleast_n_same(List, N) :-
    msort(List, SortedList),
    group_same(SortedList, Grouped),
    member(Group, Grouped),
    length(Group, Len),
    Len >= N.

/* *********************************************************************
   Predicate Name: atleast_four_same/1
   Purpose: Checks if a list contains at least 4 identical elements.
   Parameters:
               List - The input list.
   
   Algorithm:
               1) Uses `atleast_n_same/2` with N = 4.
   Reference: None
********************************************************************* */
atleast_four_same(List) :-
    atleast_n_same(List, 4).

/* *********************************************************************
   Predicate Name: atleast_three_same/1
   Purpose: Checks if a list contains at least 3 identical elements.
   Parameters:
               List - The input list.
  
   Algorithm:
               1) Uses `atleast_n_same/2` with N = 3.
   Reference: None
********************************************************************* */
atleast_three_same(List) :-
    atleast_n_same(List, 3).

/* *********************************************************************
   Predicate Name: contains_n/2
   Purpose: Checks if a list contains a specific number.
   Parameters:
               List - The input list.
               N - The number to check for.
   
   Algorithm:
               1) Checks if the head of the list is the number.
               2) If not, recursively checks the tail of the list.
   Reference: None
********************************************************************* */
contains_n([N | _], N) :- !.
contains_n([_ | T], N) :-
    contains_n(T, N).

/* *********************************************************************
   Predicate Name: get_sequence/3
   Purpose: Finds a sequence of the specified length in the list.
   Parameters:
               List - The input list.
               SequenceLength - The required length of the sequence.
               Sequence - Output parameter unified with the sequence if found.
   
   Algorithm:
               1) Sorts the list.
               2) Uses `get_sequence_sorted/3` to find the sequence in the 
                  sorted list.
   Reference: None
********************************************************************* */
get_sequence(List, SequenceLength, Sequence) :-
    sort(List, SortedList),
    get_sequence_sorted(SortedList, SequenceLength, Sequence).

/* *********************************************************************
   Predicate Name: to_string_vector/2
   Purpose: Converts a list of integers to a string representation 
            similar to C++'s to_string_vector function.
   Parameters:
               List - The input list of integers.
               String - Output parameter unified with the string representation 
                        of the list.
   
   Algorithm:
               1) Converts each number in the list to a string.
               2) Concatenates the strings with commas and spaces.
               3) Encloses the result in square brackets.
   Reference: None
********************************************************************* */
to_string_vector(List, String) :-
    maplist(number_string, List, StringList),
    atomic_list_concat(StringList, ', ', InnerString),
    format(atom(String), '[~w]', [InnerString]).


/* *********************************************************************
   Predicate Name: get_sequence_sorted/3
   Purpose: (Helper predicate) Finds a sequence of the given length in a 
            sorted list.
   Parameters:
               SortedList - The sorted input list.
               SequenceLength - The required length of the sequence.
               Sequence - Output parameter unified with the sequence if found.
   
   Algorithm:
               1) Takes a suffix of the list with the required length.
               2) Checks if the suffix is a sequence using `all_sequence/1`.
               3) If it is, unifies the suffix with the output Sequence.
   Reference: None
********************************************************************* */
get_sequence_sorted(SortedList, SequenceLength, Sequence) :-
    append(_, Suffix, SortedList),
    length(Suffix, SequenceLength),
    all_sequence(Suffix),
    Sequence = Suffix.


/* *********************************************************************
   Predicate Name: sum_list_custom/2
   Purpose: Calculates the sum of all elements in a list.
   Parameters:
               List - The input list.
               Sum - Output parameter unified with the sum of the elements.
   
   Algorithm:
               1) Calls `sum_list_custom/3` with an initial accumulator of 0.
   Reference: None
********************************************************************* */
sum_list_custom(List, Sum) :-
    sum_list_custom(List, 0, Sum).

/* *********************************************************************
   Predicate Name: sum_list_custom/3
   Purpose: (Helper predicate) Calculates the sum of all elements in a list 
            using an accumulator.
   Parameters:
               List - The input list.
               Acc - The accumulator.
               Sum - Output parameter unified with the sum of the elements.
   
   Algorithm:
               1) If the list is empty, the accumulator holds the sum.
               2) Otherwise, adds the head of the list to the accumulator 
                  and recursively calls itself with the tail of the list and 
                  the new accumulator value.
   Reference: None
********************************************************************* */
sum_list_custom([], Acc, Acc).
sum_list_custom([H | T], Acc, Sum) :-
    NewAcc is Acc + H,
    sum_list_custom(T, NewAcc, Sum).

/* *********************************************************************
   Predicate Name: count_n/3
   Purpose: Counts the occurrences of a specific number in a list.
   Parameters:
               List - The input list.
               N - The number to count.
               Count - Output parameter unified with the count of the number.
   
   Algorithm:
               1) Calls `count_n/4` with an initial accumulator of 0.
   Reference: None
********************************************************************* */
count_n(List, N, Count) :-
    count_n(List, N, 0, Count).

/* *********************************************************************
   Predicate Name: count_n/4
   Purpose: (Helper predicate) Counts the occurrences of a number in a list 
            using an accumulator.
   Parameters:
               List - The input list.
               N - The number to count.
               Acc - The accumulator.
               Count - Output parameter unified with the count of the number.
   
   Algorithm:
               1) If the list is empty, the accumulator holds the count.
               2) If the head of the list is the number, increment the 
                  accumulator and recursively call itself with the tail.
               3) Otherwise, recursively call itself with the tail and the 
                  same accumulator value.
   Reference: None
********************************************************************* */
count_n([], _, Acc, Acc).
count_n([N | T], N, Acc, Count) :-
    NewAcc is Acc + 1,
    count_n(T, N, NewAcc, Count).
count_n([H | T], N, Acc, Count) :-
    H \= N,
    count_n(T, N, Acc, Count).

/* *********************************************************************
   Predicate Name: get_unique/2
   Purpose: Extracts the unique elements from a list.
   Parameters:
               List - The input list.
               UniqueList - Output parameter unified with a list containing 
                            only the unique elements from the input list.
  
   Algorithm:
               1) Sorts the list, which removes duplicate elements.
   Reference: None
********************************************************************* */
get_unique(List, UniqueList) :-
    sort(List, UniqueList).

/* *********************************************************************
   Predicate Name: count_unique/2
   Purpose: Counts the number of unique elements in a list.
   Parameters:
               List - The input list.
               Count - Output parameter unified with the count of unique elements.
  
   Algorithm:
               1) Uses `get_unique/2` to get the unique elements.
               2) Calculates the length of the unique element list.
   Reference: None
********************************************************************* */
count_unique(List, Count) :-
    get_unique(List, UniqueList),
    length(UniqueList, Count).

/* *********************************************************************
   Predicate Name: max_count/2
   Purpose: Determines the maximum frequency of any element in a list.
   Parameters:
               List - The input list.
               MaxCount - Output parameter unified with the maximum count of 
                          any element in the list.
  
   Algorithm:
               1) Sorts the list.
               2) Groups identical elements together.
               3) Calculates the length of each group.
               4) Finds the maximum length among the groups.
   Reference: None
********************************************************************* */
max_count(List, MaxCount) :-
    msort(List, SortedList),
    group_same(SortedList, Grouped),
    maplist(length, Grouped, Lengths),
    max_list(Lengths, MaxCount).

/* *********************************************************************
   Predicate Name: num_repeats/2
   Purpose: Counts the number of repeated elements in a list.
   Parameters:
               List - The input list.
               NumRepeats - Output parameter unified with the number of 
                            repeated elements.
  
   Algorithm:
               1) Sorts the list.
               2) Groups identical elements together.
               3) Calculates the length of each group.
               4) Counts the groups with length greater than 1.
               5) Sums the counts of the repeated groups.
   Reference: None
********************************************************************* */
num_repeats(List, NumRepeats) :-
    msort(List, SortedList),
    group_same(SortedList, Grouped),
    maplist(length, Grouped, Lengths),
    findall(L, (member(L, Lengths), L > 1), Repeats),
    sum_list_custom(Repeats, NumRepeats).

/* *********************************************************************
   Predicate Name: longest_sequence_length/2
   Purpose: Computes the length of the longest consecutive sequence in a list.
   Parameters:
               List - The input list.
               LongestLength - Output parameter unified with the length of the 
                                longest sequence.
   
   Algorithm:
               1) Gets the unique elements from the list.
               2) Sorts the unique elements.
               3) Uses `longest_sequence_length_sorted/2` to find the length 
                  of the longest sequence in the sorted list.
   Reference: None
********************************************************************* */
longest_sequence_length(List, LongestLength) :-
    get_unique(List, UniqueList),
    sort(UniqueList, SortedList),
    longest_sequence_length_sorted(SortedList, LongestLength).

/* *********************************************************************
   Predicate Name: longest_sequence_length_sorted/2
   Purpose: (Helper predicate) Computes the length of the longest consecutive 
            sequence in a sorted list.
   Parameters:
               SortedList - The sorted input list.
               LongestLength - Output parameter unified with the length of the 
                                longest sequence.
  
   Algorithm:
               1) If the list is empty, the longest sequence length is 0.
               2) If the list has one element, the longest sequence length is 1.
               3) Otherwise, checks if the second element is consecutive to 
                  the first. If they are, recursively calculates the length 
                  of the longest sequence in the rest of the list and adds 1. 
                  If they are not consecutive, recursively calculates the 
                  length in the rest of the list.
   Reference: None
********************************************************************* */
longest_sequence_length_sorted([], 0).
longest_sequence_length_sorted([_], 1).
longest_sequence_length_sorted([X, Y | T], LongestLength) :-
    ( Y =:= X + 1 ->
        longest_sequence_length_sorted([Y | T], RestLength),
        LongestLength is RestLength + 1
    ; longest_sequence_length_sorted([Y | T], LongestLength)
    ).

/* *********************************************************************
   Predicate Name: full_house/1
   Purpose: Checks if a list represents a "full house" in Yahtzee 
            (three of one kind and two of another).
   Parameters:
               List - The input list.
   
   Algorithm:
               1) Sorts the list.
               2) Groups identical elements together.
               3) Gets the lengths of the groups.
               4) Checks if the sorted lengths are [2, 3].
   Reference: None
********************************************************************* */
full_house(List) :-
    msort(List, SortedList),
    group_same(SortedList, Grouped),
    maplist(length, Grouped, Lengths),
    sort(Lengths, [2, 3]).

/* *********************************************************************
   Predicate Name: split/3
   Purpose: Splits a string into a list of tokens based on a delimiter.
   Parameters:
               String - The input string.
               Delimiter - The delimiter string.
               Tokens - Output parameter unified with the list of tokens.
  
   Algorithm:
               1) Uses `split_string/4` to split the string.
   Reference: None
********************************************************************* */
split(String, Delimiter, Tokens) :-
    split_string(String, Delimiter, "", Tokens).

/* *********************************************************************
   Predicate Name: join/3
   Purpose: Joins a list of tokens into a string with a given delimiter.
   Parameters:
               Tokens - The list of tokens.
               Delimiter - The delimiter string.
               String - Output parameter unified with the joined string.
   
   Algorithm:
               1) Uses `atomic_list_concat/3` to concatenate the tokens.
   Reference: None
********************************************************************* */
join(Tokens, Delimiter, String) :-
    atomic_list_concat(Tokens, Delimiter, String).

/* *********************************************************************
   Predicate Name: trim/2
   Purpose: Removes leading and trailing whitespace from a string.
   Parameters:
               String - The input string.
               Trimmed - Output parameter unified with the trimmed string.
   
   Algorithm:
               1) Converts the string to a list of character codes.
               2) Uses `trim_codes/2` to trim the codes.
               3) Converts the trimmed codes back to a string.
   Reference: None
********************************************************************* */
trim(String, Trimmed) :-
    string_codes(String, Codes),
    trim_codes(Codes, TrimmedCodes),
    string_codes(Trimmed, TrimmedCodes).

/* *********************************************************************
   Predicate Name: trim_codes/2
   Purpose: (Helper predicate) Removes leading and trailing whitespace 
            from a list of character codes.
   Parameters:
               Codes - The input list of character codes.
               TrimmedCodes - Output parameter unified with the trimmed list 
                              of codes.
  
   Algorithm:
               1) Removes leading whitespace.
               2) Reverses the list.
               3) Removes leading whitespace (which is now trailing).
               4) Reverses the list back to its original order.
   Reference: None
********************************************************************* */
trim_codes(Codes, TrimmedCodes) :-
    drop_leading_whitespace(Codes, NoLeadingWhitespace),
    reverse(NoLeadingWhitespace, Reversed),
    drop_leading_whitespace(Reversed, ReversedTrimmed),
    reverse(ReversedTrimmed, TrimmedCodes).

/* *********************************************************************
   Predicate Name: drop_leading_whitespace/2
   Purpose: (Helper predicate) Removes leading whitespace from a list of 
            character codes.
   Parameters:
               Codes - The input list of character codes.
               NoLeadingWhitespace - Output parameter unified with the list 
                                     of codes without leading whitespace.
   
   Algorithm:
               1) If the head of the list is a space, recursively calls 
                  itself with the tail of the list.
               2) Otherwise, the input list has no leading whitespace.
   Reference: None
********************************************************************* */
drop_leading_whitespace([Code | Codes], NoLeadingWhitespace) :-
    char_type(Code, space),
    drop_leading_whitespace(Codes, NoLeadingWhitespace).
drop_leading_whitespace(Codes, Codes).

/* *********************************************************************
   Predicate Name: get_dice_permutation/2
   Purpose: Generates all possible permutations of dice rolls for a given 
            number of dice.
   Parameters:
               NumDice - The number of dice.
               Permutations - Output parameter unified with a list of 
                              permutations (lists of dice rolls).
   
   Algorithm:
               1) Uses `findall/3` and `dice_permutation/2` to generate all 
                  permutations.
   Reference: None
********************************************************************* */
get_dice_permutation(NumDice, Permutations) :-
    findall(Perm, dice_permutation(NumDice, Perm), Permutations).


/* *********************************************************************
   Predicate Name: dice_permutation/2
   Purpose: (Helper predicate) Generates a single permutation of dice rolls.
   Parameters:
               NumDice - The number of dice.
               Perm - Output parameter unified with a single permutation 
                      (list of dice rolls).
   
   Algorithm:
               1) If NumDice is 0, the permutation is an empty list.
               2) Otherwise, generates a random number between 1 and 6, 
                  decrements NumDice, and recursively generates the rest 
                  of the permutation.
   Reference: None
********************************************************************* */
dice_permutation(0, []).
dice_permutation(NumDice, [Roll | Perm]) :-
    NumDice > 0,
    between(1, 6, Roll),
    NumDice1 is NumDice - 1,
    dice_permutation(NumDice1, Perm).

/* *********************************************************************
   Predicate Name: dice_combinations/2
   Purpose: Generates all possible combinations of dice rolls for a given 
            number of dice.
   Parameters:
               NumDice - The number of dice.
               Combinations - Output parameter unified with a list of 
                              combinations (lists of dice rolls).
   
   Algorithm:
               1) Uses `findall/3` and `dice_combination/2` to generate all 
                  combinations.
   Reference: None
********************************************************************* */
dice_combinations(NumDice, Combinations) :-
    findall(Comb, dice_combination(NumDice, Comb), Combinations).

/* *********************************************************************
   Predicate Name: dice_combination/2
   Purpose: (Helper predicate) Generates a single combination of dice rolls.
   Parameters:
               NumDice - The number of dice.
               Comb - Output parameter unified with a single combination 
                      (list of dice rolls).
   
   Algorithm:
               1) If NumDice is 0, the combination is an empty list.
               2) Otherwise, generates a random number between 1 and 6, 
                  decrements NumDice, and recursively generates the rest 
                  of the combination.
   Reference: None
********************************************************************* */
dice_combination(0, []).
dice_combination(NumDice, [Roll | Comb]) :-
    NumDice > 0,
    between(1, 6, Roll),
    NumDice1 is NumDice - 1,
    dice_combination(NumDice1, Comb).

/* *********************************************************************
   Predicate Name: random_bool/1
   Purpose: Generates a random boolean value (true or false).
   Parameters:
               Bool - Output parameter unified with the random boolean value.
   
   Algorithm:
               1) Generates a random number (0 or 1).
               2) If the number is 0, unifies Bool with false; otherwise, 
                  unifies with true.
   Reference: None
********************************************************************* */
random_bool(Bool) :-
    random_between(0, 1, Rand),
    ( Rand =:= 0 -> Bool = false ; Bool = true ).

/* *********************************************************************
   Predicate Name: reversed/2
   Purpose: Reverses a list.
   Parameters:
               List - The input list.
               ReversedList - Output parameter unified with the reversed list.
  
   Algorithm:
               1) Uses the built-in `reverse/2` predicate.
   Reference: None
********************************************************************* */
reversed(List, ReversedList) :-
    reverse(List, ReversedList).

/* *********************************************************************
   Predicate Name: intersection_custom/3
   Purpose: Calculates the intersection of two lists.
   Parameters:
               List1 - The first input list.
               List2 - The second input list.
               Intersection - Output parameter unified with the intersection 
                              of the two lists.
   
   Algorithm:
               1) Uses the built-in `intersection/3` predicate.
   Reference: None
********************************************************************* */
intersection_custom(List1, List2, Intersection) :-
    intersection(List1, List2, Intersection).

/* *********************************************************************
   Predicate Name: difference/3
   Purpose: Calculates the difference between two lists.
   Parameters:
               List1 - The first input list.
               List2 - The second input list.
               Difference - Output parameter unified with the difference 
                            of the two lists (elements in List1 that are 
                            not in List2).
  
   Algorithm:
               1) Uses the built-in `subtract/3` predicate.
   Reference: None
********************************************************************* */
difference(List1, List2, Difference) :-
    subtract(List1, List2, Difference).

/* *********************************************************************
   Predicate Name: contains/2
   Purpose: Checks if a list contains a specific element.
   Parameters:
               List - The input list.
               Element - The element to check for.
  
   Algorithm:
               1) Uses the built-in `member/2` predicate.
   Reference: None
********************************************************************* */
contains(List, Element) :-
    member(Element, List).

/* *********************************************************************
   Predicate Name: concatenate/3
   Purpose: Concatenates two lists.
   Parameters:
               List1 - The first input list.
               List2 - The second input list.
               Concatenated - Output parameter unified with the concatenated list.
 
   Algorithm:
               1) Uses the built-in `append/3` predicate.
   Reference: None
********************************************************************* */
concatenate(List1, List2, Concatenated) :-
    append(List1, List2, Concatenated).

/* *********************************************************************
   Predicate Name: unordered_equal/2
   Purpose: Checks if two lists have the same elements, regardless of order.
   Parameters:
               List1 - The first input list.
               List2 - The second input list.
   
   Algorithm:
               1) Sorts both lists.
               2) Checks if the sorted lists are equal.
   Reference: None
********************************************************************* */
unordered_equal(List1, List2) :-
    msort(List1, Sorted1),
    msort(List2, Sorted2),
    Sorted1 = Sorted2.

/* *********************************************************************
   Predicate Name: subset_custom/2
   Purpose: Checks if one list is a subset of another list.
   Parameters:
               SubList - The potential subset list.
               List - The larger list.
   
   Algorithm:
               1) Uses the built-in `subset/2` predicate.
   Reference: None
********************************************************************* */
subset_custom(SubList, List) :-
    subset(SubList, List).

/* *********************************************************************
   Predicate Name: group_same/2
   Purpose: Groups consecutive identical elements in a list into sublists.
   Parameters:
               List - The input list.
               Grouped - Output parameter unified with the list of grouped sublists.
   
   Algorithm:
               1) If the list is empty, the result is an empty list.
               2) Otherwise, groups the head with all consecutive identical 
                  elements using `transfer/4` and recursively groups the 
                  remaining elements.
   Reference: None
********************************************************************* */
group_same([], []).
group_same([X | Xs], [[X | Ys] | Zs]) :-
    transfer(X, Xs, Ys, Rest),
    group_same(Rest, Zs).

/* *********************************************************************
   Predicate Name: transfer/4
   Purpose: (Helper predicate) Transfers all consecutive occurrences of an 
            element from a list to a new list.
   Parameters:
               Element - The element to transfer.
               List - The input list.
               Grouped - Output parameter unified with the list of transferred elements.
               Remaining - Output parameter unified with the remaining elements 
                           in the list.
 
   Algorithm:
               1) If the list is empty, both output lists are empty.
               2) If the head of the list is the same as the element, add it 
                  to the Grouped list and recursively call itself with the tail.
               3) Otherwise, the head is different, so the Grouped list is 
                  finished, and the remaining elements are the current head 
                  and the rest of the list.
   Reference: None
********************************************************************* */
transfer(_, [], [], []).
transfer(X, [Y | Ys], [Y | Zs], Rest) :-
    X == Y,
    transfer(X, Ys, Zs, Rest).
transfer(X, [Y | Ys], [], [Y | Ys]) :-
    X \== Y.
