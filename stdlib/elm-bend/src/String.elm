module String exposing
    ( String(..), isEmpty, length, reverse, repeat, replace
    , append, concat, split, join, words, lines
    , slice, left, right, dropLeft, dropRight
    , contains, startsWith, endsWith, indexes, indices
    , toInt, fromInt
    , toFloat, fromFloat
    , fromChar, cons, uncons
    , toList, fromList
    , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight
    , map, filter, foldl, foldr, any, all
    )

{-| A built-in representation for efficient string manipulation. String literals
are enclosed in `"double quotes"`. Strings are _not_ lists of characters.


# Strings

@docs String, isEmpty, length, reverse, repeat, replace


# Building and Splitting

@docs append, concat, split, join, words, lines


# Get Substrings

@docs slice, left, right, dropLeft, dropRight


# Check for Substrings

@docs contains, startsWith, endsWith, indexes, indices


# Int Conversions

@docs toInt, fromInt


# Float Conversions

@docs toFloat, fromFloat


# Char Conversions

@docs fromChar, cons, uncons


# List Conversions

@docs toList, fromList


# Formatting

Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight


# Higher-Order Functions

@docs map, filter, foldl, foldr, any, all

-}

import Basics exposing (..)
import Bitwise
import Char exposing (Char)
import List exposing ((::))
import Maybe exposing (Maybe)
import Result exposing (Result)



-- STRINGS


{-| A `String` is a chunk of text:

    "Hello!"

    "How are you?"

    "🙈🙉🙊"

    -- strings with escape characters
    "this\n\t\"that\""

    "🙈🙉🙊" -- "🙈🙉🙊"

    -- multiline strings
    """Triple double quotes let you
    create "multiline strings" which
    can have unescaped quotes and newlines.
    """

A `String` can represent any sequence of [unicode characters][u]. You can use
the unicode escapes from `\u{0000}` to `\u{10FFFF}` to represent characters
by their code point. You can also include the unicode characters directly.
Using the escapes can be better if you need one of the many whitespace
characters with different widths.

[u]: https://en.wikipedia.org/wiki/Unicode

**Note:** JavaScript lets you use double quotes and single quotes interchangably.
This is not true in Elm. You must use double quotes for a `String`, and you must
use single quotes for a [`Char`](Char#Char).

-}
type String
    = EmptyString
    | ConsString Char String


{-| Determine if a string is empty.

    isEmpty "" == True

    isEmpty "the world" == False

-}
isEmpty : String -> Bool
isEmpty string =
    string == ""


{-| Get the length of a string.

    length "innumerable" == 11

    length "" == 0

-}
length : String -> Int
length string =
    case string of
        EmptyString ->
            0

        ConsString _ rest ->
            1 + length rest


{-| Reverse a string.

    reverse "stressed" == "desserts"

-}
reverse : String -> String
reverse string =
    case string of
        EmptyString ->
            ""

        ConsString char rest ->
            reverse rest ++ fromChar char


{-| Repeat a string _n_ times.

    repeat 3 "ha" == "hahaha"

-}
repeat : Int -> String -> String
repeat n chunk =
    repeatHelp n chunk ""


repeatHelp : Int -> String -> String -> String
repeatHelp n chunk result =
    if n <= 0 then
        result

    else
        repeatHelp (Bitwise.shiftRightBy 1 n) (chunk ++ chunk) <|
            if Bitwise.and n 1 == 0 then
                result

            else
                result ++ chunk


{-| Replace all occurrences of some substring.

    replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"

    replace "," "/" "a,b,c,d,e" == "a/b/c/d/e"

**Note:** If you need more advanced replacements, check out the
[`elm/parser`][parser] or [`elm/regex`][regex] package.

[parser]: /packages/elm/parser/latest
[regex]: /packages/elm/regex/latest

-}
replace : String -> String -> String -> String
replace before after string =
    join after (split before string)



-- BUILDING AND SPLITTING


{-| Append two strings. You can also use [the `(++)` operator](Basics#++)
to do this.

    append "butter" "fly" == "butterfly"

-}
append : String -> String -> String
append s1 s2 =
    case s1 of
        EmptyString ->
            s2

        ConsString char rest ->
            ConsString char (append rest s2)


{-| Concatenate many strings into one.

    concat [ "never", "the", "less" ] == "nevertheless"

-}
concat : List String -> String
concat strings =
    join "" strings


{-| Split a string using a given separator.

    split "," "cat,dog,cow" == [ "cat", "dog", "cow" ]

    split "/" "home/evan/Desktop/" == [ "home", "evan", "Desktop", "" ]

-}
split : String -> String -> List String
split sep string =
    Debug.todo "String.split"


{-| Put many strings together with a given separator.

    join "a" [ "H", "w", "ii", "n" ] == "Hawaiian"

    join " " [ "cat", "dog", "cow" ] == "cat dog cow"

    join "/" [ "home", "evan", "Desktop" ] == "home/evan/Desktop"

-}
join : String -> List String -> String
join sep chunks =
    Debug.todo "String.join"


{-| Break a string into words, splitting on chunks of whitespace.

    words "How are \t you? \n Good?" == [ "How", "are", "you?", "Good?" ]

-}
words : String -> List String
words =
    Debug.todo "String.words"


{-| Break a string into lines, splitting on newlines.

    lines "How are you?\nGood?" == [ "How are you?", "Good?" ]

-}
lines : String -> List String
lines =
    Debug.todo "String.lines"



-- SUBSTRINGS


{-| Take a substring given a start and end index. Negative indexes
are taken starting from the _end_ of the list.

    slice 7 9 "snakes on a plane!" == "on"

    slice 0 6 "snakes on a plane!" == "snakes"

    slice 0 -7 "snakes on a plane!" == "snakes on a"

    slice -6 -1 "snakes on a plane!" == "plane"

-}
slice : Int -> Int -> String -> String
slice =
    Debug.todo "String.slice"


{-| Take _n_ characters from the left side of a string.

    left 2 "Mulder" == "Mu"

-}
left : Int -> String -> String
left n string =
    if n < 1 then
        ""

    else
        slice 0 n string


{-| Take _n_ characters from the right side of a string.

    right 2 "Scully" == "ly"

-}
right : Int -> String -> String
right n string =
    if n < 1 then
        ""

    else
        slice -n (length string) string


{-| Drop _n_ characters from the left side of a string.

    dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"

-}
dropLeft : Int -> String -> String
dropLeft n string =
    if n < 1 then
        string

    else
        slice n (length string) string


{-| Drop _n_ characters from the right side of a string.

    dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"

-}
dropRight : Int -> String -> String
dropRight n string =
    if n < 1 then
        string

    else
        slice 0 -n string



-- DETECT SUBSTRINGS


{-| See if the second string contains the first one.

    contains "the" "theory" == True

    contains "hat" "theory" == False

    contains "THE" "theory" == False

-}
contains : String -> String -> Bool
contains =
    Debug.todo "String.contains"


{-| See if the second string starts with the first one.

    startsWith "the" "theory" == True

    startsWith "ory" "theory" == False

-}
startsWith : String -> String -> Bool
startsWith =
    Debug.todo "String.startsWith"


{-| See if the second string ends with the first one.

    endsWith "the" "theory" == False

    endsWith "ory" "theory" == True

-}
endsWith : String -> String -> Bool
endsWith =
    Debug.todo "String.endsWith"


{-| Get all of the indexes for a substring in another string.

    indexes "i" "Mississippi" == [ 1, 4, 7, 10 ]

    indexes "ss" "Mississippi" == [ 2, 5 ]

    indexes "needle" "haystack" == []

-}
indexes : String -> String -> List Int
indexes =
    Debug.todo "String.indexes"


{-| Alias for `indexes`.
-}
indices : String -> String -> List Int
indices =
    indexes



-- FORMATTING


{-| Convert a string to all upper case. Useful for case-insensitive comparisons
and VIRTUAL YELLING.

    toUpper "skinner" == "SKINNER"

-}
toUpper : String -> String
toUpper =
    Debug.todo "String.toUpper"


{-| Convert a string to all lower case. Useful for case-insensitive comparisons.

    toLower "X-FILES" == "x-files"

-}
toLower : String -> String
toLower =
    Debug.todo "String.toLower"


{-| Pad a string on both sides until it has a given length.

    pad 5 ' ' "1" == "  1  "

    pad 5 ' ' "11" == "  11 "

    pad 5 ' ' "121" == " 121 "

-}
pad : Int -> Char -> String -> String
pad n char string =
    let
        half =
            Basics.toFloat (n - length string) / 2
    in
    repeat (ceiling half) (fromChar char) ++ string ++ repeat (floor half) (fromChar char)


{-| Pad a string on the left until it has a given length.

    padLeft 5 '.' "1" == "....1"

    padLeft 5 '.' "11" == "...11"

    padLeft 5 '.' "121" == "..121"

-}
padLeft : Int -> Char -> String -> String
padLeft n char string =
    repeat (n - length string) (fromChar char) ++ string


{-| Pad a string on the right until it has a given length.

    padRight 5 '.' "1" == "1...."

    padRight 5 '.' "11" == "11..."

    padRight 5 '.' "121" == "121.."

-}
padRight : Int -> Char -> String -> String
padRight n char string =
    string ++ repeat (n - length string) (fromChar char)


{-| Get rid of whitespace on both sides of a string.

    trim "  hats  \n" == "hats"

-}
trim : String -> String
trim =
    Debug.todo "String.trim"


{-| Get rid of whitespace on the left of a string.

    trimLeft "  hats  \n" == "hats  \n"

-}
trimLeft : String -> String
trimLeft =
    Debug.todo "String.trimLeft"


{-| Get rid of whitespace on the right of a string.

    trimRight "  hats  \n" == "  hats"

-}
trimRight : String -> String
trimRight =
    Debug.todo "String.trimRight"



-- INT CONVERSIONS


{-| Try to convert a string into an int, failing on improperly formatted strings.

    String.toInt "123" == Just 123

    String.toInt "-42" == Just -42

    String.toInt "3.1" == Nothing

    String.toInt "31a" == Nothing

If you are extracting a number from some raw user input, you will typically
want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:

    Maybe.withDefault 0 (String.toInt "42") == 42

    Maybe.withDefault 0 (String.toInt "ab") == 0

-}
toInt : String -> Maybe Int
toInt =
    Debug.todo "String.toInt"


{-| Convert an `Int` to a `String`.

    String.fromInt 123 == "123"

    String.fromInt -42 == "-42"

Check out [`Debug.toString`](Debug#toString) to convert _any_ value to a string
for debugging purposes.

-}
fromInt : Int -> String
fromInt =
    Debug.todo "String.fromInt"



-- FLOAT CONVERSIONS


{-| Try to convert a string into a float, failing on improperly formatted strings.

    String.toFloat "123" == Just 123.0

    String.toFloat "-42" == Just -42.0

    String.toFloat "3.1" == Just 3.1

    String.toFloat "31a" == Nothing

If you are extracting a number from some raw user input, you will typically
want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:

    Maybe.withDefault 0 (String.toFloat "42.5") == 42.5

    Maybe.withDefault 0 (String.toFloat "cats") == 0

-}
toFloat : String -> Maybe Float
toFloat =
    Debug.todo "String.toFloat"


{-| Convert a `Float` to a `String`.

    String.fromFloat 123 == "123"

    String.fromFloat -42 == "-42"

    String.fromFloat 3.9 == "3.9"

Check out [`Debug.toString`](Debug#toString) to convert _any_ value to a string
for debugging purposes.

-}
fromFloat : Float -> String
fromFloat =
    Debug.todo "String.fromFloat"



-- LIST CONVERSIONS


{-| Convert a string to a list of characters.

    toList "abc" == [ 'a', 'b', 'c' ]

    toList "🙈🙉🙊" == [ '🙈', '🙉', '🙊' ]

-}
toList : String -> List Char
toList string =
    foldr (::) [] string


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarily by consing, perhaps for decoding
something.

    fromList [ 'a', 'b', 'c' ] == "abc"

    fromList [ '🙈', '🙉', '🙊' ] == "🙈🙉🙊"

-}
fromList : List Char -> String
fromList =
    Debug.todo "String.fromList"



-- CHAR CONVERSIONS


{-| Create a string from a given character.

    fromChar 'a' == "a"

-}
fromChar : Char -> String
fromChar char =
    cons char ""


{-| Add a character to the beginning of a string.

    cons 'T' "he truth is out there" == "The truth is out there"

-}
cons : Char -> String -> String
cons =
    ConsString


{-| Split a non-empty string into its head and tail. This lets you
pattern match on strings exactly as you would with lists.

    uncons "abc" == Just ( 'a', "bc" )

    uncons "" == Nothing

-}
uncons : String -> Maybe ( Char, String )
uncons =
    Debug.todo "String.uncons"



-- HIGHER-ORDER FUNCTIONS


{-| Transform every character in a string

    map
        (\c ->
            if c == '/' then
                '.'

            else
                c
        )
        "a/b/c"
        == "a.b.c"

-}
map : (Char -> Char) -> String -> String
map fn s =
    case s of
        EmptyString ->
            ""

        ConsString char rest ->
            ConsString (fn char) (map fn rest)


{-| Keep only the characters that pass the test.

    filter isDigit "R2-D2" == "22"

-}
filter : (Char -> Bool) -> String -> String
filter =
    Debug.todo "String.filter"


{-| Reduce a string from the left.

    foldl cons "" "time" == "emit"

-}
foldl : (Char -> b -> b) -> b -> String -> b
foldl =
    Debug.todo "String.foldl"


{-| Reduce a string from the right.

    foldr cons "" "time" == "time"

-}
foldr : (Char -> b -> b) -> b -> String -> b
foldr =
    Debug.todo "String.foldr"


{-| Determine whether _any_ characters pass the test.

    any isDigit "90210" == True

    any isDigit "R2-D2" == True

    any isDigit "heart" == False

-}
any : (Char -> Bool) -> String -> Bool
any =
    Debug.todo "String.any"


{-| Determine whether _all_ characters pass the test.

    all isDigit "90210" == True

    all isDigit "R2-D2" == False

    all isDigit "heart" == False

-}
all : (Char -> Bool) -> String -> Bool
all =
    Debug.todo "String.all"