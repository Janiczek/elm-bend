module Bitwise exposing
    ( and, or, xor, complement
    , shiftLeftBy, shiftRightBy, shiftRightZfBy
    )

{-| Library for [bitwise operations](https://en.wikipedia.org/wiki/Bitwise_operation).


# Basic Operations

@docs and, or, xor, complement


# Bit Shifts

@docs shiftLeftBy, shiftRightBy, shiftRightZfBy

-}

import Basics exposing (Int)
import Debug


{-| Bitwise AND
-}
and : Int -> Int -> Int
and _ _ =
    Debug.todo ()


{-| Bitwise OR
-}
or : Int -> Int -> Int
or _ _ =
    Debug.todo ()


{-| Bitwise XOR
-}
xor : Int -> Int -> Int
xor _ _ =
    Debug.todo ()


{-| Flip each bit individually, often called bitwise NOT
-}
complement : Int -> Int
complement _ =
    Debug.todo ()


{-| Shift bits to the left by a given offset, filling new bits with zeros.
This can be used to multiply numbers by powers of two.

    shiftLeftBy 1 5 == 10

    shiftLeftBy 5 1 == 32

-}
shiftLeftBy : Int -> Int -> Int
shiftLeftBy _ _ =
    Debug.todo ()


{-| Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This can be used to divide numbers by powers of two.

    shiftRightBy 1 32 == 16

    shiftRightBy 2 32 == 8

    shiftRightBy 1 -32 == -16

This is called an [arithmetic right shift][ars], often written `>>`, and
sometimes called a sign-propagating right shift because it fills empty spots
with copies of the highest bit.

[ars]: https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift

-}
shiftRightBy : Int -> Int -> Int
shiftRightBy _ _ =
    Debug.todo ()


{-| Shift bits to the right by a given offset, filling new bits with zeros.

    shiftRightZfBy 1 32 == 16

    shiftRightZfBy 2 32 == 8

    shiftRightZfBy 1 -32 == 2147483632

This is called an [logical right shift][lrs], often written `>>>`, and
sometimes called a zero-fill right shift because it fills empty spots with
zeros.

[lrs]: https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift

-}
shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy _ _ =
    Debug.todo ()
