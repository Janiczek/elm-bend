module Debug exposing (todo)

{-|

@docs todo

-}

import String exposing (String)


{-| This will be caught by the compiler and it will crash in compile time.
-}
todo : String -> a
todo label =
    todo label
