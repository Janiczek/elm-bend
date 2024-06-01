module Debug exposing (todo)

{-|

@docs todo

-}


{-| This will be caught by the compiler and it will crash in compile time.
-}
todo : String -> a
todo label =
    todo label
