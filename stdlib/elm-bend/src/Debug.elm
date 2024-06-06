module Debug exposing (todo)

{-|

@docs todo

-}

import String exposing (String)


{-| Will be special-handled by the Elm->Bend compiler (compiled into a
DebugTodo expression)
-}
todo : String -> a
todo label =
    todo label
