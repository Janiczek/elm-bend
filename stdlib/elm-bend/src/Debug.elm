module Debug exposing (todo)

{-|

@docs todo

-}


todo : () -> a
todo () =
    {- This way of crashing run-time will stop working once Bend supports
       cloning non-affine lambdas. We'll need to figure out another way then.
    -}
    let
        nonaffine x =
            ( x, x )

        boom =
            ( nonaffine, nonaffine )
    in
    todo ()
