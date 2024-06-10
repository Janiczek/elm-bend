module Main exposing (main)

x = \m n -> m + n * 2

t1 _ = x 3 4
t2 _ = x 1 2

main = 
    let
        (d1,d2) = (t1 (),t2 ())
    in
    (d1,d2,999)
