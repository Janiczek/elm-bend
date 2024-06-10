module Main exposing (main)

x = \m n -> m + n * 2

main = (x 1 2, x 3 4)
