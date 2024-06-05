{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import Prelude hiding (init)
import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>))
import Text.Read (readMaybe)

import qualified Elm.Version as V
import Terminal
import Terminal.Helpers

import qualified Init
import qualified Install
import qualified Make



-- MAIN


main :: IO ()
main =
  Terminal.app intro outro
    [ init
    , make
    , install
    ]


intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        ["Hi,","thank","you","for","trying","out"
        ,P.green "Elm"
        ,P.green (P.text (V.toChars V.compiler)) <> "."
        ,"I hope you like it!"
        ]
    , ""
    , P.black "-------------------------------------------------------------------------------"
    , P.black "I highly recommend working through <https://guide.elm-lang.org> to get started."
    , P.black "It teaches many important concepts, including how to use `elm` in the terminal."
    , P.black "-------------------------------------------------------------------------------"
    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and\
    \ happy to help out. They hang out there because it is fun, so be kind to get the\
    \ best results!"



-- INIT


init :: Terminal.Command
init =
  let
    summary =
      "Start an Elm project. It creates a starter elm.json file and\
      \ provides a link explaining what to do from there."

    details =
      "The `init` command helps start Elm projects:"

    example =
      reflow
        "It will ask permission to create an elm.json file, the one thing common\
        \ to all Elm projects. It also provides a link explaining what to do from there."
  in
  Terminal.Command "init" (Common summary) details example noArgs noFlags Init.run



-- MAKE


make :: Terminal.Command
make =
  let
    details =
      "The `make` command compiles Elm code into Bend:"

    example =
      stack
        [ reflow
            "For example:"
        , P.indent 4 $ P.green "elm make src/Main.elm"
        , reflow
            "This tries to compile an Elm file named src/Main.elm, generating an out.bend\
            \ file if possible."
        ]

    makeFlags =
      flags Make.Flags
        |-- flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/elm.js to generate the JS at assets/elm.js or --output=/dev/null to generate no output at all!"
        |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
  in
  Terminal.Command "make" Uncommon details example (optional elmFile) makeFlags Make.run



-- INSTALL


install :: Terminal.Command
install =
  let
    details =
      "The `install` command fetches packages from <https://package.elm-lang.org> for\
      \ use in your project:"

    example =
      stack
        [ reflow
            "For example, if you want to get packages for HTTP and JSON, you would say:"
        , P.indent 4 $ P.green $ P.vcat $
              [ "elm install elm/http"
              , "elm install elm/json"
              ]
        , reflow
            "Notice that you must say the AUTHOR name and PROJECT name! After running those\
            \ commands, you could say `import Http` or `import Json.Decode` in your code."
        , reflow
            "What if two projects use different versions of the same package? No problem!\
            \ Each project is independent, so there cannot be conflicts like that!"
        ]

    installArgs =
      oneOf
        [ require0 Install.NoArgs
        , require1 Install.Install package
        ]
  in
  Terminal.Command "install" Uncommon details example installArgs noFlags Install.run



-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
