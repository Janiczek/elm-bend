{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit
  ( Init(..), initToReport
  , Make(..), makeToReport
  , Install(..), installToReport
  , newPackageOverview
  --
  , Solver(..)
  , Outline(..)
  , OutlineProblem(..)
  , Details(..)
  , DetailsBadDep(..)
  , PackageProblem(..)
  , RegistryProblem(..)
  , BuildProblem(..)
  , BuildProjectProblem(..)
  , DocsProblem(..)
  , Generate(..)
  --
  , toString
  , toStderr
  , toJson
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified Elm.Constraint as C
import qualified Elm.Magnitude as M
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified Json.String as Json
import Parse.Primitives (Row, Col)
import qualified Reporting.Annotation as A
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Error.Import as Import
import qualified Reporting.Error.Json as Json
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Error as Error
import qualified Reporting.Render.Code as Code



-- RENDERERS


toString :: Help.Report -> String
toString report =
  Help.toString (Help.reportToDoc report)


toStderr :: Help.Report -> IO ()
toStderr report =
  Help.toStderr (Help.reportToDoc report)


toJson :: Help.Report -> Encode.Value
toJson report =
  Help.reportToJson report



-- INIT


data Init
  = InitNoSolution [Pkg.Name]
  | InitNoOfflineSolution [Pkg.Name]
  | InitSolverProblem Solver
  | InitAlreadyExists
  | InitRegistryProblem RegistryProblem


initToReport :: Init -> Help.Report
initToReport exit =
  case exit of
    InitNoSolution pkgs ->
      Help.report "NO SOLUTION" Nothing
        "I tried to create an elm.json with the following direct dependencies:"
        [ D.indent 4 $ D.vcat $
            map (D.dullyellow . D.fromChars . Pkg.toChars) pkgs
        , D.reflow $
            "I could not find compatible versions though! This should not happen, so please\
            \ ask around one of the community forums at https://elm-lang.org/community to learn\
            \ what is going on!"
        ]

    InitNoOfflineSolution pkgs ->
      Help.report "NO OFFLINE SOLUTION" Nothing
        "I tried to create an elm.json with the following direct dependencies:"
        [ D.indent 4 $ D.vcat $
            map (D.dullyellow . D.fromChars . Pkg.toChars) pkgs
        , D.reflow $
            "I could not find compatible versions though, but that may be because I could not\
            \ connect to https://package.elm-lang.org to get the latest list of packages. Are\
            \ you able to connect to the internet? Please ask around one of the community\
            \ forums at https://elm-lang.org/community for help!"
        ]

    InitSolverProblem solver ->
      toSolverReport solver

    InitAlreadyExists ->
      Help.report "EXISTING PROJECT" Nothing
        "You already have an elm.json file, so there is nothing for me to initialize!"
        [ D.fillSep
            ["Maybe",D.green (D.fromChars (D.makeLink "init")),"can","help"
            ,"you","figure","out","what","to","do","next?"
            ]
        ]

    InitRegistryProblem problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem $
        "I need the list of published packages before I can start initializing projects"



-- OVERVIEW OF VERSIONING


newPackageOverview :: String
newPackageOverview =
  unlines
    [ "This package has never been published before. Here's how things work:"
    , ""
    , "  - Versions all have exactly three parts: MAJOR.MINOR.PATCH"
    , ""
    , "  - All packages start with initial version " ++ V.toChars V.one
    , ""
    , "  - Versions are incremented based on how the API changes:"
    , ""
    , "        PATCH = the API is the same, no risk of breaking code"
    , "        MINOR = values have been added, existing values are unchanged"
    , "        MAJOR = existing values have been changed or removed"
    , ""
    , "  - I will bump versions for you, automatically enforcing these rules"
    , ""
    ]



-- DOCS


data DocsProblem
  = DP_Http Http.Error
  | DP_Data String BS.ByteString
  | DP_Cache


toDocsProblemReport :: DocsProblem -> String -> Help.Report
toDocsProblemReport problem context =
  case problem of
    DP_Http httpError ->
      toHttpErrorReport "PROBLEM LOADING DOCS" httpError context

    DP_Data url body ->
      Help.report "PROBLEM LOADING DOCS" Nothing (context ++ ", so I fetched:")
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "I got the data back, but it was not what I was expecting. The response\
            \ body contains " ++ show (BS.length body) ++ " bytes. Here is the "
            ++ if BS.length body <= 76 then "whole thing:" else "beginning:"
        , D.indent 4 $ D.dullyellow $ D.fromChars $
            if BS.length body <= 76
            then BS_UTF8.toString body
            else take 73 (BS_UTF8.toString body) ++ "..."
        , D.reflow $
            "Does this error keep showing up? Maybe there is something weird with your\
            \ internet connection. We have gotten reports that schools, businesses,\
            \ airports, etc. sometimes intercept requests and add things to the body\
            \ or change its contents entirely. Could that be the problem?"
        ]

    DP_Cache ->
      Help.report "PROBLEM LOADING DOCS" Nothing (context ++ ", but the local copy seems to be corrupted.")
        [ D.reflow $
            "I deleted the cached version, so the next run should download a fresh copy of\
            \ the docs. Hopefully that will get you unstuck, but it will not resolve the root\
            \ problem if, for example, a 3rd party editor plugin is modifing cached files\
            \ for some reason."
        ]



-- INSTALL


data Install
  = InstallNoOutline
  | InstallBadOutline Outline
  | InstallBadRegistry RegistryProblem
  | InstallNoArgs FilePath
  | InstallNoOnlineAppSolution Pkg.Name
  | InstallNoOfflineAppSolution Pkg.Name
  | InstallNoOnlinePkgSolution Pkg.Name
  | InstallNoOfflinePkgSolution Pkg.Name
  | InstallHadSolverTrouble Solver
  | InstallUnknownPackageOnline Pkg.Name [Pkg.Name]
  | InstallUnknownPackageOffline Pkg.Name [Pkg.Name]
  | InstallBadDetails Details


installToReport :: Install -> Help.Report
installToReport exit =
  case exit of
    InstallNoOutline ->
      Help.report "NEW PROJECT?" Nothing
        "Are you trying to start a new project? Try this command instead:"
        [ D.indent 4 $ D.green "elm init"
        , D.reflow "It will help you get started!"
        ]

    InstallBadOutline outline ->
      toOutlineReport outline

    InstallBadRegistry problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem $
        "I need the list of published packages to figure out how to install things"

    InstallNoArgs elmHome ->
      Help.report "INSTALL WHAT?" Nothing
        "I am expecting commands like:"
        [ D.green $ D.indent 4 $ D.vcat $
            [ "elm install elm/http"
            , "elm install elm/json"
            , "elm install elm/random"
            ]
        , D.toFancyHint
            ["In","JavaScript","folks","run","`npm install`","to","start","projects."
            ,"\"Gotta","download","everything!\"","But","why","download","packages"
            ,"again","and","again?","Instead,","Elm","caches","packages","in"
            ,D.dullyellow (D.fromChars elmHome)
            ,"so","each","one","is","downloaded","and","built","ONCE","on","your","machine."
            ,"Elm","projects","check","that","cache","before","trying","the","internet."
            ,"This","reduces","build","times,","reduces","server","costs,","and","makes","it"
            ,"easier","to","work","offline.","As","a","result"
            ,D.dullcyan "elm install","is","only","for","adding","dependencies","to","elm.json,"
            ,"whereas",D.dullcyan "elm make","is","in","charge","of","gathering","dependencies"
            ,"and","building","everything.","So","maybe","try",D.green "elm make","instead?"
            ]
        ]

    InstallNoOnlineAppSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION" (Just "elm.json")
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible\
          \ with your existing dependencies."
        )
        [ D.reflow $
            "I checked all the published versions. When that failed, I tried to find any\
            \ compatible combination of these packages, even if it meant changing all your\
            \ existing dependencies! That did not work either!"
        , D.reflow $
            "This is most likely to happen when a package is not upgraded yet. Maybe a new\
            \ version of Elm came out recently? Maybe a common package was changed recently?\
            \ Maybe a better package came along, so there was no need to upgrade this one?\
            \ Try asking around https://elm-lang.org/community to learn what might be going on\
            \ with this package."
        , D.toSimpleNote $
            "Whatever the case, please be kind to the relevant package authors! Having\
            \ friendly interactions with users is great motivation, and conversely, getting\
            \ berated by strangers on the internet sucks your soul dry. Furthermore, package\
            \ authors are humans with families, friends, jobs, vacations, responsibilities,\
            \ goals, etc. They face obstacles outside of their technical work you will never\
            \ know about, so please assume the best and try to be patient and supportive!"
        ]

    InstallNoOfflineAppSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION LOCALLY" (Just "elm.json")
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible\
          \ with your existing dependencies."
        )
        [ D.reflow $
            "I was not able to connect to https://package.elm-lang.org/ though, so I was only\
            \ able to look through packages that you have downloaded in the past."
        , D.reflow $
            "Try again later when you have internet!"
        ]

    InstallNoOnlinePkgSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION" (Just "elm.json")
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible\
          \ with your existing constraints."
        )
        [ D.reflow $
            "With applications, I try to broaden the constraints to see if anything works,\
            \ but messing with package constraints is much more delicate business. E.g. making\
            \ your constraints stricter may make it harder for applications to find compatible\
            \ dependencies. So fixing something here may break it for a lot of other people!"
        , D.reflow $
            "So I recommend making an application with the same dependencies as your package.\
            \ See if there is a solution at all. From there it may be easier to figure out\
            \ how to proceed in a way that will disrupt your users as little as possible. And\
            \ the solution may be to help other package authors to get their packages updated,\
            \ or to drop a dependency entirely."
        ]

    InstallNoOfflinePkgSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION LOCALLY" (Just "elm.json")
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible\
          \ with your existing constraints."
        )
        [ D.reflow $
            "I was not able to connect to https://package.elm-lang.org/ though, so I was only\
            \ able to look through packages that you have downloaded in the past."
        , D.reflow $
            "Try again later when you have internet!"
        ]

    InstallHadSolverTrouble solver ->
      toSolverReport solver

    InstallUnknownPackageOnline pkg suggestions ->
      Help.docReport "UNKNOWN PACKAGE" Nothing
        (
          D.fillSep
            ["I","cannot","find","a","package","named",D.red (D.fromPackage pkg) <> "."]
        )
        [ D.reflow $
            "I looked through https://package.elm-lang.org for packages with similar names\
            \ and found these:"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map D.fromPackage suggestions
        , D.reflow $ "Maybe you want one of these instead?"
        ]

    InstallUnknownPackageOffline pkg suggestions ->
      Help.docReport "UNKNOWN PACKAGE" Nothing
        (
          D.fillSep
            ["I","cannot","find","a","package","named",D.red (D.fromPackage pkg) <> "."]
        )
        [ D.reflow $
            "I could not connect to https://package.elm-lang.org though, so new packages may\
            \ have been published since I last updated my local cache of package names."
        , D.reflow $
            "Looking through the locally cached names, the closest ones are:"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map D.fromPackage suggestions
        , D.reflow $ "Maybe you want one of these instead?"
        ]

    InstallBadDetails details ->
      toDetailsReport details



-- SOLVER


data Solver
  = SolverBadCacheData Pkg.Name V.Version
  | SolverBadHttpData Pkg.Name V.Version String
  | SolverBadHttp Pkg.Name V.Version Http.Error


toSolverReport :: Solver -> Help.Report
toSolverReport problem =
  case problem of
    SolverBadCacheData pkg vsn ->
      Help.report "PROBLEM SOLVING PACKAGE CONSTRAINTS" Nothing
        (
          "I need the elm.json of " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " to\
          \ help me search for a set of compatible packages. I had it cached locally, but\
          \ it looks like the file was corrupted!"
        )
        [ D.reflow $
            "I deleted the cached version, so the next run should download a fresh copy.\
            \ Hopefully that will get you unstuck, but it will not resolve the root\
            \ problem if a 3rd party tool is modifing cached files for some reason."
        ]

    SolverBadHttpData pkg vsn url ->
      Help.report "PROBLEM SOLVING PACKAGE CONSTRAINTS" Nothing
        (
          "I need the elm.json of " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " to\
          \ help me search for a set of compatible packages, but I ran into corrupted\
          \ information from:"
        )
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "Is something weird with your internet connection. We have gotten reports that\
            \ schools, businesses, airports, etc. sometimes intercept requests and add things\
            \ to the body or change its contents entirely. Could that be the problem?"
        ]

    SolverBadHttp pkg vsn httpError ->
      toHttpErrorReport "PROBLEM SOLVING PACKAGE CONSTRAINTS" httpError $
        "I need the elm.json of " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn
        ++ " to help me search for a set of compatible packages"



-- OUTLINE


data Outline
  = OutlineHasBadStructure (Decode.Error OutlineProblem)
  | OutlineHasMissingSrcDirs FilePath [FilePath]
  | OutlineHasDuplicateSrcDirs FilePath FilePath FilePath
  | OutlineNoPkgBend
  | OutlineNoAppBend


data OutlineProblem
  = OP_BadType
  | OP_BadPkgName Row Col
  | OP_BadVersion Row Col
  | OP_BadConstraint C.Error
  | OP_BadModuleName Row Col
  | OP_BadModuleHeaderTooLong
  | OP_BadDependencyName Row Col
  | OP_BadLicense Json.String [Json.String]
  | OP_BadSummaryTooLong
  | OP_NoSrcDirs


toOutlineReport :: Outline -> Help.Report
toOutlineReport problem =
  case problem of
    OutlineHasBadStructure decodeError ->
      Json.toReport "elm.json" (Json.FailureToReport toOutlineProblemReport) decodeError $
        Json.ExplicitReason "I ran into a problem with your elm.json file."

    OutlineHasMissingSrcDirs dir dirs ->
      case dirs of
        [] ->
          Help.report "MISSING SOURCE DIRECTORY" (Just "elm.json")
            "I need a valid elm.json file, but the \"source-directories\" field lists the following directory:"
            [ D.indent 4 $ D.red $ D.fromChars dir
            , D.reflow $
                "I cannot find it though. Is it missing? Is there a typo?"
            ]

        _:_ ->
          Help.report "MISSING SOURCE DIRECTORIES" (Just "elm.json")
            "I need a valid elm.json file, but the \"source-directories\" field lists the following directories:"
            [ D.indent 4 $ D.vcat $
                map (D.red . D.fromChars) (dir:dirs)
            , D.reflow $
                "I cannot find them though. Are they missing? Are there typos?"
            ]

    OutlineHasDuplicateSrcDirs canonicalDir dir1 dir2 ->
      if dir1 == dir2 then
        Help.report "REDUNDANT SOURCE DIRECTORIES" (Just "elm.json")
          "I need a valid elm.json file, but the \"source-directories\" field lists the same directory twice:"
          [ D.indent 4 $ D.vcat $
              map (D.red . D.fromChars) [dir1,dir2]
          , D.reflow $
              "Remove one of the entries!"
          ]
      else
        Help.report "REDUNDANT SOURCE DIRECTORIES" (Just "elm.json")
          "I need a valid elm.json file, but the \"source-directories\" field has some redundant directories:"
          [ D.indent 4 $ D.vcat $
              map (D.red . D.fromChars) [dir1,dir2]
          , D.reflow $
              "These are two different ways of refering to the same directory:"
          , D.indent 4 $ D.dullyellow $ D.fromChars canonicalDir
          , D.reflow $
              "Remove one of the redundant entries from your \"source-directories\" field."
          ]

    OutlineNoPkgBend ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "I need to see a \"Janiczek/elm-bend\" dependency your elm.json file. The default imports\
        \ of `List` and `Maybe` do not work without it."
        [ D.reflow $
            "If you modified your elm.json by hand, try to change it back! And if you are\
            \ having trouble getting back to a working elm.json, it may be easier to find a\
            \ working package and start fresh with their elm.json file."
        ]

    OutlineNoAppBend ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "I need to see a \"Janiczek/elm-bend\" dependency your elm.json file. The default imports\
        \ of `List` and `Maybe` do not work without it."
        [ D.reflow $
            "If you modified your elm.json by hand, try to change it back! And if you are\
            \ having trouble getting back to a working elm.json, it may be easier to delete it\
            \ and use `elm init` to start fresh."
        ]


toOutlineProblemReport :: FilePath -> Code.Source -> Json.Context -> A.Region -> OutlineProblem -> Help.Report
toOutlineProblemReport path source _ region problem =
  let
    toHighlight row col =
      Just $ A.Region (A.Position row col) (A.Position row col)

    toSnippet title highlight pair =
      Help.jsonReport title (Just path) $
        Code.toSnippet source region highlight pair
  in
  case problem of
    OP_BadType ->
      toSnippet "UNEXPECTED TYPE" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. I cannot handle a \"type\" like this:"
        , D.fillSep
            ["Try","changing","the","\"type\"","to"
            ,D.green "\"application\"","or",D.green "\"package\"","instead."
            ]
        )

    OP_BadPkgName row col ->
      toSnippet "INVALID PACKAGE NAME" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. I ran into trouble with the package name:"
        , D.stack
            [ D.fillSep
                ["Package","names","are","always","written","as"
                ,D.green "\"author/project\""
                ,"so","I","am","expecting","to","see","something","like:"
                ]
            , D.dullyellow $ D.indent 4 $ D.vcat $
                [ "\"mdgriffith/elm-ui\""
                , "\"w0rm/elm-physics\""
                , "\"Microsoft/elm-json-tree-view\""
                , "\"FordLabs/elm-star-rating\""
                , "\"1602/json-schema\""
                ]
            , D.reflow
                "The author name should match your GitHub name exactly, and the project name\
                \ needs to follow these rules:"
            , D.indent 4 $ D.vcat $
                [ "+--------------------------------------+-----------+-----------+"
                , "| RULE                                 | BAD       | GOOD      |"
                , "+--------------------------------------+-----------+-----------+"
                , "| only lower case, digits, and hyphens | elm-HTTP  | elm-http  |"
                , "| no leading digits                    | 3D        | elm-3d    |"
                , "| no non-ASCII characters              | elm-bjørn | elm-bear  |"
                , "| no underscores                       | elm_ui    | elm-ui    |"
                , "| no double hyphens                    | elm--hash | elm-hash  |"
                , "| no starting or ending hyphen         | -elm-tar- | elm-tar   |"
                , "+--------------------------------------+-----------+-----------+"
                ]
            , D.toSimpleNote $
                "These rules only apply to the project name, so you should never need\
                \ to change your GitHub name!"
            ]
        )

    OP_BadVersion row col ->
      toSnippet "PROBLEM WITH VERSION" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. I was expecting a version number here:"
        , D.fillSep
            ["I","need","something","like",D.green "\"1.0.0\"","or",D.green "\"2.0.4\""
            ,"that","explicitly","states","all","three","numbers!"
            ]
        )

    OP_BadConstraint constraintError ->
      case constraintError of
        C.BadFormat row col ->
          toSnippet "PROBLEM WITH CONSTRAINT" (toHighlight row col)
            ( D.reflow $
                "I got stuck while reading your elm.json file. I do not understand this version constraint:"
            , D.stack
                [ D.fillSep
                    ["I","need","something","like",D.green "\"1.0.0 <= v < 2.0.0\""
                    ,"that","explicitly","lists","the","lower","and","upper","bounds."
                    ]
                , D.toSimpleNote $
                    "The spaces in there are required! Taking them out will confuse me. Adding\
                    \ extra spaces confuses me too. I recommend starting with a valid example\
                    \ and just changing the version numbers."
                ]
            )

        C.InvalidRange before after ->
          if before == after then
            toSnippet "PROBLEM WITH CONSTRAINT" Nothing
              ( D.reflow $
                  "I got stuck while reading your elm.json file. I ran into an invalid version constraint:"
              , D.fillSep
                  ["Elm","checks","that","all","package","APIs","follow","semantic","versioning,"
                  ,"so","it","is","best","to","use","wide","constraints.","I","recommend"
                  ,D.green $ "\"" <> D.fromVersion before <> " <= v < " <> D.fromVersion (V.bumpMajor after) <> "\""
                  ,"since","it","is","guaranteed","that","breaking","API","changes","cannot"
                  ,"happen","in","any","of","the","versions","in","that","range."
                  ]
              )

          else
            toSnippet "PROBLEM WITH CONSTRAINT" Nothing
              ( D.reflow $
                  "I got stuck while reading your elm.json file. I ran into an invalid version constraint:"
              , D.fillSep
                  ["Maybe","you","want","something","like"
                  ,D.green $ "\"" <> D.fromVersion before <> " <= v < " <> D.fromVersion (V.bumpMajor before) <> "\""
                  ,"instead?","Elm","checks","that","all","package","APIs","follow","semantic"
                  ,"versioning,","so","it","is","guaranteed","that","breaking","API","changes"
                  ,"cannot","happen","in","any","of","the","versions","in","that","range."
                  ]
              )

    OP_BadModuleName row col ->
      toSnippet "PROBLEM WITH MODULE NAME" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. I was expecting a module name here:"
        , D.fillSep
            ["I","need","something","like",D.green "\"Html.Events\""
            ,"or",D.green "\"Browser.Navigation\""
            ,"where","each","segment","starts","with","a","capital"
            ,"letter","and","the","segments","are","separated","by","dots."
            ]
        )

    OP_BadModuleHeaderTooLong ->
      toSnippet "HEADER TOO LONG" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. This section header is too long:"
        , D.stack
            [ D.fillSep
                ["I","need","it","to","be"
                ,D.green "under",D.green "20",D.green "bytes"
                ,"so","it","renders","nicely","on","the","package","website!"
                ]
            , D.toSimpleNote
                "I count the length in bytes, so using non-ASCII characters costs extra.\
                \ Please report your case at https://github.com/elm/compiler/issues if this seems\
                \ overly restrictive for your needs."
            ]
        )

    OP_BadDependencyName row col ->
      toSnippet "PROBLEM WITH DEPENDENCY NAME" (toHighlight row col)
        ( D.reflow $
            "I got stuck while reading your elm.json file. There is something wrong with this dependency name:"
        , D.stack
            [ D.fillSep
                ["Package","names","always","include","the","name","of","the","author,"
                ,"so","I","am","expecting","to","see","dependencies","like"
                ,D.dullyellow "\"mdgriffith/elm-ui\"","and"
                ,D.dullyellow "\"Microsoft/elm-json-tree-view\"" <> "."
                ]
            , D.fillSep $
                ["I","generally","recommend","finding","the","package","you","want","on"
                ,"the","package","website,","and","installing","it","with","the"
                ,D.green "elm install","command!"
                ]
            ]
        )

    OP_BadLicense _ suggestions ->
      toSnippet "UNKNOWN LICENSE" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. I do not know about this type of license:"
        ,
          D.stack
            [ D.fillSep
                ["Elm","packages","generally","use"
                ,D.green "\"BSD-3-Clause\"","or",D.green "\"MIT\"" <> ","
                ,"but","I","accept","any","OSI","approved","SPDX","license."
                ,"Here","some","that","seem","close","to","what","you","wrote:"
                ]
            , D.indent 4 $ D.dullyellow $ D.vcat $ map (D.fromChars . Json.toChars) suggestions
            , D.reflow $
                "Check out https://spdx.org/licenses/ for the full list of options."
            ]
        )

    OP_BadSummaryTooLong ->
      toSnippet "SUMMARY TOO LONG" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. Your \"summary\" is too long:"
        , D.stack
            [ D.fillSep
                ["I","need","it","to","be"
                ,D.green "under",D.green "80",D.green "bytes"
                ,"so","it","renders","nicely","on","the","package","website!"
                ]
            , D.toSimpleNote
                "I count the length in bytes, so using non-ASCII characters costs extra.\
                \ Please report your case at https://github.com/elm/compiler/issues if this seems\
                \ overly restrictive for your needs."
            ]
        )

    OP_NoSrcDirs ->
      toSnippet "NO SOURCE DIRECTORIES" Nothing
        ( D.reflow $
            "I got stuck while reading your elm.json file. You do not have any \"source-directories\" listed here:"
        , D.fillSep
            ["I","need","something","like",D.green "[\"src\"]"
            ,"so","I","know","where","to","look","for","your","modules!"
            ]
        )



-- DETAILS


data Details
  = DetailsNoSolution
  | DetailsNoOfflineSolution
  | DetailsSolverProblem Solver
  | DetailsBadElmInPkg C.Constraint
  | DetailsBadElmInAppOutline V.Version
  | DetailsHandEditedDependencies
  | DetailsBadOutline Outline
  | DetailsCannotGetRegistry RegistryProblem
  | DetailsBadDeps FilePath [DetailsBadDep]


data DetailsBadDep
  = BD_BadDownload Pkg.Name V.Version PackageProblem
  | BD_BadBuild Pkg.Name V.Version (Map.Map Pkg.Name V.Version)


toDetailsReport :: Details -> Help.Report
toDetailsReport details =
  case details of
    DetailsNoSolution ->
      Help.report "INCOMPATIBLE DEPENDENCIES" (Just "elm.json")
        "The dependencies in your elm.json are not compatible."
        [ D.fillSep
            ["Did","you","change","them","by","hand?","Try","to","change","it","back!"
            ,"It","is","much","more","reliable","to","add","dependencies","with",D.green "elm install" <> "."
            ]
        , D.reflow $
            "Please ask for help on the community forums if you try those paths and are still\
            \ having problems!"
        ]

    DetailsNoOfflineSolution ->
      Help.report "TROUBLE VERIFYING DEPENDENCIES" (Just "elm.json")
        "I could not connect to https://package.elm-lang.org to get the latest list of\
        \ packages, and I was unable to verify your dependencies with the information I\
        \ have cached locally."
        [ D.reflow $
            "Are you able to connect to the internet? These dependencies may work once you\
            \ get access to the registry!"
        , D.toFancyNote
            ["If","you","changed","your","dependencies","by","hand,","try","to","change","them","back!"
            ,"It","is","much","more","reliable","to","add","dependencies","with",D.green "elm install" <> "."
            ]
        ]

    DetailsSolverProblem solver ->
      toSolverReport solver

    DetailsBadElmInPkg constraint ->
      Help.report "ELM VERSION MISMATCH" (Just "elm.json")
        "Your elm.json says this package needs a version of Elm in this range:"
        [ D.indent 4 $ D.dullyellow $ D.fromChars $ C.toChars constraint
        , D.fillSep
            [ "But", "you", "are", "using", "Elm"
            , D.red (D.fromVersion V.compiler)
            , "right", "now."
            ]
        ]

    DetailsBadElmInAppOutline version ->
      Help.report "ELM VERSION MISMATCH" (Just "elm.json")
        "Your elm.json says this application needs a different version of Elm."
        [ D.fillSep
            [ "It", "requires"
            , D.green (D.fromVersion version) <> ","
            , "but", "you", "are", "using"
            , D.red (D.fromVersion V.compiler)
            , "right", "now."
            ]
        ]

    DetailsHandEditedDependencies ->
      Help.report "ERROR IN DEPENDENCIES" (Just "elm.json")
        "It looks like the dependencies elm.json in were edited by hand (or by a 3rd\
        \ party tool) leaving them in an invalid state."
        [ D.fillSep
            ["Try","to","change","them","back","to","what","they","were","before!"
            ,"It","is","much","more","reliable","to","add","dependencies","with",D.green "elm install" <> "."
            ]
        , D.reflow $
            "Please ask for help on the community forums if you try those paths and are still\
            \ having problems!"
        ]

    DetailsBadOutline outline ->
      toOutlineReport outline

    DetailsCannotGetRegistry problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem $
        "I need the list of published packages to verify your dependencies"

    DetailsBadDeps cacheDir deps ->
      case List.sortOn toBadDepRank deps of
        [] ->
          Help.report "PROBLEM BUILDING DEPENDENCIES" Nothing
            "I am not sure what is going wrong though."
            [ D.reflow $
                "I would try deleting the " ++ cacheDir ++ " and elm-stuff/ directories, then\
                \ trying to build again. That will work if some cached files got corrupted\
                \ somehow."
            , D.reflow $
                "If that does not work, go to https://elm-lang.org/community and ask for\
                \ help. This is a weird case!"
            ]

        d:_ ->
          case d of
            BD_BadDownload pkg vsn packageProblem ->
              toPackageProblemReport pkg vsn packageProblem

            BD_BadBuild pkg vsn fingerprint ->
              Help.report "PROBLEM BUILDING DEPENDENCIES" Nothing
                "I ran into a compilation error when trying to build the following package:"
                [ D.indent 4 $ D.red $ D.fromChars $ Pkg.toChars pkg ++ " " ++ V.toChars vsn
                , D.reflow $
                    "This probably means it has package constraints that are too wide. It may be\
                    \ possible to tweak your elm.json to avoid the root problem as a stopgap. Head\
                    \ over to https://elm-lang.org/community to get help figuring out how to take\
                    \ this path!"
                , D.toSimpleNote $
                    "To help with the root problem, please report this to the package author along\
                    \ with the following information:"
                , D.indent 4 $ D.vcat $
                    map (\(p,v) -> D.fromChars $ Pkg.toChars p ++ " " ++ V.toChars v) $
                      Map.toList fingerprint
                , D.reflow $
                    "If you want to help out even more, try building the package locally. That should\
                    \ give you much more specific information about why this package is failing to\
                    \ build, which will in turn make it easier for the package author to fix it!"
                ]


toBadDepRank :: DetailsBadDep -> Int -- lower is better
toBadDepRank badDep =
  case badDep of
    BD_BadDownload _ _ _ -> 0
    BD_BadBuild _ _ _ -> 1



-- PACKAGE PROBLEM


data PackageProblem
  = PP_BadEndpointRequest Http.Error
  | PP_BadEndpointContent String
  | PP_BadArchiveRequest Http.Error
  | PP_BadArchiveContent String
  | PP_BadArchiveHash String String String


toPackageProblemReport :: Pkg.Name -> V.Version -> PackageProblem -> Help.Report
toPackageProblemReport pkg vsn problem =
  let
    thePackage =
      Pkg.toChars pkg ++ " " ++ V.toChars vsn
  in
  case problem of
    PP_BadEndpointRequest httpError ->
      toHttpErrorReport "PROBLEM DOWNLOADING PACKAGE" httpError $
        "I need to find the latest download link for " ++ thePackage

    PP_BadEndpointContent url ->
      Help.report "PROBLEM DOWNLOADING PACKAGE" Nothing
        (
          "I need to find the latest download link for " ++ thePackage ++ ", but I ran into corrupted information from:"
        )
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "Is something weird with your internet connection. We have gotten reports that\
            \ schools, businesses, airports, etc. sometimes intercept requests and add things\
            \ to the body or change its contents entirely. Could that be the problem?"
        ]

    PP_BadArchiveRequest httpError ->
      toHttpErrorReport "PROBLEM DOWNLOADING PACKAGE" httpError $
        "I was trying to download the source code for " ++ thePackage

    PP_BadArchiveContent url ->
      Help.report "PROBLEM DOWNLOADING PACKAGE" Nothing
        (
          "I downloaded the source code for " ++ thePackage ++ " from:"
        )
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
           "But I was unable to unzip the data. Maybe there is something weird with\
            \ your internet connection. We have gotten reports that schools, businesses,\
            \ airports, etc. sometimes intercept requests and add things to the body or\
            \ change its contents entirely. Could that be the problem?"
        ]

    PP_BadArchiveHash url expectedHash actualHash ->
      Help.report "CORRUPT PACKAGE DATA" Nothing
        (
          "I downloaded the source code for " ++ thePackage ++ " from:"
        )
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow "But it looks like the hash of the archive has changed since publication:"
        , D.vcat $ map D.fromChars $
            [ "  Expected: " ++ expectedHash
            , "    Actual: " ++ actualHash
            ]
        , D.reflow $
            "This usually means that the package author moved the version\
            \ tag, so report it to them and see if that is the issue. Folks\
            \ on Elm slack can probably help as well."
        ]



-- REGISTRY PROBLEM


data RegistryProblem
  = RP_Http Http.Error
  | RP_Data String BS.ByteString


toRegistryProblemReport :: String -> RegistryProblem -> String -> Help.Report
toRegistryProblemReport title problem context =
  case problem of
    RP_Http err ->
      toHttpErrorReport title err context

    RP_Data url body ->
      Help.report title Nothing (context ++ ", so I fetched:")
        [ D.indent 4 $ D.dullyellow $ D.fromChars url
        , D.reflow $
            "I got the data back, but it was not what I was expecting. The response\
            \ body contains " ++ show (BS.length body) ++ " bytes. Here is the "
            ++ if BS.length body <= 76 then "whole thing:" else "beginning:"
        , D.indent 4 $ D.dullyellow $ D.fromChars $
            if BS.length body <= 76
            then BS_UTF8.toString body
            else take 73 (BS_UTF8.toString body) ++ "..."
        , D.reflow $
            "Does this error keep showing up? Maybe there is something weird with your\
            \ internet connection. We have gotten reports that schools, businesses,\
            \ airports, etc. sometimes intercept requests and add things to the body\
            \ or change its contents entirely. Could that be the problem?"
        ]


toHttpErrorReport :: String -> Http.Error -> String -> Help.Report
toHttpErrorReport title err context =
  let
    toHttpReport intro url details =
      Help.report title Nothing intro $
        D.indent 4 (D.dullyellow (D.fromChars url)) : details
  in
  case err of
    Http.BadUrl url reason ->
      toHttpReport (context ++ ", so I wanted to fetch:") url
        [ D.reflow $ "But my HTTP library is saying this is not a valid URL. It is saying:"
        , D.indent 4 $ D.fromChars reason
        , D.reflow $
            "This may indicate that there is some problem in the compiler, so please open an\
            \ issue at https://github.com/elm/compiler/issues listing your operating system, Elm\
            \ version, the command you ran, the terminal output, and any additional information\
            \ that might help others reproduce the error."
        ]

    Http.BadHttp url httpExceptionContent ->
      case httpExceptionContent of
        HTTP.StatusCodeException response body ->
          let
            (HTTP.Status code message) = HTTP.responseStatus response
          in
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.fillSep $
                ["But","it","came","back","as",D.red (D.fromInt code)]
                ++ map D.fromChars (words (BS_UTF8.toString message))
            , D.indent 4 $ D.reflow $ BS_UTF8.toString body
            , D.reflow $
                "This may mean some online endpoint changed in an unexpected way, so if does not\
                \ seem like something on your side is causing this (e.g. firewall) please report\
                \ this to https://github.com/elm/compiler/issues with your operating system, Elm\
                \ version, the command you ran, the terminal output, and any additional information\
                \ that can help others reproduce the error!"
            ]

        HTTP.TooManyRedirects responses ->
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.reflow $ "But I gave up after following these " ++ show (length responses) ++ " redirects:"
            , D.indent 4 $ D.vcat $ map toRedirectDoc responses
            , D.reflow $
                "Is it possible that your internet connection intercepts certain requests? That\
                \ sometimes causes problems for folks in schools, businesses, airports, hotels,\
                \ and certain countries. Try asking for help locally or in a community forum!"
            ]

        otherException ->
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.reflow $ "But my HTTP library is giving me the following error message:"
            , D.indent 4 $ D.fromChars (show otherException)
            , D.reflow $
                "Are you somewhere with a slow internet connection? Or no internet?\
                \ Does the link I am trying to fetch work in your browser? Maybe the\
                \ site is down? Does your internet connection have a firewall that\
                \ blocks certain domains? It is usually something like that!"
            ]

    Http.BadMystery url someException ->
      toHttpReport (context ++ ", so I tried to fetch:") url
        [ D.reflow $ "But I ran into something weird! I was able to extract this error message:"
        , D.indent 4 $ D.fromChars (show someException)
        , D.reflow $
            "Is it possible that your internet connection intercepts certain requests? That\
            \ sometimes causes problems for folks in schools, businesses, airports, hotels,\
            \ and certain countries. Try asking for help locally or in a community forum!"
        ]


toRedirectDoc :: HTTP.Response body -> D.Doc
toRedirectDoc response =
  let
    (HTTP.Status code message) = HTTP.responseStatus response
  in
  case List.lookup HTTP.hLocation (HTTP.responseHeaders response) of
    Just loc -> D.red (D.fromInt code) <> " - " <> D.fromChars (BS_UTF8.toString loc)
    Nothing  -> D.red (D.fromInt code) <> " - " <> D.fromChars (BS_UTF8.toString message)



-- MAKE


data Make
  = MakeNoOutline
  | MakeCannotOptimizeAndDebug
  | MakeBadDetails Details
  | MakeAppNeedsFileNames
  | MakePkgNeedsExposing
  | MakeMultipleFilesIntoHtml
  | MakeNoMain
  | MakeNonMainFilesIntoBend ModuleName.Raw [ModuleName.Raw]
  | MakeCannotBuild BuildProblem
  | MakeBadGenerate Generate


makeToReport :: Make -> Help.Report
makeToReport make =
  case make of
    MakeNoOutline ->
      Help.report "NO elm.json FILE" Nothing
        "It looks like you are starting a new Elm project. Very exciting! Try running:"
        [ D.indent 4 $ D.green $ "elm init"
        , D.reflow $
            "It will help you get set up. It is really simple!"
        ]

    MakeCannotOptimizeAndDebug ->
      Help.docReport "CLASHING FLAGS" Nothing
        ( D.fillSep
            ["I","cannot","compile","with",D.red "--optimize","and"
            ,D.red "--debug","at","the","same","time."
            ]
        )
        [ D.reflow
            "I need to take away information to optimize things, and I need to\
            \ add information to add the debugger. It is impossible to do both\
            \ at once though! Pick just one of those flags and it should work!"
        ]

    MakeBadDetails detailsProblem ->
      toDetailsReport detailsProblem

    MakeAppNeedsFileNames ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need specific files like:"
        [ D.vcat
            [ D.indent 4 $ D.green "elm make src/Main.elm"
            , D.indent 4 $ D.green "elm make src/This.elm src/That.elm"
            ]
        , D.reflow $
            "I recommend reading through https://guide.elm-lang.org for guidance on what to\
            \ actually put in those files!"
        ]

    MakePkgNeedsExposing ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need specific files like:"
        [ D.vcat
            [ D.indent 4 $ D.green "elm make src/Main.elm"
            , D.indent 4 $ D.green "elm make src/This.elm src/That.elm"
            ]
        , D.reflow $
            "You can also entries to the \"exposed-modules\" list in your elm.json file, and\
            \ I will try to compile the relevant files."
        ]

    MakeMultipleFilesIntoHtml ->
      Help.report "TOO MANY FILES" Nothing
        (
          "When producing an HTML file, I can only handle one file."
        )
        [ D.fillSep
            ["Switch","to",D.dullyellow "--output=/dev/null","if","you","just","want"
            ,"to","get","compile","errors.","This","skips","the","code","gen","phase,"
            ,"so","it","can","be","a","bit","faster","than","other","options","sometimes."
            ]
        , D.fillSep
            ["Switch","to",D.dullyellow "--output=elm.js","if","you","want","multiple"
            ,"`main`","values","available","in","a","single","JavaScript","file.","Then"
            ,"you","can","make","your","own","customized","HTML","file","that","embeds"
            ,"multiple","Elm","nodes.","The","generated","JavaScript","also","shares"
            ,"dependencies","between","modules,","so","it","should","be","smaller","than"
            ,"compiling","each","module","separately."
            ]
        ]

    MakeNoMain ->
      Help.report "NO MAIN" Nothing
        (
          "When producing an HTML file, I require that the given file has a `main` value.\
          \ That way I have something to show on screen!"
        )
        [ D.reflow $
            "Try adding a `main` value to your file? Or if you just want to verify that this\
            \ module compiles, switch to --output=/dev/null to skip the code gen phase\
            \ altogether."
        , D.toSimpleNote $
            "Adding a `main` value can be as brief as adding something like this:"
        , D.vcat
            [ D.fillSep [D.cyan "import","Html"]
            , ""
            , D.fillSep [D.green "main","="]
            , D.indent 2 $ D.fillSep [D.cyan "Html" <> ".text",D.dullyellow "\"Hello!\""]
            ]
        , D.reflow $
            "From there I can create an HTML file that says \"Hello!\" on screen. I recommend\
            \ looking through https://guide.elm-lang.org for more guidance on how to fill in\
            \ the `main` value."
        ]

    MakeNonMainFilesIntoBend m ms ->
      case ms of
        [] ->
          Help.report "NO MAIN" Nothing
            "When producing a Bend file, I require that the given file has a `main` value."
            [ D.reflow $
                "Try adding a `main` value to your file? Or if you just want to verify that this\
                \ module compiles, switch to --output=/dev/null to skip the code gen phase\
                \ altogether."
            ]

        _:_ ->
          -- TODO this warning doesn't make sense for Bend.
          Help.report "NO MAIN" Nothing
            (
              "When producing a Bend file, I require that given files all have `main` values.\
              \ That way functions like Elm." ++ ModuleName.toChars m ++ ".init() are\
              \ definitely defined in the resulting file. I am missing `main` values in:"
            )
            [ D.indent 4 $ D.red $ D.vcat $ map D.fromName (m:ms)
            , D.reflow $
                "Try adding a `main` value to them? Or if you just want to verify that these\
                \ modules compile, switch to --output=/dev/null to skip the code gen phase\
                \ altogether."
            , D.toSimpleNote $
                "Adding a `main` value can be as brief as adding something like this:"
            , D.vcat
                [ D.fillSep [D.cyan "import","Html"]
                , ""
                , D.fillSep [D.green "main","="]
                , D.indent 2 $ D.fillSep [D.cyan "Html" <> ".text",D.dullyellow "\"Hello!\""]
                ]
            , D.reflow $
                "Or use https://package.elm-lang.org/packages/elm/core/latest/Platform#worker to\
                \ make a `main` with no user interface."
            ]

    MakeCannotBuild buildProblem ->
      toBuildProblemReport buildProblem

    MakeBadGenerate generateProblem ->
      toGenerateReport generateProblem



-- BUILD PROBLEM


data BuildProblem
  = BuildBadModules FilePath Error.Module [Error.Module]
  | BuildProjectProblem BuildProjectProblem


data BuildProjectProblem
  = BP_PathUnknown FilePath
  | BP_WithBadExtension FilePath
  | BP_WithAmbiguousSrcDir FilePath FilePath FilePath
  | BP_MainPathDuplicate FilePath FilePath
  | BP_RootNameDuplicate ModuleName.Raw FilePath FilePath
  | BP_RootNameInvalid FilePath FilePath [String]
  | BP_CannotLoadDependencies
  | BP_Cycle ModuleName.Raw [ModuleName.Raw]
  | BP_MissingExposed (NE.List (ModuleName.Raw, Import.Problem))


toBuildProblemReport :: BuildProblem -> Help.Report
toBuildProblemReport problem =
  case problem of
    BuildBadModules root e es ->
      Help.compilerReport root e es

    BuildProjectProblem projectProblem ->
      toProjectProblemReport projectProblem


toProjectProblemReport :: BuildProjectProblem -> Help.Report
toProjectProblemReport projectProblem =
  case projectProblem of
    BP_PathUnknown path ->
      Help.report "FILE NOT FOUND" Nothing
        "I cannot find this file:"
        [ D.indent 4 $ D.red $ D.fromChars path
        , D.reflow $ "Is there a typo?"
        , D.toSimpleNote $
            "If you are just getting started, try working through the examples in the\
            \ official guide https://guide.elm-lang.org to get an idea of the kinds of things\
            \ that typically go in a src/Main.elm file."
        ]

    BP_WithBadExtension path ->
      Help.report "UNEXPECTED FILE EXTENSION" Nothing
        "I can only compile Elm files (with a .elm extension) but you want me to compile:"
        [ D.indent 4 $ D.red $ D.fromChars path
        , D.reflow $ "Is there a typo? Can the file extension be changed?"
        ]

    BP_WithAmbiguousSrcDir path srcDir1 srcDir2 ->
      Help.report "CONFUSING FILE" Nothing
        "I am getting confused when I try to compile this file:"
        [ D.indent 4 $ D.red $ D.fromChars path
        , D.reflow $
            "I always check if files appear in any of the \"source-directories\" listed in\
            \ your elm.json to see if there might be some cached information about them. That\
            \ can help me compile faster! But in this case, it looks like this file may be in\
            \ either of these directories:"
        , D.indent 4 $ D.red $ D.vcat $ map D.fromChars [srcDir1,srcDir2]
        , D.reflow $
            "Try to make it so no source directory contains another source directory!"
        ]

    BP_MainPathDuplicate path1 path2 ->
      Help.report "CONFUSING FILES" Nothing
        "You are telling me to compile these two files:"
        [ D.indent 4 $ D.red $ D.vcat $ map D.fromChars [ path1, path2 ]
        , D.reflow $
            if path1 == path2 then
              "Why are you telling me twice? Is something weird going on with a script?\
              \ I figured I would let you know about it just in case something is wrong.\
              \ Only list it once and you should be all set!"
            else
              "But seem to be the same file though... It makes me think something tricky is\
              \ going on with symlinks in your project, so I figured I would let you know\
              \ about it just in case. Remove one of these files from your command to get\
              \ unstuck!"
        ]

    BP_RootNameDuplicate name outsidePath otherPath ->
      Help.report "MODULE NAME CLASH" Nothing
        "These two files are causing a module name clash:"
        [ D.indent 4 $ D.red $ D.vcat $ map D.fromChars [ outsidePath, otherPath ]
        , D.reflow $
            "They both say `module " ++ ModuleName.toChars name ++ " exposing (..)` up\
            \ at the top, but they cannot have the same name!"
        , D.reflow $
            "Try changing to a different module name in one of them!"
        ]

    BP_RootNameInvalid givenPath srcDir _ ->
      Help.report "UNEXPECTED FILE NAME" Nothing
        "I am having trouble with this file name:"
        [ D.indent 4 $ D.red $ D.fromChars givenPath
        , D.reflow $
            "I found it in your " ++ FP.addTrailingPathSeparator srcDir ++ " directory\
            \ which is good, but I expect all of the files in there to use the following\
            \ module naming convention:"
        , toModuleNameConventionTable srcDir [ "Main", "HomePage", "Http.Helpers" ]
        , D.reflow $
            "Notice that the names always start with capital letters! Can you make your file\
            \ use this naming convention?"
        , D.toSimpleNote $
            "Having a strict naming convention like this makes it a lot easier to find\
            \ things in large projects. If you see a module imported, you know where to look\
            \ for the corresponding file every time!"
        ]

    BP_CannotLoadDependencies ->
      corruptCacheReport

    BP_Cycle name names ->
      Help.report "IMPORT CYCLE" Nothing
        "Your module imports form a cycle:"
        [ D.cycle 4 name names
        , D.reflow $
            "Learn more about why this is disallowed and how to break cycles here:"
            ++ D.makeLink "import-cycles"
        ]

    BP_MissingExposed (NE.List (name, problem) _) ->
      case problem of
        Import.NotFound ->
          Help.report "MISSING MODULE" (Just "elm.json")
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 $ D.red $ D.fromName name
            , D.reflow $
                "But I cannot find it in your src/ directory. Is there a typo? Was it renamed?"
            ]

        Import.Ambiguous _ _ pkg _ ->
          Help.report "AMBIGUOUS MODULE NAME" (Just "elm.json")
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 $ D.red $ D.fromName name
            , D.reflow $
                "But a module from " ++ Pkg.toChars pkg ++ " already uses that name. Try\
                \ choosing a different name for your local file."
            ]

        Import.AmbiguousLocal path1 path2 paths ->
          Help.report "AMBIGUOUS MODULE NAME" (Just "elm.json")
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 $ D.red $ D.fromName name
            , D.reflow $
                "But I found multiple files with that name:"
            , D.dullyellow $ D.indent 4 $ D.vcat $
                map D.fromChars (path1:path2:paths)
            , D.reflow $
                "Change the module names to be distinct!"
            ]

        Import.AmbiguousForeign _ _ _ ->
          Help.report "MISSING MODULE" (Just "elm.json")
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 $ D.red $ D.fromName name
            , D.reflow $
                "But I cannot find it in your src/ directory. Is there a typo? Was it renamed?"
            , D.toSimpleNote $
                "It is not possible to \"re-export\" modules from other packages. You can only\
                \ expose modules that you define in your own code."
            ]


toModuleNameConventionTable :: FilePath -> [String] -> D.Doc
toModuleNameConventionTable srcDir names =
  let
    toPair name =
      ( name
      , srcDir </> map (\c -> if c == '.' then FP.pathSeparator else c) name <.> "elm"
      )

    namePairs = map toPair names
    nameWidth = maximum (11 : map (length . fst) namePairs)
    pathWidth = maximum ( 9 : map (length . snd) namePairs)

    padded width str =
      str ++ replicate (width - length str) ' '

    toRow (name, path) =
      D.fromChars $
        "| " ++ padded nameWidth name ++ " | " ++ padded pathWidth path ++ " |"

    bar =
      D.fromChars $
        "+-" ++ replicate nameWidth '-' ++ "-+-" ++ replicate pathWidth '-' ++ "-+"
  in
  D.indent 4 $ D.vcat $
    [ bar, toRow ("Module Name", "File Path"), bar ] ++ map toRow namePairs ++ [ bar ]



-- GENERATE


data Generate
  = GenerateCannotLoadArtifacts
  | GenerateCannotOptimizeDebugValues ModuleName.Raw [ModuleName.Raw]


toGenerateReport :: Generate -> Help.Report
toGenerateReport problem =
  case problem of
    GenerateCannotLoadArtifacts ->
      corruptCacheReport

    GenerateCannotOptimizeDebugValues m ms ->
      Help.report "DEBUG REMNANTS" Nothing
        "There are uses of the `Debug` module in the following modules:"
        [ D.indent 4 $ D.red $ D.vcat $ map (D.fromChars . ModuleName.toChars) (m:ms)
        , D.reflow "But the --optimize flag only works if all `Debug` functions are removed!"
        , D.toSimpleNote $
            "The issue is that --optimize strips out info needed by `Debug` functions.\
            \ Here are two examples:"
        , D.indent 4 $ D.reflow $
            "(1) It shortens record field names. This makes the generated JavaScript\
            \ smaller, but `Debug.toString` cannot know the real field names anymore."
        , D.indent 4 $ D.reflow $
            "(2) Values like `type Height = Height Float` are unboxed. This reduces\
            \ allocation, but it also means that `Debug.toString` cannot tell if it is\
            \ looking at a `Height` or `Float` value."
        , D.reflow $
            "There are a few other cases like that, and it will be much worse once we start\
            \ inlining code. That optimization could move `Debug.log` and `Debug.todo` calls,\
            \ resulting in unpredictable behavior. I hope that clarifies why this restriction\
            \ exists!"
        ]



-- CORRUPT CACHE


corruptCacheReport :: Help.Report
corruptCacheReport =
  Help.report "CORRUPT CACHE" Nothing
    "It looks like some of the information cached in elm-bend-stuff/ has been corrupted."
    [ D.reflow $
        "Try deleting your elm-bend-stuff/ directory to get unstuck."
    , D.toSimpleNote $
        "This almost certainly means that a 3rd party tool (or editor plugin) is\
        \ causing problems your the elm-bend-stuff/ directory. Try disabling 3rd party tools\
        \ one by one until you figure out which it is!"
    ]
