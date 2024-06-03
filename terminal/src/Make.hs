{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Make
  ( Flags(..)
  , Output(..)
  , ReportType(..)
  , run
  , reportType
  , output
  , docsFile
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Generate
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import Terminal (Parser(..))



-- FLAGS


data Flags =
  Flags
    { _output :: Maybe Output
    , _report :: Maybe ReportType
    }


data Output
  = Bend FilePath
  | DevNull


data ReportType
  = Json



-- RUN


type Task a = Task.Task Exit.Make a


run :: [FilePath] -> Flags -> IO ()
run paths flags@(Flags _ report) =
  do  style <- getStyle report
      maybeRoot <- Stuff.findRoot
      Reporting.attemptWithStyle style Exit.makeToReport $
        case maybeRoot of
          Just root -> runHelp root paths style flags
          Nothing   -> return $ Left $ Exit.MakeNoOutline


runHelp :: FilePath -> [FilePath] -> Reporting.Style -> Flags -> IO (Either Exit.Make ())
runHelp root paths style (Flags maybeOutput _) =
  BW.withScope $ \scope ->
  Stuff.withRootLock root $ Task.run $
  do  details <- Task.eio Exit.MakeBadDetails (Details.load style scope root)
      case paths of
        [] ->
          do  exposed <- getExposed details
              buildExposed style root details exposed

        p:ps ->
          do  artifacts <- buildPaths style root details (NE.List p ps)
              case maybeOutput of
                Nothing ->
                  case getMains artifacts of
                    [] ->
                      return ()

                    [name] ->
                      do  builder <- toBuilder root details artifacts
                          generate style "out.bend" builder (NE.List name [])

                    name:names ->
                      do  builder <- toBuilder root details artifacts
                          generate style "out.bend" builder (NE.List name names)

                Just DevNull ->
                  return ()

                Just (Bend target) ->
                  case getNoMains artifacts of
                    [] ->
                      do  builder <- toBuilder root details artifacts
                          generate style target builder (Build.getRootNames artifacts)

                    name:names ->
                      Task.throw (Exit.MakeNonMainFilesIntoBend name names)



-- GET INFORMATION


getStyle :: Maybe ReportType -> IO Reporting.Style
getStyle report =
  case report of
    Nothing -> Reporting.terminal
    Just Json -> return Reporting.json


getExposed :: Details.Details -> Task (NE.List ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ ->
      Task.throw Exit.MakeAppNeedsFileNames

    Details.ValidPkg _ exposed _ ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m:ms -> return (NE.List m ms)



-- BUILD PROJECTS


buildExposed :: Reporting.Style -> FilePath -> Details.Details -> NE.List ModuleName.Raw -> Task ()
buildExposed style root details exposed =
  Task.eio Exit.MakeCannotBuild $
    Build.fromExposed style root details exposed


buildPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths style root details paths



-- GET MAINS


getMains :: Build.Artifacts -> [ModuleName.Raw]
getMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getMain modules) (NE.toList roots)


getMain :: [Build.Module] -> Build.Root -> Maybe ModuleName.Raw
getMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
      then Just name
      else Nothing

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _ _) ->
      case maybeMain of
        Just _  -> Just name
        Nothing -> Nothing


isMain :: ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _ _) ->
      Maybe.isJust maybeMain && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- GET MAINLESS


getNoMains :: Build.Artifacts -> [ModuleName.Raw]
getNoMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getNoMain modules) (NE.toList roots)


getNoMain :: [Build.Module] -> Build.Root -> Maybe ModuleName.Raw
getNoMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
      then Nothing
      else Just name

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _ _) ->
      case maybeMain of
        Just _  -> Nothing
        Nothing -> Just name



-- GENERATE


generate :: Reporting.Style -> FilePath -> B.Builder -> NE.List ModuleName.Raw -> Task ()
generate style target builder names =
  Task.io $
    do  Dir.createDirectoryIfMissing True (FP.takeDirectory target)
        File.writeBuilder target builder
        Reporting.reportGenerate style names target



-- TO BUILDER


toBuilder :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
toBuilder root details artifacts =
  Task.mapError Exit.MakeBadGenerate $
    Generate.debug root details artifacts



-- PARSERS


reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type"
    , _plural = "report types"
    , _parser = \string -> if string == "json" then Just Json else Nothing
    , _suggest = \_ -> return ["json"]
    , _examples = \_ -> return ["json"]
    }


output :: Parser Output
output =
  Parser
    { _singular = "output file"
    , _plural = "output files"
    , _parser = parseOutput
    , _suggest = \_ -> return []
    , _examples = \_ -> return [ "elm.js", "index.html", "/dev/null" ]
    }


parseOutput :: String -> Maybe Output
parseOutput name
  | isDevNull name      = Just DevNull
  | hasExt ".bend" name = Just (Bend name)
  | otherwise           = Nothing


docsFile :: Parser FilePath
docsFile =
  Parser
    { _singular = "json file"
    , _plural = "json files"
    , _parser = \name -> if hasExt ".json" name then Just name else Nothing
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json","documentation.json"]
    }


hasExt :: String -> String -> Bool
hasExt ext path =
  FP.takeExtension path == ext && length path > length ext


isDevNull :: String -> Bool
isDevNull name =
  name == "/dev/null" || name == "NUL" || name == "$null"
