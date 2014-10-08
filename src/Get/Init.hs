module Command.Init where

import System.IO (hFlush, stdout)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Elm.Package.Name as N
import qualified Elm.Package.Version as V
import qualified Elm.Package.Description as D
import qualified Elm.Package.Paths as Path
import qualified Data.ByteString.Lazy as BS


query :: Show a => String -> Maybe a -> (String -> Either String a) -> IO a
query prompt maybeDefault verify =
  do  putStrLn prompt

      answer <- getLine
      case verify answer of
        Right result -> return result

        Left message ->
            case maybeDefault of
                Just result -> return result
                Nothing ->
                    do  putStrLn message
                        query check prompt


repoPrompt :: String
repoPrompt =
    unlines
    [ "We need the name of your project's GitHub repo."
    , ""
    , "  Why:"
    , "    This makes it easy to keep track of different versions of your code in"
    , "    case you ever want to publish it for the community."
    , ""
    , "  Example: https://github.com/evancz/elm-html.git"
    , "  Default: https://github.com/USER/PROJECT.git"
    , ""
    ]


summaryPrompt :: String
summaryPrompt =
    unlines
    [ "Please summarize your project in under 80 characters!"
    , ""
    , "  Why:"
    , "    When people search for your project"
    ]


licensePrompt :: String
licensePrompt =
    unlines
    [ "We need to choose a license."
    , ""
    , "  Why:"
    , "    This makes it clear to other people how they can use your code. We"
    , "    recommend using BSD3 which makes it easy to use within companies."
    , ""
    , "  Example: MIT"
    , "  Default: BSD3"
    , ""
    ]



askFor :: String -> IO String
askFor req =
    askForChecked Right req


askForWithDefault :: String -> String -> IO String
askForWithDefault def req =
    askForChecked (Right . injectDefault (Just def)) req


verifySummary :: String -> Either String String
verifySummary summary =
    if length summary < 80
        then Right summary
        else Left errorMessage
    where
      errorMessage =
          concat
          [ "Your summary should be the one sen"
          , " length shouldn't exceed "
          , show limit
          , " characters!"
          ]


askUser :: IO D.Description
askUser =
  do  repo    <- query "GitHub Repo" Nothing
      summary <- query "Short Summary" Nothing verifySummary
      desc    <- query "Description" Nothing Right
      license <- query "License" (Just "BSD3") Right
      
      return $ D.Description (N.Name userName projectName)


initialize :: IO ()
initialize = do
  dependencies <- readDeps
  BS.writeFile Path.description (encodePretty dependencies)
