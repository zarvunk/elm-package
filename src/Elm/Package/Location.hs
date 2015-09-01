{-# LANGUAGE OverloadedStrings #-}
module Elm.Package.Location where

import Data.Aeson
import Control.Applicative ( (<|>) )
import System.FilePath ( FilePath )


type Url = String


data Location
        = Catalog
        | GitHub
        | Local FilePath
        | Url Url


instance ToJSON Location where
    toJSON location =
        case location of
            Catalog -> String "catalog"
            GitHub  -> String "github"
            Local p -> object
                    [ "local-path" .= p ]
            Url u   -> object
                    [ "git-url" .= u ]


instance FromJSON Location where
    parseJSON j =
        case j of
            Object o ->
                (Local <$> o .: "local-path")
                <|> (Url <$> o .: "git-url")
                <|> (fail "expecting a key named either `local-path' or `git-url'")

            String s
                | s == "catalog"  -> return Catalog
                | s == "github"   -> return GitHub
                | otherwise       -> fail "expecting a string consisting of either `catalog' or `github'"

            _ ->
                fail "location must be either a string or an object"
