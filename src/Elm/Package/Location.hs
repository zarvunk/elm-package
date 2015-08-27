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
        | Remote Url
        deriving ( Eq )


instance ToJSON Location where
    toJSON location =
        case location of
            Catalog  -> String "catalog"
            GitHub   -> String "github"
            Local p  -> object
                    [ "local-path" .= p ]
            Remote u -> object
                    [ "remote-url" .= u ]


instance FromJSON Location where
    parseJSON (Object o) =
        (Local <$> o .: "local-path")
        <|> (Remote <$> o .: "remote-url")
        <|> (fail "expecting a key named either `local-path' or `remote-url'")

    parseJSON (String s)
        | s == "catalog"   = return Catalog
        | s == "github"    = return GitHub
        | otherwise        = fail "expecting a string consisting of either `catalog' or `github'"

    parseJSON _ =
        fail "location must be either a string or an object"
