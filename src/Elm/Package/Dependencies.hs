{-# LANGUAGE OverloadedStrings #-}
module Elm.Package.Dependencies where

import Data.Aeson
import qualified Data.List as List
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Control.Applicative ( (<$>) )
import Control.Arrow ( (***) )
import Control.Monad ( forM )

import qualified Elm.Package.Name as N
import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Location as L


{- These newtypes are purely for the sake of the custom ToJSON and
FromJSON instances. The same code for these instances previously
existed in a slightly different form in Elm.Package.Description, but
typeclass instances seem like the more suitable place for it.
-}

newtype NormalDependencies = Normal
            { normal :: [(N.Name, (L.Location, C.Constraint))] }


instance ToJSON NormalDependencies where
    toJSON deps =
        toJSON
            $ Map.fromList
            $ map (N.toText *** snd) 
            $ normal deps


instance FromJSON NormalDependencies where
    parseJSON (Object deps) =
       Normal <$>
           (forM (Map.toList deps) $ \(rawName, jsonConstraint) ->
                do  name <- parseJSON $ String rawName
                    constraint <- parseJSON jsonConstraint
                    return (name, (L.Catalog, constraint))
           )

    parseJSON _ =
        fail "the dependency list must be a JSON object"


newtype DevDependencies = Dev
            { dev :: [(N.Name, (L.Location, C.Constraint))] }


instance ToJSON DevDependencies where
    toJSON deps =
        object
            $ map organize
            $ dev deps

        where organize (name, (location, constraint)) =
                N.toText name .= object
                    [ "location" .= location
                    , "version" .= constraint
                    ]


instance FromJSON DevDependencies where
    parseJSON (Object deps) =
        Dev <$>
            (forM (Map.toList deps) $
                \(rawName, info) ->
                    withObject
                        "each dev-dependency must be a object"
                        (parseDependency rawName)
                        info
            )

        where parseDependency rawName info =
                do  name <- parseJSON $ String rawName
                    constraint <- info .: "version"
                    location <- info .: "location"
                    return (name, (location, constraint))


sortDependencies :: [(N.Name, (L.Location, C.Constraint))] -> (NormalDependencies, DevDependencies)
sortDependencies deps =
    (Normal *** Dev) $ List.partition isInCatalog deps
    where
        isInCatalog (name, (location, constraint)) =
            case location of
                L.Catalog -> True
                _ -> False
