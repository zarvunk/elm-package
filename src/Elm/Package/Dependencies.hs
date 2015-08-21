module Elm.Package.Dependencies where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Control.Monad ( forM )

import qualified Elm.Package.Name as N
import qualified Elm.Package.Constraint as C


{- This newtype is purely for the sake of the custom ToJSON and
FromJSON instances. The same code for these instances previously
existed in a slightly different form in Elm.Package.Description, but
typeclass instances seem like the more suitable place for it.
-}
newtype Dependencies = Dependencies 
            { list :: [(N.Name, C.Constraint)] }


instance ToJSON Dependencies where
    toJSON deps =
        toJSON
            $ Map.fromList
            $ map (first (T.pack . N.toString))
            $ list deps


instance FromJSON Dependencies where
    parseJSON (Object deps) =
       Dependencies <$>
          do  forM (Map.toList deps) $ \(rawName, jsonConstraint) ->
                do  name <- parseJSON $ String rawName
                    constraint <- parseJSON jsonConstraint
                    return (name, constraint)

    parseJSON _ =
        fail "the dependency list must be a JSON object"
