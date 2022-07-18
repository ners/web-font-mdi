{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Char (toUpper)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo
import GHC.Generics

data Icon = Icon
    { name :: Text
    , codepoint :: Text
    }
    deriving (Generic, Show)

instance FromJSON Icon

camelCase :: [Text] -> Text
camelCase [] = ""
camelCase (x : xs) = Text.toLower x <> pascalCase xs

pascalCase :: [Text] -> Text
pascalCase = Text.concat . fmap p
  where
    p = (\x -> Text.cons (toUpper $ Text.head x) $ Text.tail x) . Text.toLower

iconToHaskell :: Icon -> Text
iconToHaskell Icon{..} =
    let fname = camelCase $ "mdi" : Text.splitOn "-" name
     in Text.unlines $
            mconcat
                <$> [ [fname, " :: Char"]
                    , [fname, " = '\\x", codepoint, "'"]
                    ]

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{preBuild = mkMdi}

mkMdi :: Args -> BuildFlags -> IO HookedBuildInfo
mkMdi args bflags = do
    icons <- fromRight [] <$> eitherDecodeFileStrict @[Icon] "./meta.json"
    let hs = Text.unlines $ iconToHaskell <$> icons
    Text.appendFile "./src/Web/Font/MDI.hs" hs
    return emptyHookedBuildInfo
