{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Aeson
    ( FromJSON
    , Value (Object, String)
    , eitherDecodeStrict
    , parseJSON
    , withObject
    , withText
    , (.:)
    )
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import qualified Data.ByteString as ByteString
import Data.Char (chr, toUpper)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC (runGhc)
import GHC.Generics
import GHC.Paths (libdir)
import GHC.SourceGen
import Numeric (readHex)
import System.Environment (getArgs)

data Icon = Icon
    { name :: Text
    , codepoint :: Char
    }
    deriving (Generic, Show)

instance FromJSON Icon where
    parseJSON = withObject "Icon" $ \o -> do
        nameKebabCase :: Text <- o .: "name"
        codepointStr :: String <- o .: "codepoint"
        [(codepointInt, "")] <- pure $ readHex codepointStr
        pure
            Icon
                { name = mconcat . fmap capitalise $ "mdi" : Text.splitOn "-" nameKebabCase
                , codepoint = chr codepointInt
                }

capitalise :: Text -> Text
capitalise t
    | Just (x, xs) <- Text.uncons t = Text.cons (toUpper x) xs
    | otherwise = t

fromText :: (IsString a) => Text -> a
fromText = fromString . Text.unpack

main :: IO ()
main = do
    [iconsFile] <- getArgs
    icons <- either error id . eitherDecodeStrict @[Icon] <$> ByteString.readFile iconsFile
    runGhc (Just libdir) . putPpr . module' (Just "Web.Font.MDI") Nothing [] $
        [ data'
            "MDI"
            []
            [prefixCon (fromText name) [] | Icon{name} <- icons]
            [deriving' [var "Eq", var "Bounded", var "Enum"]]
        , typeSig "mdiChar" (var "MDI" --> var "Char")
        ]
            <> [ funBind "mdiChar" $ match [conP_ . fromText $ name] (char codepoint)
               | Icon{..} <- icons
               ]
