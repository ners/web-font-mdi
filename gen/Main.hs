{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import GHC.Char (chr)
import GHC.Generics (Generic)
import GHC.SourceGen
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, renderWithContext)
import Numeric (readHex)
import Ormolu qualified
import Ormolu.Config (FourmoluConfig (..))
import Prelude

data Icon = Icon
    { name :: Text
    , codepoint :: Char
    }
    deriving stock (Generic)

instance FromJSON Icon where
    parseJSON = withObject "Icon" $ \o -> do
        nameKebabCase <- o .: "name"
        codepointStr <- o .: "codepoint"
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

fourmoluConfig :: IO FourmoluConfig
fourmoluConfig =
    either (fail . show) pure
        =<< Yaml.decodeFileEither
        =<< either (fail . show) pure
        =<< Ormolu.findConfigFile "."

ormoluConfig :: IO (Ormolu.Config Ormolu.RegionIndices)
ormoluConfig =
    fourmoluConfig <&> \FourmoluConfig{..} ->
        Ormolu.defaultConfig
            { Ormolu.cfgPrinterOpts = Ormolu.resolvePrinterOpts [cfgFilePrinterOpts]
            , Ormolu.cfgFixityOverrides = cfgFileFixities
            , Ormolu.cfgModuleReexports = cfgFileReexports
            }

getIcons :: IO [Icon]
getIcons = either fail pure . Aeson.eitherDecode =<< LazyByteString.readFile "./meta.json"

genHs :: [Icon] -> Text
genHs icons = Text.pack . renderWithContext defaultSDocContext . ppr $ module' (Just "Web.Font.MDI") Nothing imports decls
  where
    imports :: [ImportDecl']
    imports = [import' "Prelude"]
    decls :: [HsDecl']
    decls =
        [ data' "MDI" [] [prefixCon (fromText name) [] | Icon{..} <- icons] [deriving' [var "Eq", var "Bounded", var "Enum"]]
        , typeSig "mdiChar" $ var "MDI" --> var "Char"
        , funBinds "mdiChar" [match [bvar (fromText name)] (char codepoint) | Icon{..} <- icons]
        ]
    fromText :: (IsString s) => Text -> s
    fromText = fromString . Text.unpack

main :: IO ()
main = do
    config <- ormoluConfig
    Text.putStrLn =<< Ormolu.ormolu config "" . genHs =<< getIcons
