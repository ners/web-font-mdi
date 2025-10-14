{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Data.Aeson (FromJSON, Value (Object, String), decodeStrict, parseJSON, withObject, withText, (.:))
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Char (chr, toUpper)
import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Language.Haskell.TH
import Numeric (readHex)

data Icon = Icon
    { name :: Name
    , codepoint :: Char
    }
    deriving (Generic, Show)

instance FromJSON Name where
    parseJSON = withText "Name" $ pure . mkName . Text.unpack

instance FromJSON Icon where
    parseJSON = withObject "Icon" $ \o -> do
        nameKebabCase :: Text <- o .: "name"
        codepointStr :: String <- o .: "codepoint"
        [(codepointInt, "")] <- pure $ readHex codepointStr
        pure
            Icon
                { name = mkName . Text.unpack . mconcat . fmap capitalise $ "mdi" : Text.splitOn "-" nameKebabCase
                , codepoint = chr codepointInt
                }

capitalise :: Text -> Text
capitalise t
    | Just (x, xs) <- Text.uncons t = Text.cons (toUpper x) xs
    | otherwise = t

icons :: [Icon]
icons = fromMaybe (error "Could not parse file") $ decodeStrict @[Icon] $(embedFile "./meta.json")

mdiEnumName :: Name
mdiEnumName = mkName "MDI"

mkMdiEnum :: DecsQ
-- DataD Cxt Name [TyVarBndr ()] (Maybe Kind) [Con] [DerivClause]
mkMdiEnum =
    pure
        [ DataD
            []
            mdiEnumName
            []
            Nothing
            (con <$> icons)
            [ DerivClause
                Nothing
                [ ConT ''Eq
                , ConT ''Bounded
                , ConT ''Enum
                ]
            ]
        ]
  where
    con i = NormalC (name i) []

mkMdiChar :: DecsQ
-- FunD Name [Clause]
-- Clause [Pat] Body [Dec]
mkMdiChar =
    pure
        [ SigD fname $ ArrowT `AppT` ConT mdiEnumName `AppT` ConT ''Char
        , FunD fname (clause <$> icons)
        ]
  where
    fname = mkName "mdiChar"
    clause i = Clause [ConP (name i) [] []] (body i) []
    body = NormalB . LitE . CharL . codepoint
