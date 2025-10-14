{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Data.Aeson (FromJSON, Value (Object, String), decodeStrict, parseJSON, (.:))
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
    parseJSON (String s) = pure $ mkName $ Text.unpack s
    parseJSON invalid = prependFailure "parsing name failed, " (typeMismatch "String" invalid)

instance FromJSON Icon where
    parseJSON (Object o) = do
        nameKebabCase :: Text <- o .: "name"
        let nameParts = "mdi" : Text.splitOn "-" nameKebabCase
        let name = mkName $ Text.unpack $ pascalCase nameParts
        codepointStr :: String <- o .: "codepoint"
        let codepointInt = fst $ head $ readHex codepointStr
        let codepoint = chr codepointInt
        pure Icon{..}
    parseJSON invalid = prependFailure "parsing icon failed, " (typeMismatch "Object" invalid)

pascalCase :: [Text] -> Text
pascalCase = mconcat . fmap p
  where
    p "" = ""
    p x = Text.cons (toUpper $ Text.head x) (Text.tail x)

icons :: [Icon]
icons = fromMaybe (error "Could not parse file") $ decodeStrict @[Icon] $(embedFile "./meta.json")

mdiEnumName :: Name
mdiEnumName = mkName "MDI"

mkMdiEnum :: DecsQ
-- DataD Cxt Name [TyVarBndr ()] (Maybe Kind) [Con] [DerivClause]
mkMdiEnum = pure [DataD [] mdiEnumName [] Nothing (con <$> icons) []]
  where
    con i = NormalC (name i) []

mkMdiChar :: DecsQ
-- FunD Name [Clause]
-- Clause [Pat] Body [Dec]
mkMdiChar = pure
    [ SigD fname $ ArrowT `AppT` ConT mdiEnumName `AppT` ConT ''Char
    , FunD fname (clause <$> icons)
    ]
  where
    fname = mkName "mdiChar"
    clause i = Clause [ConP (name i) [] []] (body i) []
    body = NormalB . LitE . CharL . codepoint
