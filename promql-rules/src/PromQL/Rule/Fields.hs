-- | The pieces that the rule file and the rule test file are both written with.
--
-- Each file is written as JSON, which Prometheus and promtool read because
-- JSON is YAML, and which anything else can read without a YAML parser's
-- guesses about what a bare word means.
--
-- Left out of mutation testing, like the codecs and the file renderers that
-- use it: the golden files of what they write fail on any change to it.
module PromQL.Rule.Fields
  ( labelMapField,
    validatedObject,
    validatedText,
    renderFile,
  )
where

import Autodocodec
import qualified Data.Aeson.Encode.Pretty as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Validity
import PromQL

{-# ANN module ("DisableMutations" :: String) #-}

-- | A map of labels as a field, or no field where there are none, so that a
-- file says no more than its author did.
--
-- The names are checked where the value holding them is, because each
-- codec here decodes and then validates the whole.
labelMapField :: Text -> Text -> ObjectCodec (Map LabelName Text) (Map LabelName Text)
labelMapField key =
  optionalFieldWithOmittedDefaultWith
    key
    (dimapCodec (Map.mapKeys LabelName) (Map.mapKeys unLabelName) (mapCodec textCodec))
    Map.empty

-- | An object that is refused, once it is read, if it is not valid.
validatedObject :: (Validity a) => Text -> ObjectCodec a a -> JSONCodec a
validatedObject name = bimapCodec prettyValidate id . object name

-- | Text that is refused, once it is read, if what it names is not valid.
validatedText :: (Validity a) => (Text -> a) -> (a -> Text) -> JSONCodec a
validatedText from to = bimapCodec (prettyValidate . from) to textCodec

-- | Two spaces and sorted keys, so that the same rules are always written the
-- same way and a change to one rule shows up as a change to that rule.
renderFile :: ValueCodec a void -> a -> ByteString
renderFile fileCodec =
  LazyByteString.toStrict
    . JSON.encodePretty'
      JSON.defConfig
        { JSON.confIndent = JSON.Spaces 2,
          JSON.confCompare = compare
        }
    . toJSONVia fileCodec
