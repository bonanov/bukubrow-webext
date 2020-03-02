module Data.Natural where

import Prelude

import Control.Alternative.Custom (ensure)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either, note)
import Data.Lens (Prism', preview, prism', review)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)

newtype Natural = Natural Int

derive instance newtypeNatural :: Newtype Natural _

instance encodeNatural :: EncodeJson Natural where
    encodeJson = toInt >>> encodeJson

instance decodeNatural :: DecodeJson Natural where
    decodeJson json = do
        int <- decodeJson json :: Either String Int
        note "Value is not a Natural" (fromInt int)

isNatural :: Int -> Boolean
isNatural = (_ >= 0)

naturalPrism :: Prism' Int Natural
naturalPrism = prism' (un Natural) (ensure isNatural >>> map Natural)

sign :: Int -> Natural
sign = max 0 >>> Natural

fromInt :: Int -> Maybe Natural
fromInt = preview naturalPrism

toInt :: Natural -> Int
toInt = review naturalPrism
