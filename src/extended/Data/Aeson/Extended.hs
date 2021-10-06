{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Aeson.Extended
       ( module Data.Aeson
       , aesonOptions
       , parsejsonoundedEnum
       , deriveJSON'
       , deriveJSON
       , toLowerCamelCase
       , splitCamelWords
       ) where

import           Data.Aeson
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types
import           Language.Haskell.TH (Dec, Name, Q, nameBase)
import           Data.Char (isDigit, isUpper, toLower)

-- | Options to derive @'ToJSON'@/@'FromJSON'@ instances.
--
-- @aesonOptions prefix@ drops @prefix@ for every field and converts
-- what's left to @lowerCamelCase@.
aesonOptions :: String -> Options
aesonOptions prefix = defaultOptions
  { fieldLabelModifier = nameModifier prefix
  , constructorTagModifier = nameModifier prefix
  , sumEncoding = ObjectWithSingleField
  , omitNothingFields = True
  }

-- | Default name modifier.
-- Transforms to @lowerCamelCase@ and cuts longest common whole-word prefix.
--
-- >>> nameModifier "Dog" "dogName"
-- "name"
-- >>> nameModifier "PersonEntry" "personLastName"
-- "lastName"
-- >>> nameModifier "MessageStatus" "MessageSent"
-- "sent"
-- >>> nameModifier "SMSEntry" "smsExpiresAt"
-- "expiresAt"
-- >>> nameModifier "Progress" "progress75"
-- "75"
nameModifier :: String -> String -> String
nameModifier prefix name = concat (lowerFirstWord suffix)
  where
    (_, _, suffix) = commonPrefixOn
      (map toLower)
      (splitCamelWords prefix)
      (splitCamelWords name)
    lowerFirstWord [] = []
    lowerFirstWord (w : ws) = map toLower w : ws


-- | Strip longest common prefix of to lists.
--
-- >>> commonPrefixOn id "MessageStatus" "MessageDelivered" :: (String, String, String)
-- ("Message","Status","Delivered")
-- >>> commonPrefixOn id "MessageStatus" "MessageSent" :: (String, String, String)
-- ("MessageS","tatus","ent")
--
-- prop> commonPrefixOn id xs xs == (xs, [], [])
-- prop> commonPrefixOn id [] xs == ([], [], xs)
-- prop> commonPrefixOn id xs [] == ([], xs, [])
commonPrefixOn :: Eq b => (a -> b) -> [a] -> [a] -> ([a], [a], [a])
commonPrefixOn f (x : xs) (y : ys)
  | f x == f y = (x : prefix, xs', ys')
  | otherwise = ([], x : xs, y : ys)
  where (prefix, xs', ys') = commonPrefixOn f xs ys
commonPrefixOn _ xs ys = ([], xs, ys)

-- | Convert @CamelCase@ to @lowerCamelCase@.
--
-- >>> toLowerCamelCase "Call"
-- "call"
-- >>> toLowerCamelCase "CampaignId"
-- "campaignId"
-- >>> toLowerCamelCase "SMSId"
-- "smsId"
-- >>> toLowerCamelCase "progress75"
-- "progress75"
toLowerCamelCase :: String -> String
toLowerCamelCase = concat . lowerFirstWord . splitCamelWords
  where lowerFirstWord [] = []
        lowerFirstWord (w : ws) = map toLower w : ws

-- | Split @CamelCase@ name into its constituent words.
--
-- >>> splitCamelWords "CamelCase"
-- ["Camel","Case"]
-- >>> splitCamelWords "SMSEntry"
-- ["SMS","Entry"]
-- >>> splitCamelWords "progress75"
-- ["progress","75"]
--
-- prop> concat (splitCamelWords s) == s
splitCamelWords :: String -> [String]
splitCamelWords = reverse . splitWordsReversed . reverse
  where
    splitWordsReversed :: String -> [String]
    splitWordsReversed [] = []
    splitWordsReversed rs
      | null ls = reverse us : splitWordsReversed urs
      | otherwise = case lrs of
        [] -> [reverse ls]
        (c : cs) -> (c : reverse ls) : splitWordsReversed cs
      where
        (ls, lrs) = break isBorder rs
        (us, urs) = span isBorder rs
        isBorder c = isUpper c || isDigit c

-- | Helper for the parsing of the bounded values.
parsejsonoundedEnum
  :: forall a
   . (Bounded a, Enum a)
  => String  -- ^ Value description to be printed in error messages.
  -> Value
  -> Parser a
parsejsonoundedEnum err js = do
  n <- parseJSON js
  if minN <= n && n <= maxN
    then return (toEnum n)
    else fail (errorMessage n)
  where
    minN = fromEnum (minBound :: a)
    maxN = fromEnum (maxBound :: a)
    errorMessage n = 
      "illegal " ++ err ++ ": " ++ show n ++ 
      " (expected a value between " ++ 
      show minN ++ " and " ++ show maxN ++ ")" 
     
-- | Derive 'ToJSON' and 'FromJSON'
-- with Template Haskell using 'aesonOptions'.
deriveJSON' :: Name -> Q [Dec]
deriveJSON' typeName = deriveJSON (aesonOptions (nameBase typeName)) typeName