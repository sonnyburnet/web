{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Control.Lens.Iso.Extended
       (
          textbs
        , textbsl
        , integral
        , stext
        , stextl
        , seql
        , listSeq
        , stringify
        , lazytext
        , bytesLazy
        , enumtext
        , jsonb
       ) where

import Control.Lens
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import Data.Foldable           (toList)
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Aeson
import Data.String.Interpolate
import Text.Casing


-- WARNING: Strictly speaking, 'utf8' is not isomorphism, since exists
-- ByteString, that is not decodable as Text. But it is very convenient.
textbs :: Iso' T.Text B.ByteString
textbs = iso T.encodeUtf8 T.decodeUtf8

textbsl :: Iso' T.Text BL.ByteString
textbsl = iso (LT.encodeUtf8 . LT.fromStrict)
                 (LT.toStrict . LT.decodeUtf8)

bytesLazy :: Iso' BL.ByteString B.ByteString
bytesLazy = iso BL.toStrict BL.fromStrict

integral :: (Integral a, Integral b) => Iso' a b
integral = iso fromIntegral fromIntegral

stext :: Iso' String T.Text
stext = iso T.pack T.unpack

stextl :: Iso' String LT.Text
stextl = iso LT.pack LT.unpack

lazytext :: Iso' LT.Text T.Text
lazytext = iso LT.toStrict LT.fromStrict

seql :: Iso' [a] (Seq.Seq a)
seql = iso Seq.fromList toList

listSeq :: Iso' [a] (Seq.Seq a)
listSeq = iso Seq.fromList toList

stringify :: (Show a, Read a) => Iso' a String
stringify = iso show read

enumtext :: (FromJSON a, ToJSON a) => Iso' a T.Text
enumtext = 
  iso ((^.from textbsl.to strip._Just) . encode) 
      (either err id `fmap` 
       (eitherDecode . (^.from stext.to pascal.stext.to mkJson.textbsl))) 
  where err = error . (<>) "decode error: "
        mkJson x = "\"" <> x <> "\""
        strip x = do 
         x' <- T.stripPrefix "\"" x
         T.stripSuffix "\"" x'
  
jsonb :: (FromJSON a, ToJSON a) => Iso' a Value
jsonb = iso toJSON (fmap getObj fromJSON) 
    where getObj (Success x) = x
          getObj (Error e) = error [i|decode error: #{show e}|]