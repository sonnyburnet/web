{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module TH.Mk
       ( mkToSchemaAndJSON
       , mkToSchemaAndDefJSON
       , mkEnumConvertor
       , mkFromHttpApiDataEnum
       , mkParamSchemaEnum
       , mkMigrationSeq
       , mkMigrationTest
       , mkEncoder
       , mkArbitrary
       )
       where

import Control.Lens
import Language.Haskell.TH
import Data.Aeson.Extended
import Data.Swagger.Schema.Extended
import Data.Swagger
import Data.Maybe
import Control.Lens.Iso.Extended
import Text.Casing (quietSnake)
import Servant.API
import qualified Data.Text as T
import Control.Monad.IO.Class
import System.Directory
import System.FilePath.Posix
import Data.List
import System.IO
import Control.Monad
import qualified System.IO.Strict as IOS
import Test.QuickCheck.Arbitrary.Generic
import Language.Haskell.TH.Syntax
import Data.Coerce

getConstructorType (RecC _ [(_, _, ConT c)]) = c
getConstructorType (NormalC _ [(_, ConT c)]) = c
getConstructorType _ = error "data not supported"

getConstructorPat (RecC c _) = c
getConstructorPat (NormalC c _) = c
getConstructorPat _ = error "data not supported"


mkArbitrary :: Name -> Q [Dec]
mkArbitrary name =
  [d|instance Arbitrary $(conT (mkName (nameBase name))) where
       arbitrary = genericArbitrary
       shrink = genericShrink|]

mkToSchemaAndJSON :: Name -> Q [Dec]
mkToSchemaAndJSON name = do
  x <- deriveJSON' name
  y <- deriveToSchema name
  return $ x ++ y

mkToSchemaAndDefJSON :: Name -> Q [Dec]
mkToSchemaAndDefJSON name = do
  x <- deriveJSON
       (defaultOptions
        { tagSingleConstructors = True })
       name
  y <- deriveToSchemaDef name
  return $ x ++ y

mkEnumConvertor :: Name -> Q [Dec]
mkEnumConvertor name =
  do TyConI (DataD _ _ _ _ xs _) <- reify name
     let stripUnderScore = filter (not . (`elem` ("_" :: String))) . nameBase
     let stripPrefix s = fromMaybe s $ s^?stext.to (T.stripPrefix (nameBase name^.stext))._Just.from stext
     let str = mkName "String"
     let err = mkName "error"
     let isoNFrom = mkName ("from" <> stripUnderScore name)
     let mkClauseFrom (NormalC n _) =
          let n' = (quietSnake . stripPrefix . nameBase) n
          in Clause [ConP n []] (NormalB (LitE (StringL n'))) []
     let fromSig = SigD isoNFrom (AppT (AppT ArrowT (ConT name)) (ConT str))
     let fromN = FunD isoNFrom (map mkClauseFrom xs)
     let isoNTo = mkName ("to" <> stripUnderScore name)
     let mkClauseTo (NormalC n _) =
          let n' = (quietSnake . stripPrefix . nameBase) n
          in Clause [LitP (StringL n')] (NormalB (ConE n)) []
     let mkErrorClauseTo =
          Clause
          [WildP]
          (NormalB
           (AppE (VarE err)
            (LitE (StringL ("error in enum converting: " <>
             nameBase name))))) []
     let toSig = SigD isoNTo (AppT (AppT ArrowT (ConT str)) (ConT name))
     let toN = FunD isoNTo (map mkClauseTo xs ++ [mkErrorClauseTo])
     let isoN = mkName $ "iso" <> stripUnderScore name
     let iso = mkName "Iso'"
     let isoSig = SigD isoN (AppT (AppT (ConT iso) (ConT name)) (ConT str))
     iosDec <- [d| $(varP isoN) = $(appE (appE (varE (mkName "iso")) (varE isoNFrom)) (varE isoNTo)) |]
     return $ [fromSig, fromN, toSig, toN, isoSig] ++ iosDec

mkFromHttpApiDataEnum :: Name -> Q Exp -> Q [Dec]
mkFromHttpApiDataEnum name iso = do
  reified <- reify name
  let base = nameBase name
  [d| instance FromHttpApiData $(conT name) where
        parseUrlPiece :: T.Text -> Either T.Text $(conT name)
        parseUrlPiece x = view $iso x
   |]

newtype ParamSchemaEnumCon = ParamSchemaEnumCon Con

instance Lift ParamSchemaEnumCon where
  lift (ParamSchemaEnumCon (NormalC n _)) = pure $ ConE n
  lift _ = error "unsupported constructor"
  liftTyped = undefined

mkParamSchemaEnum :: Name -> Q Exp-> Q [Dec]
mkParamSchemaEnum name iso = do
  r <- reify name
  TyConI (DataD _ _ _ _ old_xs _) <- reify name
  let new_xs = coerce old_xs :: [ParamSchemaEnumCon]
  [d| instance ToParamSchema $(conT name) where
       toParamSchema _ = mempty & type_ ?~
         SwaggerString & enum_ ?~ (new_xs <&> \x -> view $iso (coerce x))
   |]

loadMigrationList :: IO [(Integer, String)]
loadMigrationList = do
  dir <- getCurrentDirectory
  let migDir = dir </> "migration/index"
  let mkTpl file =
        fmap
        (, migDir </> file)
        (Data.List.stripPrefix
        "version"
        (dropExtension file))
  fs <- fmap (mapMaybe mkTpl) (listDirectory migDir)
  fmap (sortOn (^._1)) $ forM fs $ \x -> do
    hdl <- openFile (x^._2) ReadMode
    content <- IOS.hGetContents hdl
    hClose hdl
    return (read @Integer (x^._1), content)

loadMigrationListTest :: IO [String]
loadMigrationListTest = do
  dir <- getCurrentDirectory
  let migDir = dir </> "migration/index"
  let mkTpl file =
        fmap
        (, migDir </> file)
        (Data.List.stripPrefix
        "version"
        (dropExtension file))
  fs <- fmap (mapMaybe mkTpl) (listDirectory migDir)
  fmap (map snd . sortOn (^._1)) $ forM fs $ \x -> do
    hdl <- openFile (x^._2) ReadMode
    content <- IOS.hGetContents hdl
    hClose hdl
    return (read @Integer (x^._1), content)

mkMigrationSeq :: Q [Dec]
mkMigrationSeq = do
  migrations <- liftIO loadMigrationList
  let version = mkName "Version"
  let lastIdx = fst (last migrations)
  let step = mkName "MigrationStep"
  let list = mkName "list"
  let next = mkName "NextSql"
  let stop = mkName "Stop"
  let mkVersion xs [] = xs ++ [TupE [Just (AppE (ConE version) (LitE (IntegerL lastIdx))), Just (ConE stop)]]
      mkVersion xs ((i, str):is) =
          TupE [Just (AppE (ConE version) (LitE (IntegerL (i - 1))))
               , Just (AppE (AppE (ConE next)
                 (LitE (StringL str)))
                 (AppE (ConE version) (LitE (IntegerL i))))]
          : mkVersion xs is
  let xs = if null migrations then [] else mkVersion [] migrations
  return [ValD (VarP list) (NormalB (ListE xs)) []]

mkMigrationTest :: Q [Dec]
mkMigrationTest = do
  xs <- liftIO loadMigrationListTest
  let list = mkName "list"
  let mkSql str = LitE (StringL str)
  let xs' = if null xs then [] else map mkSql xs
  let sig = SigD list (AppT ListT (ConT (mkName "ByteString")))
  return $ sig : [ValD (VarP list) (NormalB (ListE xs')) []]

mkEncoder :: Name -> Q [Dec]
mkEncoder name = do
  TyConI (DataD _ _ _ _ c@[RecC _ xs] _) <- reify name
  let types = flip map xs $ \(_, _, ty) ->
        case ty of
          ConT t -> mkType t
          AppT (ConT x) (ConT y) -> AppT (ConT x) (mkType y)
          _ -> ty
  let mkTpl [] tpl = tpl
      mkTpl (t:ts) app = mkTpl ts (AppT app t)
  let mkTypeSyn =
        TySynD (mkName (nameBase name <> "Encoder")) []
               (mkTpl types (TupleT (length xs)))
  let mkEncoderSig =
        SigD (mkName ("mkEncoder" <> nameBase name))
             (AppT (AppT ArrowT (ConT name))
                    (mkTpl types (TupleT (length xs))))
  let fields = flip map xs $ \(field, _, _) -> VarE (mkName (nameBase field))
  let mkTplExp r [] = r
      mkTplExp r (f:fs) = mkTplExp (r ++ [Just (AppE f (VarE (mkName "x")))]) fs
  let mkEncoderFun =
        FunD (mkName ("mkEncoder" <> nameBase name))
             [Clause [] (NormalB (LamE [VarP (mkName "x")] (AppE (ConE (mkName "Just")) (TupE (mkTplExp [] fields))))) []]
  return [mkTypeSyn, mkEncoderSig, mkEncoderFun]
  where
    mkType t =
      case nameModule t of
        Just "Data.Text.Internal" -> ConT (mkName ("T." <> nameBase t))
        Just "Data.Text.Internal.Lazy" -> ConT (mkName ("LT." <> nameBase t))
        Just "Protobuf.Scalar" -> ConT (mkName ("Protobuf." <> nameBase t))
        _ -> ConT (mkName (nameBase t))