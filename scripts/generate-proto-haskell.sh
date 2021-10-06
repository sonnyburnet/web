#!/usr/bin/env stack

import System.Directory 
import Data.Bool
import System.Process
import Control.Monad
import System.FilePath.Posix

main :: IO ()
main = 
  do 
   xs <- search "sub/proto" 
   flip mapM_ xs $ \f ->
     unless (takeExtension f == ".git") $ do 
      let f' = tail $ dropWhile (/= '/') $ tail $ dropWhile (/= '/') f
      callProcess "stack" 
        ["exec"
        , "--no-nix"
        , "--verbose"
        , "compile-proto-file"
        , "--"
        , "--out"
        , "src/proto/"
        ,  "--includeDir"
        , "sub/proto/"
        , "--proto"
        , f']

search path =
  do
    entries <- listDirectory path
    r <- flip mapM entries $ \p -> do
      ok <- doesDirectoryExist $ path <> "/" <> p
      bool (return [path <> "/" <> p]) 
           (search (path <> "/" <> p)) ok
    return $ concat r