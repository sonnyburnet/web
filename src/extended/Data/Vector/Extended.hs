module Data.Vector.Extended 
      ( module Data.Vector
      , unzip7
      , unzip8
      , unzip9
      ) where 

import Data.Vector
import Data.Vector.Generic hiding (Vector)
import Prelude hiding (map)

unzip7 :: Vector (a, b, c, d, e, f, g) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g)
unzip7 xs = 
  (Data.Vector.Generic.map (\(a, _, _, _, _, _, _) -> a) xs,
   Data.Vector.Generic.map (\(_, b, _, _, _, _, _) -> b) xs,
   Data.Vector.Generic.map (\(_, _, c, _, _, _, _) -> c) xs,
   Data.Vector.Generic.map (\(_, _, _, d, _, _, _) -> d) xs,
   Data.Vector.Generic.map (\(_, _, _, _, e, _, _) -> e) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, f, _) -> f) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, _, g) -> g) xs)

unzip8 :: Vector (a, b, c, d, e, f, g, h) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h)
unzip8 xs =
  (Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _) -> a) xs,
   Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _) -> b) xs,
   Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _) -> c) xs,
   Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _) -> d) xs,
   Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _) -> e) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _) -> f) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _) -> g) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h) -> h) xs)  

unzip9 :: Vector (a, b, c, d, e, f, g, h, k) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f, Vector g, Vector h, Vector k)
unzip9 xs =
  (Data.Vector.Generic.map (\(a, _, _, _, _, _, _, _, _) -> a) xs,
   Data.Vector.Generic.map (\(_, b, _, _, _, _, _, _, _) -> b) xs,
   Data.Vector.Generic.map (\(_, _, c, _, _, _, _, _, _) -> c) xs,
   Data.Vector.Generic.map (\(_, _, _, d, _, _, _, _, _) -> d) xs,
   Data.Vector.Generic.map (\(_, _, _, _, e, _, _, _, _) -> e) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, f, _, _, _) -> f) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, _, g, _, _) -> g) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, _, _, h, _) -> h) xs,
   Data.Vector.Generic.map (\(_, _, _, _, _, _, _, _, k) -> k) xs)  

