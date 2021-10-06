{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.HList.HList.Extended (module Data.HList.HList) where

import Data.HList.HList

instance HTuple '[p1,p2,p3,p4,p5,p6,p7] (p1,p2,p3,p4,p5,p6,p7) where
  hToTuple (p1 `HCons` p2 `HCons` p3 `HCons` p4 `HCons` p5 `HCons` p6 `HCons` p7 `HCons` HNil) = (p1,p2,p3,p4,p5,p6,p7)
  hFromTuple (p1,p2,p3,p4,p5,p6,p7) = (p1 `HCons` p2 `HCons` p3 `HCons` p4 `HCons` p5 `HCons` p6 `HCons` p7 `HCons` HNil)

instance HTuple '[p1,p2,p3,p4,p5,p6,p7,p8] (p1,p2,p3,p4,p5,p6,p7,p8) where
  hToTuple (p1 `HCons` p2 `HCons` p3 `HCons` p4 `HCons` p5 `HCons` p6 `HCons` p7 `HCons` p8 `HCons` HNil) = (p1,p2,p3,p4,p5,p6,p7,p8)
  hFromTuple (p1,p2,p3,p4,p5,p6,p7,p8) = (p1 `HCons` p2 `HCons` p3 `HCons` p4 `HCons` p5 `HCons` p6 `HCons` p7 `HCons` p8 `HCons` HNil)

instance HTuple '[p1,p2,p3,p4,p5,p6,p7,p8,p9] (p1,p2,p3,p4,p5,p6,p7,p8,p9) where
  hToTuple (p1 `HCons` p2 `HCons` p3 `HCons` p4 `HCons` p5 `HCons` p6 `HCons` p7 `HCons` p8 `HCons` p9 `HCons` HNil) = (p1,p2,p3,p4,p5,p6,p7,p8,p9)
  hFromTuple (p1,p2,p3,p4,p5,p6,p7,p8,p9) = (p1 `HCons` p2 `HCons` p3 `HCons` p4 `HCons` p5 `HCons` p6 `HCons` p7 `HCons` p8 `HCons` p9 `HCons` HNil)