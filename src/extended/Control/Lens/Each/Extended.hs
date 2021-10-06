{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Lens.Each.Extended (module E) where

import Control.Lens.Each as E

instance (a~a2, a~a3, a~a4, a~a5, a~a6, a~a7, a~a8, a~a9, a~a10, b~b2, b~b3, b~b4, b~b5, b~b6, b~b7, b~b8, b~b9, b~b10) => Each (a,a2,a3,a4,a5,a6,a7,a8,a9,a10) (b,b2,b3,b4,b5,b6,b7,b8,b9,b10) a b where
  each f ~(a,b,c,d,e,g,h,i,j,k) = (,,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j <*> f k
  {-# INLINE each #-}