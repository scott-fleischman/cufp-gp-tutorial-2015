{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NP where

import GHC.Exts (Constraint)

-- data HList :: [*] -> * where
--   HNil :: HList '[]
--   HCons :: x -> HList xs -> HList (x ': xs)

-- infixr 5 `HCons`

data NP :: (k -> *) -> [k] -> * where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x ': xs)

-- dream syntax
-- All :: (* -> Constraint) -> [*] -> Constraint

type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint where
  All c '[] = () -- empty constraint, don't need a quote char
  All c (x ': xs) = (c x, All c xs) -- constraint pairing syntax

class (f (g x)) => Compose f g x -- as class because type family requires things to be fully applied, and (Show . f) below is not
instance (f (g x)) => Compose f g x

-- deriving instance ((Show . f) x1, (Show . f) x2, ...) => Show (NP f xs)
deriving instance All (Compose Show f) xs => Show (NP f xs)

infixr 5 :*

newtype I a = I a -- identity
  deriving Show

newtype K a b = K a -- constant
  deriving Show

map_NP :: (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP _ Nil = Nil
map_NP f (x :* xs) = f x :* map_NP f xs

eq_NP :: (All Eq xs) => NP I xs -> NP I xs -> Bool
eq_NP Nil Nil = True
eq_NP (I x :* xs) (I y :* ys) = x == y && eq_NP xs ys
eq_NP _ _ = undefined -- satisfy current exhaustivity checker

-- cmap_NP :: (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
