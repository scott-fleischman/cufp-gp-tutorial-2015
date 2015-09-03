{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NP where

import Data.Proxy
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

-- data Proxy a = Proxy

cmap_NP :: (All c xs) => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ _ Nil = Nil
cmap_NP p f (x :* xs) = f x :* cmap_NP p f xs

-- type class constraints correspond to actual functions, making it inflexible (leaking implementation details)
-- class resolution happens at compile time, will unfold the entire list at compile time
class Pure_NP (xs :: [k]) where
  pure_NP' :: (forall x . f x) -> NP f xs

instance Pure_NP '[] where
  pure_NP' _ = Nil

instance (Pure_NP xs) => Pure_NP (x ': xs) where
  pure_NP' x = x :* pure_NP' x


class SingI (xs :: [k]) where
  sing :: Sing xs

instance SingI '[] where
  sing = SNil

instance (SingI xs) => SingI (x ': xs) where
  sing = SCons

data Sing :: [k] -> * where
  SNil :: Sing '[]
  SCons :: SingI xs => Sing (x ': xs)

-- here nothing will be unrolled at compile time
pure_NP :: SingI xs => (forall x . f x) -> NP f xs
pure_NP x = go sing x
  where
    go :: Sing xs -> (forall x . f x) -> NP f xs
    go SNil _ = Nil
    go SCons y = y :* pure_NP y

cpure_NP :: (SingI xs, All c xs) => Proxy c -> (forall x . c x => f x) -> NP f xs
cpure_NP = undefined

-- reason for doing this: map f xs = pure f <*> xs
-- get map we defined, and zipwith2, 3 etc.

ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil Nil = Nil
ap_NP (Fn f :* fs) (x :* xs) = f x :* ap_NP fs xs
ap_NP _ _ = undefined

newtype (f -.-> g) x = Fn (f x -> g x)

map_NP' :: SingI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP' f xs = pure_NP (Fn f) `ap_NP` xs

-- ex zipwith
-- ex redefine eq

zipWith_NP :: SingI xs => (forall x . f x -> g x -> h x) -> NP f xs -> NP g xs -> NP h xs
zipWith_NP f xs ys = pure_NP (fn_2 f) `ap_NP` xs `ap_NP` ys

czipWith_NP :: (All c xs, SingI xs)
            => Proxy c
            -> (forall x . c x => f x -> g x -> h x)
            -> NP f xs -> NP g xs -> NP h xs
czipWith_NP p f xs ys = cpure_NP p (fn_2 f) `ap_NP` xs `ap_NP` ys

fn_2 f = Fn (\x -> Fn (\y -> f x y))

eq_NP' :: (All Eq xs, SingI xs) => NP I xs -> NP I xs -> Bool
eq_NP' xs ys =
    and
  $ collapse_NP
  $ czipWith_NP (Proxy :: Proxy Eq) (\ (I x) (I y) -> K (x == y)) xs ys

collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil = []
collapse_NP (K x :* xs) = x : collapse_NP xs
