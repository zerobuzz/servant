{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Path rewriting for a site's API
--
-- This module's main purpose is to transform an API, *and* the value that
-- has that type (or more precisely, a type that is a function of that
-- type), into an optimized and canonical form. This allows for e.g.
-- asymptotically faster routing.
--
-- The behavior of the transformed server, however, should not change.
module Servant.CanonicalType where

import Data.Proxy
import Servant.API
import GHC.TypeLits

--------------------------------------------------------------------------
-- Canonicalize
--
-- Combine the individual transformations into a single one.
--------------------------------------------------------------------------

class Canonicalize orig new | orig -> new where
    canonicalize :: orig -> new

{-instance (Canonicalize a b) => Canonicalize (Proxy a) (Proxy b) where-}
    {-canonicalize _ = Proxy-}
instance ( AddNil start withNil
         , QSort withNil sortedWithNil
         , RemoveNil sortedWithNil end
         ) => Canonicalize start end where
    canonicalize = removeNil . qsort . addNil
--------------------------------------------------------------------------
-- Add or remove Nil
--
--------------------------------------------------------------------------

class AddNil orig new | orig -> new where
    addNil :: orig -> new

instance ( AddNil tail nilTail
         ) => AddNil (head :<|> tail) (head :<|> nilTail) where
    addNil (h :<|> t) = h :<|> addNil t

instance ( last' ~ (last :<|> Nil)
         ) => AddNil last last' where
    addNil t = t :<|> Nil

class RemoveNil orig new | orig -> new where
    removeNil :: orig -> new

instance RemoveNil (x :<|> Nil) x where
    removeNil (x :<|> Nil) = x

instance ( RemoveNil y z
         , orig ~ (x :<|> y)
         , new ~ (x :<|> z)
         ) => RemoveNil orig new where
    removeNil (x :<|> y) = x :<|> removeNil y

--------------------------------------------------------------------------
-- Type directed sort
--
-- Adapted from an example by Roman Leshchinskiy [0] to include value-level
-- lock-step logic. We don't have enough information at the value-level to
-- perform the sort, so we need to use the types.
--
-- [0] https://www.haskell.org/haskellwiki/Type_arithmetic#An_Advanced_Example_:_Type-Level_Quicksort
--
--  Quicksort is certainly the wrong idea, though.
--------------------------------------------------------------------------

class Cmp x y c | x y -> c where
    cmp :: Proxy x -> Proxy y -> Ordering
instance (KnownSymbol a, KnownSymbol b, o ~ CmpSymbol a b) => Cmp a b o where
    cmp x y = symbolVal x `compare` symbolVal y
instance (KnownSymbol a) => Cmp a (Capture n t) LT where
    cmp _ _ = LT
instance (KnownSymbol b) => Cmp (Capture n t) b GT where
    cmp _ _ = GT
instance                    Cmp (Capture n t) (Capture n' t') EQ where
    cmp _ _ = EQ
instance (Cmp a' b' o)     => Cmp (a :> a') (a :> b') o where
    cmp (Proxy::Proxy(a :> a'))
        (Proxy::Proxy(b :> b')) = cmp (Proxy::Proxy a') (Proxy::Proxy b')
instance (Cmp a b o)     => Cmp (a :> a') (b :> b') o where
    cmp (Proxy::Proxy(a :> a'))
        (Proxy::Proxy(b :> b')) = cmp (Proxy::Proxy a) (Proxy::Proxy b)
instance (Cmp a b o)     => Cmp a (b :> b') o where
    cmp (Proxy::Proxy a)
        (Proxy::Proxy(b :> b')) = cmp (Proxy::Proxy a) (Proxy::Proxy b)
instance (Cmp a b o)     => Cmp (a :> a') b o where
    cmp (Proxy::Proxy (a :> a'))
        (Proxy::Proxy b) = cmp (Proxy::Proxy a) (Proxy::Proxy b)
instance                    Cmp (Get x) (Get y) EQ where
    cmp _ _ = EQ
-- add queryparams etc.
-- is @cmp@ really necessary (or useful)?


-- put a value into one of three lists according to a pivot element
class Pick c x ls eqs gs ls' eqs' gs' | c x ls eqs gs -> ls' eqs' gs' where
    pick :: Proxy c -> x -> (ls, eqs, gs) -> (ls', eqs', gs')
instance Pick LT x ls eqs gs (x :<|> ls) eqs gs where
    pick _ x (ls, eqs, gs) = (x :<|> ls, eqs, gs)
instance Pick EQ x ls eqs gs ls (x :<|> eqs) gs where
    pick _ x (ls, eqs, gs) = (ls, x :<|> eqs, gs)
instance Pick GT x ls eqs gs ls eqs (x :<|> gs) where
    pick _ x (ls, eqs, gs) = (ls, eqs, x :<|> gs)

data Nil = Nil

-- split a list into three parts according to a pivot element
class Split n xs ls eqs gs | n xs -> ls eqs gs where
    split :: n -> xs -> (ls, eqs, gs)
instance Split n Nil Nil Nil Nil where
    split _ Nil = (Nil, Nil, Nil)
instance ( Split n xs ls' eqs' gs'
         , Cmp x n c
         , Pick c x ls' eqs' gs' ls eqs gs
         ) => Split n (x :<|> xs) ls eqs gs where
    split n (x :<|> xs) = (ls, eqs, gs)
      where (ls', eqs', gs') = split n xs
            (ls , eqs , gs ) = pick (Proxy::Proxy c) x (ls', eqs', gs')

-- zs = xs ++ ys
class App xs ys zs | xs ys -> zs where
    app :: xs -> ys -> zs
instance App Nil ys ys where
    app _ ys = ys
instance App xs ys zs => App (x :<|> xs) ys (x :<|> zs) where
    app (x :<|> xs) ys = x :<|> app xs ys

class App' xs n ys zs | xs n ys -> zs where
    app' :: xs -> n -> ys -> zs
instance App' Nil n ys (n :<|> ys) where
    app' _ n ys = n :<|> ys
instance (App' xs n ys zs) => App' (x :<|> xs) n ys (x :<|> zs) where
    app' (x :<|> xs) n ys = x :<|> app' xs n ys

-- quicksort
class QSort xs ys | xs -> ys where
    qsort :: xs -> ys
instance QSort Nil Nil where
    qsort _ = Nil
instance ( Split x xs ls eqs gs
         , QSort ls ls'
         , QSort gs gs'
         , App eqs gs' geqs
         , App' ls' x geqs ys
         ) => QSort (x :<|> xs) ys where
    qsort (x :<|> xs) = app' ls' x (app eqs gs')
      where (ls, eqs, gs) = split x xs
            ls' = qsort ls
            gs' = qsort gs

--------------------------------------------------------------------------
-- Type families for testing
--------------------------------------------------------------------------
type family CommonInitial x where
    CommonInitial (a :> b :<|> a :> c) = a :> (CommonInitial b :<|> c)
    CommonInitial a = a

type family Sort x where
    Sort (a :> b :<|> a :> d) = a :> (Sort (a :<|> d))
    Sort (a :> b :<|> c :> d) = Sort' (CmpSymbol a c) (a :> b) (c :> d)
    Sort (a :<|> b) = a :<|> b

type family Sort' (o::Ordering) a b where
    Sort' LT a b = a :<|> b
    Sort' EQ a b = a :<|> b
    Sort' GT a b = b :<|> a



