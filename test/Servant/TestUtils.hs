{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Servant.TestUtils where

import Test.Hspec

data HTrue
data HFalse

-- Kiselyov's Type Equality predicate
-- Used to convert what would be a type error in to a run-time Bool.
class  TypeEq x y b | x y -> b where { areEq :: x -> y -> Bool }
instance               TypeEq x x HTrue where { areEq _ _ = True }
instance b ~ HFalse => TypeEq x y b where     { areEq _ _ = False}

infix 4 ~=
(~=) :: TypeEq x y b => x -> y -> Bool
(~=) = areEq

u :: a
u = undefined

infix 3 ~>
(~>) :: (Show a, Eq a) => a -> a -> Expectation
(~>) = shouldBe
