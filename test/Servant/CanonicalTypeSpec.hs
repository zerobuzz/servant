{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.CanonicalTypeSpec where

import Servant.API
import Servant.CanonicalType
import Test.Hspec

import Servant.TestUtils


spec :: Spec
spec = describe "Servant.CanonicalType" canonicalizeSpec

canonicalizeSpec :: Spec
canonicalizeSpec =
    describe "canonicalize" $
        it "Should sort the API" $ do
            let sorted = canonicalize (u::TestBinTreeApiA)
            let ans = u::TestBinTreeApiB
            sorted ~= ans  ~> True

type TestBinTreeApiA =
       "d" :> Get Int
  :<|> "b" :> Get Int
  :<|> "a" :> Get Int
  :<|> "c" :> Get Int
  :<|> "e" :> Get Int

type TestBinTreeApiB = BinTree ("c" :> Get Int)
                               ("a" :> Get Int :<|> "b" :> Get Int)
                               ("d" :> Get Int :<|> "e" :> Get Int)
