{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
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
            let sorted = canonicalize (u::TestSortedApiA)
            sorted ~= (u::TestSortedApiB) ~> True

type TestSortedApiA =
       "d" :> Get Int
  :<|> "b" :> Get Int
  :<|> "a" :> Get Int
  :<|> "c" :> Get Int

type TestSortedApiB =
       "a" :> Get Int
  :<|> "b" :> Get Int
  :<|> "c" :> Get Int
  :<|> "d" :> Get Int
