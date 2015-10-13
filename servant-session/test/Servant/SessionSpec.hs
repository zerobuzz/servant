{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS -fno-warn-orphans #-}

module Servant.SessionSpec (spec) where

import           Data.ByteString    (ByteString)
import           Data.ByteString.Lazy    (fromStrict)
import           Data.Time.Clock    ()
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Test   (simpleBody, simpleHeaders)
import           Servant
import           Test.Hspec         (Spec, context, describe, it, shouldBe,
                                     shouldSatisfy)
import           Test.Hspec.Wai
import qualified Data.Vault.Lazy as Vault
import           Web.Cookie
import qualified Data.Map as Map
import Network.Wai.Session
import Network.Wai.Session.Map
import Data.IORef
import Control.Monad.Trans.Except

import Servant.Session

spec :: Spec
spec = describe "Servant.Session" . with server $ do

  context "the cookie is set" $ do

    it "has access to the cookie" $ do
        x <- request methodGet "" [("Cookie", "test=const")] ""
        liftIO $ simpleBody x `shouldSatisfy` (== "1")


  context "no cookie is set" $ do

    it "one will be in the Set-Cookie header of the response" $ do
        resp <- request methodGet "" [] ""
        let Just c = parseSetCookie <$> lookup "Set-Cookie" (simpleHeaders resp)
        liftIO $ setCookieName c `shouldBe` setCookieName setCookieOpts
        liftIO $ setCookieValue c `shouldBe` "const"

    it "adds SetCookie params" $ do
        resp <- request methodGet "" [] ""
        liftIO $ print resp
        let Just c = parseSetCookie <$> lookup "Set-Cookie" (simpleHeaders resp)
        liftIO $ setCookieMaxAge c `shouldBe` setCookieMaxAge setCookieOpts

type API = SSession IO Int String :> Get '[JSON] String

setCookieOpts :: SetCookie
setCookieOpts = def { setCookieName = "test" , setCookieMaxAge = Just 300 }

sessionMiddleware :: SessionStore IO Int a -> Vault.Key (Session IO Int a) -> Middleware
sessionMiddleware s = withSession s "test" setCookieOpts

server ::  IO Application
server = do
    ref <- mapStore (return "const")
    key <- Vault.newKey
    return $ sessionMiddleware ref key
           $ serve (Proxy :: Proxy API) (handler key)

handler :: Vault.Key (Session IO Int String)
        -> (Vault.Key (Session IO Int String) -> Maybe (Session IO Int String))
        -> ExceptT ServantErr IO String
handler key map = do
    x <- liftIO $ lkup 1
    liftIO $ print ("Cookie:\t" ++ show x)
    case x of
        Nothing -> liftIO (ins 1 "a") >> return "Nothing"
        Just y -> liftIO (ins 1 ('a':y)) >> return (show y)
  where
   Just (lkup, ins) = map key
