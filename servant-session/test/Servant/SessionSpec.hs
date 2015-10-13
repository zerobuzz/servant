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
import           Web.ClientSession
import           Web.Cookie

import Servant.Session

spec :: Spec
spec = describe "Servant.Session" . with server $ do

    context "the cookie is set" $ do

        it "returns nothing when the cookie cannot be decoded" $ do
            request methodGet "" [("Cookie", "test=val")] "" `shouldRespondWith` "Nothing"

    context "no cookie is set" $ do

        it "returns a fresh one" $ do
            resp <- request methodGet "" [] ""
            liftIO $ simpleBody resp `shouldSatisfy` (/= mempty)

        it "one will be in the Set-Cookie header of the response" $ do
            resp <- request methodGet "" [] ""
            let Just c = parseSetCookie <$> lookup "Set-Cookie" (simpleHeaders resp)
            liftIO $ setCookieName c `shouldBe` setCookieName setCookieOpts

        it "adds SetCookie params" $ do
            resp <- request methodGet "" [] ""
            let Just c = parseSetCookie <$> lookup "Set-Cookie" (simpleHeaders resp)
            liftIO $ setCookieMaxAge c `shouldBe` setCookieMaxAge setCookieOpts

type API = Session "test" :> Get '[OctetStream] (Maybe ByteString)

instance MimeRender OctetStream (Maybe ByteString) where
    mimeRender _ Nothing  = "Nothing"
    mimeRender _ (Just x) = fromStrict x


setCookieOpts :: SetCookie
setCookieOpts = def { setCookieName = "test" , setCookieMaxAge = Just 300 }

server ::  IO Application
server = do
    (_, key) <- randomKey
    let handler x = return (unCookie <$> x key)
    return $ sessionMiddleware key setCookieOpts
           $ serve (Proxy :: Proxy API) handler
