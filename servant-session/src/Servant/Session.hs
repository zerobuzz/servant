{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Servant.Session (Session, sessionMiddleware, Cookie(unCookie)) where

import qualified Blaze.ByteString.Builder as Builder
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack)
import           Data.Maybe               (fromJust)
import           Data.Proxy               (Proxy (Proxy))
import           Data.String              (IsString)
import           Data.Typeable            (Typeable)
import           Data.UUID                (toASCIIBytes)
import           GHC.Generics             (Generic)
import           GHC.TypeLits
import           Network.Wai
import           Servant.API              ((:>))
import           Servant.Server.Internal
import           System.Random
import           Web.ClientSession
import           Web.Cookie


data Session (sessKey :: Symbol)

-- | A @UUID@ cookie.
newtype Cookie = Cookie { unCookie :: ByteString }
  deriving (Eq, Show, IsString, Monoid, Read, Typeable, Generic)

instance ( KnownSymbol sessKey, HasServer sublayout
         ) => HasServer (Session sessKey :> sublayout) where
  type ServerT (Session sessKey :> sublayout) m
    = (Key -> Maybe Cookie) -> ServerT sublayout m
  route Proxy a = WithRequest
        $ \request -> route (Proxy :: Proxy sublayout)
        $ passToServer a (fromJust $ go request)
      where
        key = pack $ symbolVal (Proxy :: Proxy sessKey)
        go req = do
            c <- lookup "Cookie" $ requestHeaders req
            v <- lookup key $ parseCookies c
            return (\x -> Cookie <$> decrypt x v)

sessionMiddleware :: Key -> SetCookie -> Middleware
sessionMiddleware key setCookie app req respond
  = case lookup "Cookie" (requestHeaders req) of
    Nothing -> do
        newCookie <- mkCookie key setCookie
        let renderedC = Builder.toByteString $ renderSetCookie newCookie
        app (req { requestHeaders = ("Cookie", renderedC):requestHeaders req})
            (respond . injectCookie renderedC)
    Just _  -> app req respond

mkCookie :: Key -> SetCookie -> IO SetCookie
mkCookie key sc = do
  x <- randomIO
  c <- encryptIO key (toASCIIBytes x)
  return (sc { setCookieValue = c })

injectCookie :: ByteString -> Response -> Response
injectCookie c = mapResponseHeaders (("Set-Cookie", c) :)
