{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Servant.Session (SSession) where

import           Data.Proxy               (Proxy (Proxy))
import           Network.Wai
import           Servant.API              ((:>))
import           Servant.Server.Internal
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Session


data SSession (m :: * -> *) (k :: *) (v :: *)

instance ( HasServer sublayout
         ) => HasServer (SSession n k v :> sublayout) where
  type ServerT (SSession n k v :> sublayout) m
    = (Vault.Key (Session n k v) -> Maybe (Session n k v)) -> ServerT sublayout m
  route Proxy a = WithRequest $ \request -> route (Proxy :: Proxy sublayout)
        $ passToServer a (\(key :: Vault.Key (Session n k v)) -> Vault.lookup key $ vault request)
