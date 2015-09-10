{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Servant.Server.Internal.RoutingApplication where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                (Applicative, (<$>))
import           Data.Monoid                        (Monoid, mappend, mempty,
                                                     (<>))
#endif
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as BL
import           Data.IORef                         (newIORef, readIORef,
                                                     writeIORef)
import           Network.HTTP.Types                 hiding (Header,
                                                     ResponseHeaders)
import           Network.Wai                        (Application, Request,
                                                     Response, ResponseReceived,
                                                     requestBody,
                                                     strictRequestBody)
import           Servant.API                        ((:<|>) (..))
import           Servant.Server.Internal.ServantErr

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

-- | A wrapper around @'Either' 'RouteMismatch' a@.
data RouteResult a =
    Retriable ServantErrWithPriority      -- ^ Keep trying other paths
  | NonRetriable ServantErrWithPriority   -- ^ Stop trying.
  | HandlerVal a
  deriving (Eq, Show, Read, Functor)

instance Monoid (RouteResult a) where
    mempty = Retriable $ ServantErrWithPriority err404
    Retriable a `mappend` Retriable b = if a > b
        then Retriable a
        else Retriable b
    Retriable _ `mappend` x           = x
    x           `mappend` Retriable _ = x
    NonRetriable a `mappend` NonRetriable b = if a > b
        then NonRetriable a
        else NonRetriable b
    NonRetriable _ `mappend` HandlerVal a = HandlerVal a
    HandlerVal a   `mappend` _            = HandlerVal a


instance Eq a => Ord (RouteResult a) where
    Retriable a    <= Retriable b    = a <= b
    Retriable _    <= _              = True
    _              <= Retriable _    = False
    NonRetriable a <= NonRetriable b = a <= b
    NonRetriable _ <= _              = True
    _              <= NonRetriable _ = False
    HandlerVal _   <= _              = True

-- Note that the ordering of the constructors has great significance! It
-- determines the Ord instance and, consequently, the monoid instance.
data RouteMismatch =
    NotFound           -- ^ the usual "not found" error
  | WrongMethod        -- ^ a more informative "you just got the HTTP method wrong" error
  | UnsupportedMediaType -- ^ request body has unsupported media type
  | InvalidBody String -- ^ an even more informative "your json request body wasn't valid" error
  | HttpError Status (Maybe BL.ByteString)  -- ^ an even even more informative arbitrary HTTP response code error.
  deriving (Eq, Ord, Show)

instance Monoid RouteMismatch where
  mempty = NotFound
  -- The following isn't great, since it picks @InvalidBody@ based on
  -- alphabetical ordering, but any choice would be arbitrary.
  --
  -- "As one judge said to the other, 'Be just and if you can't be just, be
  -- arbitrary'" -- William Burroughs
  mappend = max

data ReqBodyState = Uncalled
                  | Called !B.ByteString
                  | Done !B.ByteString

toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  reqBodyRef <- newIORef Uncalled
  -- We may need to consume the requestBody more than once.  In order to
  -- maintain the illusion that 'requestBody' works as expected,
  -- 'ReqBodyState' is introduced, and the complete body is memoized and
  -- returned as many times as requested with empty "Done" marker chunks in
  -- between.
  -- See https://github.com/haskell-servant/servant/issues/3
  let memoReqBody = do
          ior <- readIORef reqBodyRef
          case ior of
            Uncalled -> do
                r <- BL.toStrict <$> strictRequestBody request
                writeIORef reqBodyRef $ Done r
                return r
            Called bs -> do
                writeIORef reqBodyRef $ Done bs
                return bs
            Done bs -> do
                writeIORef reqBodyRef $ Called bs
                return B.empty

  ra request{ requestBody = memoReqBody } routingRespond
 where
  routingRespond :: RouteResult Response -> IO ResponseReceived
  routingRespond (Retriable err)    = respond . responseServantErr $! unSEPrio err
  routingRespond (NonRetriable err) = respond . responseServantErr $! unSEPrio err
  routingRespond (HandlerVal v)     = respond v

runAction :: IO (RouteResult (ExceptT ServantErr IO a))
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action respond k = action >>= go >>= respond
  where
    go (Retriable  e)   = return $! Retriable e
    go (NonRetriable e) = return . succeedWith . responseServantErr $! unSEPrio e
    go (HandlerVal a)   = do
      e <- runExceptT a
      case e of
        Left err -> return . succeedWith . responseServantErr $! err
        Right x  -> return $! k x

feedTo :: IO (RouteResult (a -> b)) -> a -> IO (RouteResult b)
feedTo f x = (($ x) <$>) <$> f

extractL :: RouteResult (a :<|> b) -> RouteResult a
extractL (HandlerVal (a :<|> _)) = HandlerVal a
extractL (Retriable x)           = Retriable x
extractL (NonRetriable x)        = NonRetriable x

extractR :: RouteResult (a :<|> b) -> RouteResult b
extractR (HandlerVal (_ :<|> b)) = HandlerVal b
extractR (Retriable x)           = Retriable x
extractR (NonRetriable x)        = NonRetriable x

failWith :: ServantErr -> RouteResult a
failWith = Retriable . ServantErrWithPriority

succeedWith :: a -> RouteResult a
succeedWith = HandlerVal

isMismatch :: RouteResult a -> Bool
isMismatch (Retriable _) = True
isMismatch _             = False
