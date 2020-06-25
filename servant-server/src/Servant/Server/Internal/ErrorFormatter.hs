{-# LANGUAGE DataKinds #-}

-- | TODO: more documentation for this module
module Servant.Server.Internal.ErrorFormatter
  where

import           Data.String.Conversions
                 (cs)
import           Data.Typeable
import           Network.Wai.Internal
                 (Request)

import           Servant.API
                 (Capture, ReqBody)
import           Servant.Server.Internal.Context
import           Servant.Server.Internal.ServerError

-- | 'Context' that contains default error formatters.
--
-- Default formatters will just return HTTP 400 status code with error
-- message as response body.
type DefaultErrorFormatters = '[ErrorFormatters]

-- | TODO doc
data ErrorFormatters = ErrorFormatters
  { bodyParserErrorFormatter :: ErrorFormatter
  , urlParseErrorFormatter :: ErrorFormatter
  , headerParseErrorFormatter :: ErrorFormatter
  , notFoundErrorFormatter :: NotFoundErrorFormatter
  }

defaultErrorFormatters :: ErrorFormatters
defaultErrorFormatters = ErrorFormatters
  { bodyParserErrorFormatter = err400Formatter
  , urlParseErrorFormatter = err400Formatter
  , headerParseErrorFormatter = err400Formatter
  , notFoundErrorFormatter = const err404
  }

-- | A custom formatter for errors produced by parsing combinators like
-- 'ReqBody' or 'Capture'.
--
-- A 'TypeRep' argument described the concrete combinator that raised
-- the error, allowing formatter to customize the message for different
-- combinators.
--
-- A full 'Request' is also passed so that the formatter can react to @Accept@ header,
-- for example.
type ErrorFormatter = TypeRep -> Request -> String -> ServerError

-- | This formatter does not get neither 'TypeRep' nor error message.
type NotFoundErrorFormatter = Request -> ServerError

-- Internal

err400Formatter :: ErrorFormatter
err400Formatter _ _ e = err400 { errBody = cs e }

-- These definitions suppress "unused import" warning.
-- The imorts are needed for Haddock to correctly link to them.
_RB :: Proxy ReqBody
_RB = undefined
_C :: Proxy Capture
_C = undefined
_CT :: Proxy Context
_CT = undefined
