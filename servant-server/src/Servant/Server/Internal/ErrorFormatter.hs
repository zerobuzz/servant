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

-- | 'Context' that contains default formatters for all error types.
--
-- Default formatters will just return HTTP 400 status code with error
-- message as response body.
type DefaultErrorFormatters = '[BodyParseErrorFormatter, URLParseErrorFormatter, HeaderParseErrorFormatter]

defaultErrorFormatters :: Context DefaultErrorFormatters
defaultErrorFormatters =
  defaulyBodyParseErrorFormatter
  :. defaultURLParseErrorFormatter
  :. defaultHeaderParseErrorFormatter
  :. EmptyContext

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

-- | Formatter for errors that occur while parsing request body.
newtype BodyParseErrorFormatter = BodyParseErrorFormatter
  { getBodyParseErrorFormatter :: ErrorFormatter
  }

defaulyBodyParseErrorFormatter :: BodyParseErrorFormatter
defaulyBodyParseErrorFormatter = BodyParseErrorFormatter err400Formatter

-- | Formatter for errors that occur while parsing URL parts, like 'Servant.API.Capture' or
-- 'Servant.API.QueryParam'.
newtype URLParseErrorFormatter = URLParseErrorFormatter
  { getUrlParseErrorFormatter :: ErrorFormatter
  }

defaultURLParseErrorFormatter :: URLParseErrorFormatter
defaultURLParseErrorFormatter = URLParseErrorFormatter err400Formatter

-- | Formatter for errors that occur while parsing HTTP headers.
newtype HeaderParseErrorFormatter = HeaderParseErrorFormatter
  { getHeaderParseErrorFormatter :: ErrorFormatter
  }

defaultHeaderParseErrorFormatter :: HeaderParseErrorFormatter
defaultHeaderParseErrorFormatter = HeaderParseErrorFormatter err400Formatter

-- Internal

err400Formatter :: ErrorFormatter
err400Formatter _ _ e = err400 { errBody = cs e }

-- These definitions suppress "unused import" warning.
-- The imorts are needed for Haddock to correctly link to them.
_RB :: Proxy ReqBody
_RB = undefined
_C :: Proxy Capture
_C = undefined
