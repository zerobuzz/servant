{-# LANGUAGE DataKinds #-}

-- | TODO: more documentation for this module
module Servant.Server.Internal.ErrorFormatter
  where

import           Data.String.Conversions
                 (cs)
import           Data.Typeable

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
-- 'Servant.API.ReqBody' or 'Servant.API.Capture'.
--
-- A 'TypeRep' argument described the concrete combinator that raised
-- the error, allowing formatter to customize the message for different
-- combinators.
type ErrorFormatter = TypeRep -> String -> ServerError

-- | Formatter for errors that occur while parsing request body.
newtype BodyParseErrorFormatter = BodyParseErrorFormatter
  { getBodyParseErrorFormatter :: ErrorFormatter
  }

defaulyBodyParseErrorFormatter :: BodyParseErrorFormatter
defaulyBodyParseErrorFormatter = BodyParseErrorFormatter defaultErrorFormatter

-- | Formatter for errors that occur while parsing URL parts, like 'Servant.API.Capture' or
-- 'Servant.API.QueryParam'.
newtype URLParseErrorFormatter = URLParseErrorFormatter
  { getUrlParseErrorFormatter :: ErrorFormatter
  }

defaultURLParseErrorFormatter :: URLParseErrorFormatter
defaultURLParseErrorFormatter = URLParseErrorFormatter defaultErrorFormatter

-- | Formatter for errors that occur while parsing HTTP headers.
newtype HeaderParseErrorFormatter = HeaderParseErrorFormatter
  { getHeaderParseErrorFormatter :: ErrorFormatter
  }

defaultHeaderParseErrorFormatter :: HeaderParseErrorFormatter
defaultHeaderParseErrorFormatter = HeaderParseErrorFormatter defaultErrorFormatter

-- Internal

defaultErrorFormatter :: ErrorFormatter
defaultErrorFormatter _ e = err400 { errBody = cs e }
