module Cardano.Types.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , buildRequest
  , mkCallType
  , JsonRpc2Response
  , decodeAesonJsonRpc2Response
  ) where

import Prelude

import Aeson
  ( class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  , getFieldOptional
  )
import Cardano.Types.UniqueId (ListenerId, uniqueId)
import Data.Either (Either(Left))
import Data.Maybe (Maybe)
import Effect (Effect)
import Record as Record

-- | Structure of all json rpc2.0 websocket requests
-- described in:  https://ogmios.dev/getting-started/basics/
type JsonRpc2Request (a :: Type) =
  { jsonrpc :: String
  , method :: String
  , params :: a
  , id :: ListenerId
  }

-- | Convenience helper function for creating `JsonRpc2Request a` objects
mkJsonRpc2Request
  :: forall (a :: Type)
   . { jsonrpc :: String }
  -> { method :: String
     , params :: a
     }
  -> Effect (JsonRpc2Request a)
mkJsonRpc2Request service method = do
  id <- uniqueId $ method.method <> "-"
  pure
    $ Record.merge { id }
    $ Record.merge service method

-- | Structure of all json rpc websocket responses
-- described in: https://ogmios.dev/getting-started/basics/
type JsonRpc2Response =
  { jsonrpc :: String
  -- methodname is not always present if `error` is not empty
  , method :: Maybe String
  , result :: Maybe Aeson
  , error :: Maybe Aeson
  , id :: ListenerId
  }

decodeAesonJsonRpc2Response
  :: Aeson -> Either JsonDecodeError JsonRpc2Response
decodeAesonJsonRpc2Response = caseAesonObject (Left (TypeMismatch "Object"))
  $ \o -> do
      jsonrpc <- getField o "jsonrpc"
      method <- getFieldOptional o "method"
      result <- getFieldOptional o "result"
      error <- getFieldOptional o "error"
      id <- getField o "id"
      pure
        { jsonrpc
        , method
        , result
        , error
        , id
        }

-- | A wrapper for tying arguments and response types to request building.
newtype JsonRpc2Call :: Type -> Type -> Type
newtype JsonRpc2Call (i :: Type) (o :: Type) = JsonRpc2Call
  (i -> Effect { body :: Aeson, id :: String })

-- | Creates a "jsonrpc call" which ties together request input and response output types
-- | along with a way to create a request object.
mkCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => { jsonrpc :: String }
  -> { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkCallType service { method, params } = JsonRpc2Call \i -> do
  req <- mkJsonRpc2Request service { method, params: params i }
  pure { body: encodeAeson req, id: req.id }

-- | Create a JsonRpc2 request body and id
buildRequest
  :: forall (i :: Type) (o :: Type)
   . JsonRpc2Call i o
  -> i
  -> Effect { body :: Aeson, id :: String }
buildRequest (JsonRpc2Call c) = c