{-# LANGUAGE OverloadedStrings #-}

module FFI.Python.MessagePack
  ( callViaSerialization
  , callViaSerializationPretty
  , (=:)
  , encode
  , decode
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.MessagePack as M
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import qualified CPython.Protocols.Object as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.Module as Py


callViaSerialization :: Text -> Text -> ByteString -> ByteString -> IO ByteString
callViaSerialization moduleName functionName msgpackArgs msgpackKwargs = do
  msgpackModule <- Py.importModule "msgpack"
  unpackb <- Py.getAttribute msgpackModule =<< Py.toUnicode "unpackb"
  packb <- Py.getAttribute msgpackModule =<< Py.toUnicode "packb"

  pyModule <- Py.importModule moduleName
  pyFunction <- Py.getAttribute pyModule =<< Py.toUnicode functionName

  pyMsgpackArgs <- Py.toBytes msgpackArgs
  pyMsgpackKwargs <- Py.toBytes msgpackKwargs

  unpackbSettingsKwargs <- PyDict.new
  encodingText <- Py.toUnicode "encoding"
  utf8Text <- Py.toUnicode "utf-8"
  PyDict.setItem unpackbSettingsKwargs encodingText utf8Text

  unpackbArgsForArgs <- Py.toTuple [Py.toObject pyMsgpackArgs]
  unpackbArgsForKwargs <- Py.toTuple [Py.toObject pyMsgpackKwargs]

  pyArgsListObject <- Py.call unpackb unpackbArgsForArgs unpackbSettingsKwargs
  pyKwargsDictObject <- Py.call unpackb unpackbArgsForKwargs unpackbSettingsKwargs

  pyArgsTuple <- Py.iterableToTuple pyArgsListObject
  Just pyKwargsDict <- Py.cast pyKwargsDictObject

  pyResult <- Py.call pyFunction pyArgsTuple pyKwargsDict

  pyMsgpackResultObject <- Py.callArgs packb [pyResult]
  Just pyMsgpackResult <- Py.cast pyMsgpackResultObject
  msgpackResult <- Py.fromBytes pyMsgpackResult
  return msgpackResult

encode :: (M.MessagePack a) => a -> ByteString
encode = BSL.toStrict . M.pack

decode :: (M.MessagePack a) => ByteString -> Maybe a
decode = M.unpack . BSL.fromStrict

toArgs :: (M.MessagePack a) => a -> ByteString
toArgs = encode

toKwargs :: [(Text, M.Object)] -> ByteString
toKwargs l = encode (M.ObjectMap [ (M.toObject keyText, obj) | (keyText, obj) <- l ])

callViaSerializationPretty :: (M.MessagePack argsTuple, M.MessagePack result) => Text -> Text -> argsTuple -> [(Text, M.Object)] -> IO result
callViaSerializationPretty moduleName functionName args kwargs = do
  resBs <- callViaSerialization moduleName functionName (toArgs args) (toKwargs kwargs)
  case decode resBs of
    Nothing -> error $ "callViaSerializationPretty: Call to Python function '" ++ T.unpack (moduleName <> "." <> functionName) ++ " did not return a valid MessagePack object"
    Just x -> return x

(=:) :: (M.MessagePack a) => Text -> a -> (Text, M.Object)
key =: value = (key, M.toObject value)
