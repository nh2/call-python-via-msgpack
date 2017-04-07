{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.MessagePack as M
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))
import           System.IO (stdout, stderr, hPutStrLn)

import qualified CPython as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.System as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.Exception as Py
import qualified CPython.Types.Module as Py

import           FFI.Python.MessagePack

main :: IO ()
main = do
  cwd <- getCurrentDirectory

  Py.initialize

  E.handle onPyExceptionPrint $ do

    pythonpath <- Py.getPath
    T.putStrLn ("Python path at startup is: " <> pythonpath <> "\n")
    -- Appending so that the user's PYTHONPATH variable (ready by python) can go first.
    let updatedPytonpath = pythonpath <> ":/usr/lib/python2.7/dist-packages:."
    T.putStrLn ("Setting Python path to: " <> updatedPytonpath <> "\n")
    Py.setPath updatedPytonpath

    examplemodule <- Py.importModule "exampleparentmodule.examplemodule27"
    f1 <- Py.getAttribute examplemodule =<< Py.toUnicode "f1"
    res <- Py.callArgs f1 []

    restype <- Py.getType res
    putStr "Type of result of f1: "
    Py.print restype stdout
    putStr "Result of f1:         "
    Py.print res stdout

    putStrLn ""

    -- Explicit interface

    resMsgpack <- callViaSerialization
      "exampleparentmodule.examplemodule27"
      "f2"
      (encode ("hello" :: String, "world" :: ByteString))
      (encode (M.ObjectMap [ (M.toObject ("stringKey" :: Text), M.toObject ("value" :: ByteString))
                           , (M.toObject ("intKey" :: Text), M.toObject (1 :: Int))
                           ]))

    print (decode resMsgpack :: Maybe String)

    -- Prettier interface:

    putStrLn ""

    let f2 = callViaSerializationPretty "exampleparentmodule.examplemodule27" "f2"

    res <- f2 ("hello" :: String, "world" :: ByteString)
      [ "stringKey" =: ("value" :: ByteString)
      , "intKey" =: (1 :: Int)
      ]

    print (res :: String)

  return ()


onPyExceptionPrint :: Py.Exception -> IO ()
onPyExceptionPrint exc = do
  hPutStrLn stderr "Got python exception:"

  hPutStrLn stderr "Exception type:"
  Py.print (Py.exceptionType exc) stderr

  hPutStrLn stderr "Exception value:"
  -- Py.print (Py.exceptionValue exc) stderr
  case Py.exceptionValue exc of
    Nothing -> hPutStrLn stderr "(none)"
    Just value -> Py.print value stderr

  hPutStrLn stderr "Exception traceback:"
  case Py.exceptionTraceback exc of
    Nothing -> hPutStrLn stderr "(none)"
    Just traceback -> Py.print traceback stderr

  hPutStrLn stderr "End of python exception"
