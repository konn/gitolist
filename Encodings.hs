{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Encodings ( encode, decode, TextEncoding
                 , mkTextEncoding, localeEncoding
                 , detectEncoding
                 ) where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Text.Detect as D
import qualified GHC.IO.Encoding as E
import GHC.IO.Encoding hiding (encode)
import GHC.IO.Buffer
import Foreign.Ptr
import Foreign.C.String
import Foreign.ForeignPtr
import Data.Word
import Control.Exception
import System.IO.Unsafe
import Foreign.Marshal.Array
import Prelude
  
detectEncoding :: BS.ByteString -> Maybe TextEncoding
detectEncoding bs = unsafePerformIO $ D.detectEncoding $ LBS.fromChunks [bs]

bufToBS :: Buffer Word8 -> IO BS.ByteString
bufToBS b@(Buffer raw _ c l r) = withBuffer b $ \ptr -> do
  BS.packCStringLen (ptr `plusPtr` l, r - l)

bsToBuffer :: BS.ByteString -> IO (Buffer Word8)
bsToBuffer bs = BS.useAsCStringLen bs $ \(ptr, len) -> do
  fPtr <- newForeignPtr_ (castPtr ptr)
  return $ Buffer fPtr ReadBuffer len 0 len

strToBuffer :: String -> IO CharBuffer
strToBuffer str = do
  ptr <- newArray str
  fptr <- newForeignPtr_ ptr
  let len = length str
  return $ Buffer fptr ReadBuffer len 0 len

bufToString :: CharBuffer -> IO String
bufToString b@(Buffer raw _ c l r) = withBuffer b $ \ptr ->
  peekCWStringLen (castPtr ptr `plusPtr` l, r - l)  

decode :: TextEncoding -> BS.ByteString -> String
decode (TextEncoding eName dec _) bs = unsafePerformIO $ do
  bracket dec close $ \decoder -> do
    fromBuf <- bsToBuffer bs
    toBuf   <- newCharBuffer (BS.length bs) WriteBuffer
    (rem, new) <- E.encode decoder fromBuf toBuf
    bufToString new

encode :: TextEncoding -> String -> BS.ByteString
encode (TextEncoding eName _ enc) str = unsafePerformIO $ do
  bracket enc close $ \encoder -> do
    fromBuf <- strToBuffer str
    toBuf   <- newByteBuffer (length str * 3) WriteBuffer
    (rem, new) <- E.encode encoder fromBuf toBuf
    bufToBS new
