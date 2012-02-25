{-# LANGUAGE CPP #-}
module Encodings ( encode, decode, TextEncoding
                 , mkTextEncoding, localeEncoding
                 , detectEncoding
                 ) where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
#ifdef charsetdetect
import qualified Codec.Text.Detect as D
#else
import qualified Detect as D
#endif
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
import Control.Monad

detectEncoding :: BS.ByteString -> Maybe TextEncoding
#ifdef charsetdetect
detectEncoding bs = unsafePerformIO $ D.detectEncoding $ LBS.fromChunks [bs]
#else
detectEncoding bs = unsafePerformIO $ D.detectEncoding bs
#endif

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

decode :: TextEncoding -> BS.ByteString -> Maybe String
decode (TextEncoding eName dec _) bs = unsafePerformIO $ do
  bracket dec close $ \decoder -> do
    fromBuf <- bsToBuffer bs
    toBuf   <- newCharBuffer (BS.length bs) WriteBuffer
    (rem, new) <- E.encode decoder fromBuf toBuf
    if (isEmptyBuffer rem)
       then Just `fmap` bufToString new
       else return Nothing

encode :: TextEncoding -> String -> Maybe  BS.ByteString
encode (TextEncoding eName _ enc) str = unsafePerformIO $ do
  bracket enc close $ \encoder -> do
    fromBuf <- strToBuffer str
    toBuf   <- newByteBuffer (length str * 3) WriteBuffer
    (rem, new) <- E.encode encoder fromBuf toBuf
    if isEmptyBuffer rem
       then Just `fmap` bufToBS new
       else return Nothing
