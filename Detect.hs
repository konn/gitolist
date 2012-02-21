{-# LANGUAGE OverloadedStrings #-}
module Detect (detectEncodingName, detectEncoding) where
import Data.ByteString hiding (any, map, unzip, elem, maximum)
import Data.ByteString.Char8 ()
import Data.Maybe
import Control.Applicative
import Data.Word
import Data.Attoparsec
import GHC.IO.Encoding
import Data.Traversable

import Prelude hiding (head, tail, init, take, drop, length, all, null)

detectEncoding :: ByteString -> IO (Maybe TextEncoding)
detectEncoding bs = sequenceA (mkTextEncoding <$> detectEncodingName bs)

detectEncodingName :: ByteString -> Maybe String
detectEncodingName bs
    | "\xEF\xBB\xBF" `isPrefixOf` bs     = Just "UTF-8" -- UTF-8 BOM
    | "\xFE\xFF\x00\x00" `isPrefixOf` bs = Just "X-ISO-10646-UCS-4-3412"
    | "\xFE\xFF" `isPrefixOf` bs         = Just "UTF-16BE"
    | "\x00\x00\xFE\xFF" `isPrefixOf` bs = Just "UTF-32BE"
    | "\x00\x00\xFF\xFE" `isPrefixOf` bs = Just "X-ISO-10646-UCS-4-2143"
    | "\xFF\xFE\x00\x00" `isPrefixOf` bs = Just "UTF-32LE"
    | "\xFF\xFE" `isPrefixOf` bs         = Just "UTF-16LE"
    | null bs                            = Nothing
    | isJIS bs  = Just "ISO-2022-JP"
    | otherwise =
        let euc  = ptsEUC bs
            sjis = ptsSJIS bs
            utf8 = ptsUTF8 bs
        in if euc > sjis && euc > utf8
           then Just "EUC-JP"
           else if (sjis > euc && sjis > utf8)
           then Just "Shift_JIS"
           else if (utf8 > euc && utf8 > sjis)
           then Just "UTF-8"
           else Nothing

isJIS :: ByteString -> Bool
isJIS bs= any (`isInfixOf` bs)
            ["\x1B$@", "\x1B$B", "\x1B(B", "\x1B(J", "\x1B(I"
            ,"\x1B$(D", "\x1B&@\x1B$B"]

ptsSJIS :: ByteString -> Integer
ptsSJIS = either error sum . parseOnly (many ptSJIS)

ptSJIS :: Parser Integer
ptSJIS = skipWhile (\w -> (w < 0x81 || 0x9F < w) && (w < 0xE0 || 0xFC < w)) >> body
  where
    body = 2 <$ try (inRange (0x81, 0x9F) <|> inRange (0xE0, 0xFC) >> (inRange (0x40,0x7E) <|> inRange (0x80,0xFC)))
                

inRange :: (Word8, Word8) -> Parser Word8
inRange (from, to) = satisfy $ \w -> from <= w && w <= to

ptsEUC :: ByteString -> Integer
ptsEUC = either error sum . parseOnly (many ptEUC) 
ptEUC = skipWhile garbage *> body
  where
    garbage w = (w /= 0x8F) && (w < 0xA1 || 0xFE < w) && (w /= 0x8E)
    body = 3 <$ try (word8 0x8F >> inRange (0xA1, 0xFE) >> inRange (0xA1, 0xFE))
       <|> 2 <$ try (inRange (0xA1, 0xFE) >> inRange (0xA1, 0xFE))
       <|> 2 <$ try (word8 0x8E >> inRange (0xA1, 0xDF))

ptsUTF8 :: ByteString -> Integer
ptsUTF8 = either error sum . parseOnly (many ptUTF8)
ptUTF8 = skipWhile garbage *> body
  where
    garbage w = (w < 0xE0 || 0xEF < w) && (w < 0xC0 || 0xDF < w)
    body = 3 <$ try (inRange (0xE0, 0xEF) >> inRange (0x80, 0xBF) >> inRange (0x80, 0xBF))
       <|> 2 <$ try (inRange (0xC0, 0xDF) >> inRange (0x80, 0xBF))
