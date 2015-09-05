-- | This module is for reading and parsing an IRC-log produced by irssi and turning it into something useful.
{-# LANGUAGE OverloadedStrings #-}

module ParseLog (parseLog) where

import Control.Applicative (many)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as Convert

-- | Parse and return a message in the IRC-log.
messageParser :: AP.Parser BS.ByteString
messageParser = do
  let digits = AP.satisfy $ AP.inClass "0-9"
  -- Skip the timestamp and the nick.
  AP.count 2 digits 
  AP.string ":"
  AP.count 2 digits 
  AP.string " <"
  AP.skipWhile (/= 62) -- > == 62
  AP.string "> "

  -- Here comes the message!
  -- Lets consume it and return it.
  AP.takeWhile1 (/= 10) 

-- | This parser devours a line, never to be seen again.
eatLine :: AP.Parser BS.ByteString
eatLine = AP.skipWhile (/= 10) >> return ""

-- | Parse a row of the IRC-log.
rowParser :: AP.Parser BS.ByteString
rowParser = (AP.choice [messageParser, eatLine]) <* endOfLine
  where
    endOfLine = AP.satisfy (\w -> w == 10 || w == 13)

-- | Decode from latin-1 to Unicode.
decodeLatin1 :: BS.ByteString -> IO T.Text
decodeLatin1 bs = do
  conv <- Convert.open "latin-1" Nothing
  return $ Convert.toUnicode conv bs

-- | Parse a log file to a list of rows.
parseLog :: FilePath -> IO [T.Text]
parseLog fp = do
  result <- (AP.parseOnly (many rowParser)) <$> BS.readFile fp
  case result of
    Left  s  -> error s
    Right xs -> mapM decodeLatin1 $ filter (not . BS.null) xs 

