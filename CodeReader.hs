{-# LANGUAGE NamedFieldPuns #-}

module CodeReader where

import Data.Binary.Get
import Data.Word
import Data.Bits

import Control.Monad.State.Lazy

import Types

readCode :: Int -> CodeReader Word16
readCode n = do
  BitReader { brByte, brBitCnt } <- get
  if n <= brBitCnt
    then do let mask = (1 `shiftL` n) - 1
                res = fromIntegral $ brByte .&. mask
            modify consumeBits
            return res
    else do tl <- readCode brBitCnt
            consumeByte
            hd <- readCode (n - brBitCnt)
            return $ (hd `shiftL` brBitCnt) .|. tl
  where
    consumeBits br = br { brBitCnt = (brBitCnt br) - n
                        , brByte = (brByte br) `shiftR` n
                        }
    consumeByte = do
      BitReader { brByteCnt } <- get
      if brByteCnt == 0
        then do dataLen <- lift getWord8
                if dataLen == 0
                  then fail "Encountered premature end of image data"
                  else modify (\br -> br { brByteCnt = fromIntegral dataLen }) >> consumeByte
        else do nextBy <- lift getWord8
                put $ BitReader nextBy 8 (brByteCnt - 1)

