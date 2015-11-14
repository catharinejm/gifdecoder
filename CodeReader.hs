{-# LANGUAGE NamedFieldPuns #-}

module CodeReader where

import Data.Binary.Get
import Data.Word
import Data.Bits

import Control.Monad.State.Lazy

data BitReader = BitReader { brByte    :: !Word8
                           , brBitCnt  :: !Int
                           }
               deriving (Show)

type CodeReader = StateT BitReader Get Word16

getCode :: Int -> CodeReader
getCode n = do
  BitReader { brByte, brBitCnt } <- get
  if n <= brBitCnt
    then do let mask = (1 `shiftL` n) - 1
                res = fromIntegral $ brByte .&. mask
            modify consumeBits
            return res
    else do tl <- getCode brBitCnt
            nextBy <- lift $ getWord8
            put $ BitReader nextBy 8
            hd <- getCode (n - brBitCnt)
            return $ (hd `shiftL` brBitCnt) .|. tl
  where
    consumeBits br = br { brBitCnt = (brBitCnt br) - n
                        , brByte = (brByte br) `shiftR` n
                        }
