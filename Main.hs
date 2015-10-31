{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Vector as V

import Control.Monad.RWS.Lazy

import Data.Word
import Data.Bits
import Data.Binary.Get hiding (getBytes)
import Data.Char (ord)

data Canvas = Canvas { cvWidth         :: !Int
                     , cvHeight        :: !Int
                     , cvBgColorIdx    :: !Int
                     , cvPxAspectRatio :: !Double
                     , cvColorTable    :: Maybe ColorTable
                     }
data Color = Color { colorRed   :: !Word8
                   , colorGreen :: !Word8
                   , colorBlue  :: !Word8
                   }
type ColorTable = V.Vector Color

data Image = Image { imgLeft         :: !Int
                   , imgTop          :: !Int
                   , imgWidth        :: !Int
                   , imgHeight       :: !Int
                   , imgIsInterlaced :: !Bool
                   , imgColorTable   :: Maybe ColorTable
                   }

data LScreenDesc = LSD { lsdHasCT    :: !Bool
                       , lsdColorRes :: !Int
                       , lsdIsDesc   :: !Bool
                       , lsdCTSize   :: !Int
                       }
                   
type GIFParser = RWST Canvas BS.ByteString Image Get

imageSeparator :: Word8
imageSeparator = 0x2C


validateHeader :: Get ()
validateHeader = do
  header <- getByteString 6
  if header /= C.pack "GIF89a"
    then error "Invalid header. Only GIF 89a supported"
    else return ()


parseLSD :: Word8 -> LScreenDesc
parseLSD lsdByte = LSD hasCT colorRes isDesc ctSize
  where
    hasCT = lsdByte .&. 0x80 /= 0
    colorRes = fromIntegral $ ((lsdByte .&. 0x70) `shiftR` 4) + 1
    isDesc = lsdByte .&. 0x08 /= 0
    ctSize = 1 `shift` (fromIntegral (lsdByte .&. 0x07))
  
toI :: Integral i => i -> Int
toI = fromIntegral

decodeCanvas :: Get Canvas
decodeCanvas = do
  width <- getWord16le
  height <- getWord16le
  meta <- getWord8
  let LSD hasCT _ _ ctSize = parseLSD meta
  bgColorIdx <- getWord8
  pxAspectRatioByte <- getWord8
  let w = toI width
      h = toI height
      bgc = toI bgColorIdx
      pxAR = (15.0 + fromIntegral pxAspectRatioByte) / 64
  if hasCT
    then do colorTable <- getColorTable ctSize
            return $ Canvas w h bgc pxAR (Just colorTable)
    else return $ Canvas w h bgc pxAR Nothing
    

getColorTable :: Int -> Get ColorTable
getColorTable n = do
  bytes <- getByteString (n * 3)
  return $ V.fromList (parseColorTable bytes)
  where
    parseColorTable bytes = map mkColor (groupsOf 3 bytes)
    mkColor bs = let [r,g,b] = BS.unpack bs in
                  Color r g b

groupsOf :: Int -> BS.ByteString -> [BS.ByteString]
groupsOf n bs = map fst splits
  where
    splits :: [(BS.ByteString, BS.ByteString)]
    splits = tail $ iterate mkSplits (BS.empty, bs)
    mkSplits (_, rem) = BS.splitAt n rem

-- decodeGif :: Get Word8
-- decodeGif = do
--   validateHeader
--   canvas <- decodeCanvas

main :: IO ()
main = putStrLn "hi there"
