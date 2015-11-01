{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Text.Printf (printf)

import Types

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


parseGfxControlExt :: Get GfxControlExt
parseGfxControlExt = do
  skip 2 -- Gfx Control Label and Block size (always 0xF9, 0x04)
  packed <- getWord8
  delayTimeW <- getWord16le
  transpIdxW <- getWord8
  skip 1 -- Block terminator (0x00)
  let (disp, userInp, hasTransp) = decodeMeta packed
      delay = toI delayTimeW
      transpIdx = toI transpIdxW
  return $ GCE disp userInp hasTransp delay transpIdx
  where
    decodeMeta b = (getDisp b, getUserInp b, getTransp b)
    getDisp b = fromIntegral ((b .&. 0x1C) `shiftR` 2)
    getUserInp b = b .&. 0x02 /= 0
    getTransp b = b .&. 0x01 /= 0


extractImageHeaders :: Get (ImageDesc, Maybe GfxControlExt)
extractImageHeaders = do
  gfxControlRes <- parseGfxControlExt
  case gfxControlRes of
   Right gce -> do imgHeaders <- parseImageHeaders Nothing
                   return (imgHeaders, Just gce)
   Left byte -> do imgHeaders <- parseImageHeaders (Just byte)
                   return (imgHeadres, Nothing)

  
decodeImageData :: ImageDataParser ()

getDataSegments :: Canvas -> Get [DataSegment]
getDataSegments canvas = do
  byte <- getWord8
  getSegment (dispatchByte byte) []
  where
    getSegment EndOfGif segs = return segs
    getSegment ExtensionIntroducer segs = getWord8 >>= (\b -> getSegment b segs)
    getSegment GfxControlLabel segs = do gce <- parseGfxControlExt
                                      doImageDecode (Just gce)
    getSegment ImageSeparator segs = doImageDecode Nothing
    getSegment (UnknownDispatch b) = error $ printf "Unknown dispatch byte: 0x%02X" b
    doImageDecode mbGce = do (imgDesc, colorTab, codeTab) <- parseImageDesc
                             let parseState = ParseState codeTab mbGce imgDesc
                             (_, _, imgData) <- runRWST decodeImageData canvas parseState
                             return ((ImageDataSeg imgData) : segs)


parseGif :: Get [DataSegment]
parseGif = do
  validateHeader
  canvas <- decodeCanvas
  getDataSegments canvas

  
  
  

main :: IO ()
main = putStrLn "hi there"
