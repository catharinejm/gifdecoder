{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

import Data.Vector ((!), (!?))
import qualified Data.Vector as V

import Control.Monad.RWS.Lazy

import Data.List (sortBy)

import Data.Word
import Data.Bits
import Data.Binary.Get
import qualified Data.Binary.Bits.Get as Bits
import Data.Char (ord)
import Text.Printf (printf)

import Types

validateHeader :: Get ()
validateHeader = do
  header <- getByteString 6
  if header /= C.pack "GIF89a"
    then fail "Invalid header. Only GIF 89a supported"
    else return ()


parseLSD :: Word8 -> LScreenDesc
parseLSD lsdByte = LSD hasCT colorRes isDesc ctSize
  where
    hasCT = lsdByte .&. 0x80 /= 0
    colorRes = fromIntegral $ ((lsdByte .&. 0x70) `shiftR` 4) + 1
    isDesc = lsdByte .&. 0x08 /= 0
    ctSize = 1 `shift` (toI ((lsdByte .&. 0x07) + 1))


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
  return $ V.fromList (parseColorTable bytes [])
  where
    parseColorTable bytes colors = if BS.null bytes
                                   then reverse colors
                                   else let (hd, tl) = BS.splitAt 3 bytes in
                                         parseColorTable tl ((mkColor hd) : colors)
    mkColor bs = let [r,g,b] = BS.unpack bs in
                  Color r g b


parseGfxControlExt :: Get GfxControlExt
parseGfxControlExt = do
  skip 1 -- Block size (always 0x04)
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


parseImageDesc :: Get ImageDesc
parseImageDesc = do
  left <- getWord16le
  top <- getWord16le
  width <- getWord16le
  height <- getWord16le
  packed <- getWord8
  let hasColorTab = packed .&. 0x80 /= 0
      isInterlaced = packed .&. 0x40 /= 0
      colorTabSize = 1 `shift` (fromIntegral $ (packed .&. 0x07))
      imgDesc = ImageDesc (toI left) (toI top) (toI width) (toI height) isInterlaced
  if hasColorTab
    then do colorTab <- getColorTable colorTabSize
            return $ imgDesc (Just colorTab)
    else return $ imgDesc Nothing


orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing mb = mb
orElse ma      _  = ma


buildCodeTable :: ParseEnv -> CodeTable
buildCodeTable ParseEnv { peMinCodeSize, peColorTableSize } =
  CodeTable codeSize maxCode clearCode (clearCode+1) initialCodeTable
  where
    codeSize = peMinCodeSize + 1
    clearCode = 1 `shiftL` peMinCodeSize
    maxCode = (1 `shiftL` codeSize) - 1
    initialCodeTable = V.fromList $ map (\n -> [n]) [0..peColorTableSize-1]


increaseCodeSize :: CodeTable -> CodeTable
increaseCodeSize codeTable @ CodeTable { ctCodeSize, ctMaxCode, ctCodes } =
  codeTable { ctCodeSize = ctCodeSize + 1
            , ctMaxCode = (ctMaxCode `shiftL` 1) + 1
            }


getDataSegments :: Canvas -> Get [DataSegment]
getDataSegments canvas @ Canvas { cvColorTable } = do
  byte <- getWord8
  getSegment (dispatchByte byte) Nothing []
  where
    getSegment :: DispatchByte -> Maybe GfxControlExt -> [DataSegment] -> Get [DataSegment]
    getSegment EndOfGif gce segs = return segs
    getSegment ExtensionIntroducer gce segs = getWord8 >>= (\b -> getSegment (dispatchByte b) gce segs)
    getSegment GfxControlLabel _ segs = do gce <- parseGfxControlExt
                                           b <- getWord8
                                           getSegment (dispatchByte b) (Just gce) segs
    getSegment ImageSeparator gce segs =
      do imgDesc @ ImageDesc { imgColorTable } <- parseImageDesc
         minSize <- getWord8
         let colorTab = case imgColorTable `orElse` cvColorTable of
                         Just ct -> ct
                         Nothing -> fail "No color table found!"
             parseEnv = ParseEnv (V.length colorTab) (toI minSize)
             codeTab = buildCodeTable parseEnv
         (_, indices) <- execRWST decodeIndexStream parseEnv codeTab
         let colors = V.fromList (foldr (\idx acc -> (colorTab ! idx) : acc) [] indices)
         return ((ImageData imgDesc colors) : segs)
    getSegment (UnknownDispatch b) _ _ = fail $ printf "Unknown dispatch byte: 0x%02X" b


readCodes :: Maybe Int -> CodeReader ()
readCodes lastCode = do
  codeTable @ CodeTable { ctCodeSize } <- get
  code <- lift $ Bits.getWord16be ctCodeSize
  case codeMatch codeTable code of
   ClearCode -> do env <- ask
                   put (buildCodeTable env)
                   readCodes Nothing
   EndOfInputCode -> return ()
   _ -> getIndices (toI code) >> readCodes (Just $ toI code)
  where
    getIndices :: Int -> CodeReader ()
    getIndices code = do
      codeTable @ CodeTable { ctCodes } <- get
      case lastCode of
       Nothing -> let indices = (ctCodes ! code) in tell indices
       Just lc -> case ctCodes !? code of
                   Nothing -> do let lastIdxs @ (k:_) = ctCodes ! lc
                                     newIdxs = lastIdxs ++ [k]
                                 tell newIdxs
                                 put $ addCode codeTable newIdxs
                   Just idxs @ (k:_) -> do let lastIdxs = ctCodes ! lc
                                           tell idxs
                                           put $ addCode codeTable (lastIdxs ++ [k])

addCode :: CodeTable -> [Int] -> CodeTable
addCode codeTable @ CodeTable { ctCodes } indices =
  case codeMatch codeTable (fromIntegral $ V.length ctCodes) of
   ClearCode -> codeTable { ctCodes = V.concat [ctCodes, V.fromList [[],[],indices]] }
   MaxCode -> addCode (increaseCodeSize codeTable) indices
   _ -> codeTable { ctCodes = V.snoc ctCodes indices }


decodeIndexStream :: ImageDataParser ()
decodeIndexStream = do
  dataLen <- lift getWord8
  if dataLen == 0
    then return ()
    else do env <- ask
            codes <- get
            bytes <- lift $ getByteString (toI dataLen)
            let (newCodes, indices) =
                  runGet (Bits.runBitGet $ do
                             (_, nc, idxs) <- runRWST (readCodes Nothing) env codes
                             return (nc, idxs))
                  (BSL.fromStrict bytes)
            put newCodes
            tell indices
            decodeIndexStream


parseGif :: Get (Canvas, [DataSegment])
parseGif = do
  validateHeader
  canvas <- decodeCanvas
  segments <- getDataSegments canvas
  return (canvas, segments)


main :: IO ()
main = do
  case runGetOrFail parseGif image of
   Left (bs, off, err) -> do
     putStrLn err
     putStrLn $ show $ map ((printf "0x%02X")::Word8->String) (BSL.unpack bs)
     putStrLn $ "Offset: " ++ show off
   Right (_, _, (canvas, segments)) -> do
     let (ImageData { imgDatColors, imgDatDesc } : _) = sortBy topLeft (filter isImage segments)
     putStrLn $ show imgDatColors
     let colorCount = length imgDatColors
         imageArea = (imgWidth imgDatDesc) * (imgHeight imgDatDesc)
     case compare imageArea colorCount of
      EQ -> putStrLn "Colors are correct!"
      GT -> putStrLn $ "Too few colors :( Expected: " ++ show imageArea ++ " Actual: " ++ show colorCount
      LT -> putStrLn $ "Too many colors! Expected: " ++ show imageArea ++ " Actual: " ++ show colorCount


  where
    isImage (ImageData _ _) = True
    isImage _               = False
    topLeft
      ImageData { imgDatDesc = i1 }
      ImageData { imgDatDesc = i2 } =
        compare (imgTop i1, imgLeft i1) (imgTop i2, imgLeft i2)


image :: BSL.ByteString
image = BSL.pack [ 0x47, 0x49, 0x46, 0x38, 0x39, 0x61
                 , 0x0A, 0x00, 0x0A, 0x00, 0x91, 0x00
                 , 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00
                 , 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00
                 , 0x00, 0x21, 0xF9, 0x04, 0x00, 0x00
                 , 0x00, 0x00, 0x00, 0x2C, 0x00, 0x00
                 , 0x00, 0x00, 0x0A, 0x00, 0x0A, 0x00
                 , 0x00, 0x02, 0x16, 0x8C, 0x2D, 0x99
                 , 0x87, 0x2A, 0x1C, 0xDC, 0x33, 0xA0
                 , 0x02, 0x75, 0xEC, 0x95, 0xFA, 0xA8
                 , 0xDE, 0x60, 0x8C, 0x04, 0x91, 0x4C
                 , 0x01, 0x00, 0x3B
                 ]
