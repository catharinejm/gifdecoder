{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Data.Word
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Binary.Bits.Get
import Control.Monad.RWS.Lazy

data Canvas = Canvas { cvWidth         :: !Int
                     , cvHeight        :: !Int
                     , cvBgColorIdx    :: !Int
                     , cvPxAspectRatio :: !Double
                     , cvColorTable    :: !(Maybe ColorTable)
                     }

data Color = Color { colorRed   :: !Word8
                   , colorGreen :: !Word8
                   , colorBlue  :: !Word8
                   }
           deriving Show

type ColorTable = V.Vector Color

data ImageDesc = ImageDesc { imgLeft         :: !Int
                           , imgTop          :: !Int
                           , imgWidth        :: !Int
                           , imgHeight       :: !Int
                           , imgIsInterlaced :: !Bool
                           , imgColorTable   :: !(Maybe ColorTable)
                           }

data LScreenDesc = LSD { lsdHasCT    :: !Bool
                       , lsdColorRes :: !Int
                       , lsdIsDesc   :: !Bool
                       , lsdCTSize   :: !Int
                       }

data GfxControlExt = GCE { gceDisposal  :: !Int
                         , gceUserInput :: !Bool
                         , gceHasTransp :: !Bool
                         , gceDelayTime :: !Int
                         , gceTranspIdx :: !Int
                         }

data CodeTable = CodeTable { ctCodeSize  :: !Int
                           , ctMaxCode   :: !Word16
                           , ctClearCode :: !Word16
                           , ctEOI       :: !Word16
                           , ctCodes     :: !(V.Vector [Word16])
                           }

data CodeMatch = MaxCode
               | ClearCode
               | EndOfInputCode
               | BasicCode
codeMatch :: CodeTable -> Word16 -> CodeMatch
codeMatch CodeTable { ctMaxCode   } code | code == ctMaxCode   = MaxCode
codeMatch CodeTable { ctClearCode } code | code == ctClearCode = ClearCode
codeMatch CodeTable { ctEOI       } code | code == ctEOI       = EndOfInputCode
codeMatch _                         _                          = BasicCode

data DispatchByte = ImageSeparator
                  | ExtensionIntroducer
                  | GfxControlLabel
                  | EndOfGif
                  | UnknownDispatch !Word8

dispatchByte :: Word8 -> DispatchByte
dispatchByte 0x2C = ImageSeparator
dispatchByte 0x21 = ExtensionIntroducer
dispatchByte 0xF9 = GfxControlLabel
dispatchByte 0x3B = EndOfGif
dispatchByte b    = UnknownDispatch b

data ParseEnv = ParseEnv { peBaseCodeTable :: !(V.Vector [Word16])
                         , peMinCodeSize   :: !Int
                         }

data DataSegment = ImageData { imgDatDesc   :: !ImageDesc
                             , imgDatColors :: !(V.Vector Color)
                             }
                 | PlainTextData
                 | ApplicationData
                 | CommentData

type CodeReader = RWST ParseEnv [Word16] CodeTable BitGet
type ImageDataParser = RWST ParseEnv [Word16] CodeTable Get
