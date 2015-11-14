{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Data.Word
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Binary.Get
import Control.Monad.RWS.Lazy

data Canvas = Canvas { cvWidth         :: !Int
                     , cvHeight        :: !Int
                     , cvBgColorIdx    :: !Int
                     , cvPxAspectRatio :: !Double
                     , cvColorTable    :: !(Maybe ColorTable)
                     }
            deriving Show

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
               deriving Show

data LScreenDesc = LSD { lsdHasCT    :: !Bool
                       , lsdColorRes :: !Int
                       , lsdIsDesc   :: !Bool
                       , lsdCTSize   :: !Int
                       }
                 deriving Show

data GfxControlExt = GCE { gceDisposal  :: !Int
                         , gceUserInput :: !Bool
                         , gceHasTransp :: !Bool
                         , gceDelayTime :: !Int
                         , gceTranspIdx :: !Int
                         }
                   deriving Show

data CodeTable = CodeTable { ctCodeSize  :: !Int
                           , ctMaxCode   :: !Word16
                           , ctClearCode :: !Word16
                           , ctEOI       :: !Word16
                           , ctCodes     :: !(V.Vector [Int])
                           }
               deriving Show

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
                  deriving Show

dispatchByte :: Word8 -> DispatchByte
dispatchByte 0x2C = ImageSeparator
dispatchByte 0x21 = ExtensionIntroducer
dispatchByte 0xF9 = GfxControlLabel
dispatchByte 0x3B = EndOfGif
dispatchByte b    = UnknownDispatch b

data ParseEnv = ParseEnv { peColorTableSize :: !Int
                         , peMinCodeSize    :: !Int
                         }
                deriving Show

data DataSegment = ImageData { imgDatDesc   :: !ImageDesc
                             , imgDatColors :: !(V.Vector Color)
                             }
                 | PlainTextData
                 | ApplicationData
                 | CommentData
                 deriving Show

type ImageDataParser = RWST ParseEnv [Int] CodeTable
