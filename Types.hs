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

data GfxControlExt = GCE { gceDisposal  :: !Int
                         , gceUserInput :: !Bool
                         , gceHasTransp :: !Bool
                         , gceDelayTime :: !Int
                         , gceTranspIdx :: !Int
                         }

type GIFParser = RWST Canvas BS.ByteString Image Get
