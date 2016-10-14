module ARD.Bitmap
  ( writeBitmapToFile
  ) where

import ARD.Color
import Data.Bits
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as L

data BitmapFileHeader
  = BitmapFileHeader
    { signature :: Word16
    , fileSize :: Word32
    , reserved1 :: Word16
    , reserved2 :: Word16
    , fileOffset :: Word32
    }

data BitmapInfoHeader
  = BitmapInfoHeader
    { headerSize :: Word32
    , bitmapWidth :: Word32
    , bitmapHeight :: Word32
    , numColorPlanes :: Word16
    , bitsPerPixel :: Word16
    , compressionMethod :: Word32
    , rawImageSize :: Word32
    , horizontalResolution :: Word32
    , verticalResolution :: Word32
    , numColorsInColorPalette :: Word32
    , numImportantColors :: Word32
    }

class Encodeable a where
  encode :: a -> [Word8]

instance Encodeable Word16 where
  encode value = map fromIntegral [ value .&. 0xff
                                  , (shiftR value 8) .&. 0xff
                                  ]

instance Encodeable Word32 where
  encode value = map fromIntegral [ value .&. 0xff
                                  , (shiftR value 8) .&. 0xff
                                  , (shiftR value 16) .&. 0xff
                                  , (shiftR value 24) .&. 0xff
                                  ]

instance Encodeable BitmapFileHeader where
  encode value = concat [ encode $ signature value
                        , encode $ fileSize value
                        , encode $ reserved1 value
                        , encode $ reserved2 value
                        , encode $ fileOffset value
                        ]

instance Encodeable BitmapInfoHeader where
  encode value = concat [ encode $ headerSize value
                        , encode $ bitmapWidth value
                        , encode $ bitmapHeight value
                        , encode $ numColorPlanes value
                        , encode $ bitsPerPixel value
                        , encode $ compressionMethod value
                        , encode $ rawImageSize value
                        , encode $ horizontalResolution value
                        , encode $ verticalResolution value
                        , encode $ numColorsInColorPalette value
                        , encode $ numImportantColors value
                        ]

neededRowPadding :: Int -> Int
neededRowPadding width = (4 - (width `mod` 4)) `mod` 4

encodePixelData [] _ = []
encodePixelData pixels width =
  let rowPixels = concat $ map (take 3 . encode . encodeColorToRGBWord32) $ take width pixels
      padding = take (neededRowPadding $ length rowPixels) [0,0,0,0]
  in rowPixels ++ padding ++ encodePixelData (drop width pixels) width

writeBitmapToFile :: Int -> Int -> [Color] -> FilePath -> IO ()
writeBitmapToFile width height pixels filepath =
  do handle <- openFile filepath WriteMode
     L.hPut handle $ L.pack $ encode fileHeader
     L.hPut handle $ L.pack $ encode infoHeader
     L.hPut handle $ L.pack $ encodePixelData pixels width
     hClose handle
  where
    rawImageSize = (width * 3 + neededRowPadding width) * height
    headerSize = 14 + 40
    fileSize = headerSize + rawImageSize
    fileHeader = BitmapFileHeader {
      signature = 0x4d42,
      fileSize = fromIntegral fileSize,
      reserved1 = 0,
      reserved2 = 0,
      fileOffset = fromIntegral headerSize
    }
    infoHeader = BitmapInfoHeader
      { headerSize = 40
      , bitmapWidth = fromIntegral width
      , bitmapHeight = fromIntegral height
      , numColorPlanes = 1
      , bitsPerPixel = 24
      , compressionMethod = 0
      , rawImageSize = fromIntegral rawImageSize
      , horizontalResolution = 0
      , verticalResolution = 0
      , numColorsInColorPalette = 0
      , numImportantColors = 0
      }


