import System.Environment

import ARD.Bitmap

main :: IO ()
main = writeBitmapToFile 256 256 [0..(256*256-1)] "out.bmp"

