import ARD.Color
import ARD.Bitmap
import ARD.Parser (parseWorld)
import ARD.Tracer
import ARD.ViewPlane
import ARD.World

import System.Environment
import System.Exit hiding (die)

main :: IO ()
main = getArgs >>= handle

handle ["-h"] = usage exitSuccess
handle ["--help"] = usage die
handle [] = usage die
handle [filepath] = traceFile filepath
handle _ = usage die

usage exit = do
  progName <- getProgName
  putStrLn $ "USAGE: " ++ progName ++ " <path/to/file.scene>"
  exit

die = exitWith (ExitFailure 1)

traceFile filepath = do
  file <- readFile filepath
  case parseWorld filepath file of
    Left err -> putStrLn err
    Right world ->
      let
        vp = viewPlane world
        width = horizontalResolution vp
        height = verticalResolution vp
        pixels = map maxToOne . traceScene $ world
      in writeBitmapToFile width height pixels "out.bmp"

