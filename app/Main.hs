module Main (main) where

import Args.Parser (Conf (Conf), readArgs)
import Clustering.Clustering (getClusters)
import Control.Exception (catch)
import Display (display)
import Error (exceptionHandler, readHandler)
import File.ReadFile (getPixelsFromFile)
import Prelude (IO, (>>=))

main :: IO ()
main = catch (readArgs >>= imageCompressor) exceptionHandler

imageCompressor :: Conf -> IO ()
imageCompressor (Conf k limit file) =
  catch
    ( getPixelsFromFile file
        >>= \pixels ->
          getClusters pixels k limit >>= display
    )
    readHandler
