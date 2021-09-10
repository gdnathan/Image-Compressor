module Clustering.Random (getNRandomColors) where

import File.Parser (Color (..), Short (..))
import System.Random (randomRIO)

getNRandomColors :: Int -> IO [Color]
getNRandomColors = sequence . getNRandomColors'

getNRandomColors' :: Int -> [IO Color]
getNRandomColors' 0 = []
getNRandomColors' n = getRandomColor : getNRandomColors' (n - 1)

getRandomColor :: IO Color
getRandomColor = getRandomShort >>= \r -> getRandomShort >>= \g -> getRandomShort >>= \b -> return $ Color r g b

getRandomShort :: IO Short
getRandomShort = Short <$> randomRIO (0, 255)
