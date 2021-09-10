module Display (display) where

import Clustering.Clustering (GravityCenter)
import File.Parser (Pixel)

display :: [(GravityCenter, [Pixel])] -> IO ()
display = mapM_ printClusterPixels

printClusterPixels :: (GravityCenter, [Pixel]) -> IO ()
printClusterPixels (cluster, pixels) = putStrLn "--" >> print cluster >> putStrLn "-" >> mapM_ print pixels
