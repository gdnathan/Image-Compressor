{-# LANGUAGE TupleSections #-}

module Clustering.Clustering (getClusters, GravityCenter) where

import Clustering.Random (getNRandomColors)
import File.Parser (Color (..), Pixel (..), Short (..))

data GravityCenter = GravityCenter Short Short Short deriving Eq

instance Show GravityCenter where
  show (GravityCenter (Short r) (Short g) (Short b)) = '(' : show r ++ "," ++ show g ++ "," ++ show b ++ ")"

getClusters :: [Pixel] -> Int -> Float -> IO [(GravityCenter, [Pixel])]
getClusters pixels k limit = getNRandomCenters k >>= \centers -> return $ getGravityCenters centers limit pixels

getNRandomCenters :: Int -> IO [GravityCenter]
getNRandomCenters n = map convertColorToCenter <$> getNRandomColors n

convertColorToCenter :: Color -> GravityCenter
convertColorToCenter (Color r g b) = GravityCenter r g b

getGravityCenters :: [GravityCenter] -> Float -> [Pixel] -> [(GravityCenter, [Pixel])]
getGravityCenters clusters limit pixels
  | hasReachedLimit clusters (getNewGravityCenters $ clusterize clusters pixels) limit
              = clusterize clusters pixels
  | otherwise = getGravityCenters (getNewGravityCenters $ clusterize clusters pixels) limit pixels

getNewGravityCenters :: [(GravityCenter, [Pixel])] -> [GravityCenter]
getNewGravityCenters = map getNewGravityCenter

clusterize :: [GravityCenter] -> [Pixel] -> [(GravityCenter, [Pixel])]
clusterize clusters pixels = clusterize' clusters pixels (map (,[]) clusters)

clusterize' :: [GravityCenter] -> [Pixel] -> [(GravityCenter, [Pixel])] -> [(GravityCenter, [Pixel])]
clusterize' _        []       list = list
clusterize' []       _        _    = []
clusterize' clusters (p : ps) list = clusterize' clusters ps $ addPixelToGravityCenter p (fst (findPixelsGravityCenter clusters (noGravityCenter, p))) list

noGravityCenter :: GravityCenter
noGravityCenter = GravityCenter (Short $ -1) (Short $ -1) $ Short $ -1

addPixelToGravityCenter :: Pixel -> GravityCenter -> [(GravityCenter, [Pixel])] -> [(GravityCenter, [Pixel])]
addPixelToGravityCenter pixel cluster (pair@(c, pixels) : rest)
  | cluster == c                         = (c, pixel : pixels) : rest
  | otherwise                            = pair : addPixelToGravityCenter pixel cluster rest
addPixelToGravityCenter _     _       [] = []

findPixelsGravityCenter :: [GravityCenter] -> (GravityCenter, Pixel) -> (GravityCenter, Pixel)
findPixelsGravityCenter (clust : cs) (curr, pix)
  | curr == noGravityCenter                                  = findPixelsGravityCenter cs (clust, pix)
  | distanceFromPixel clust pix < distanceFromPixel curr pix = findPixelsGravityCenter cs (clust, pix)
  | otherwise                                                = findPixelsGravityCenter cs (curr, pix)
findPixelsGravityCenter []           pair                    = pair

distanceFromPixel :: GravityCenter -> Pixel -> Float
distanceFromPixel (GravityCenter r g b) (Pixel _ (Color r' g' b')) = distance (r, g, b) (r', g', b')

distance :: (Short, Short, Short) -> (Short, Short, Short) -> Float
distance (Short r, Short g, Short b) (Short r', Short g', Short b') =
  sqrt . fromIntegral $ (r - r') ^ (2 :: Int) + (g - g') ^ (2 :: Int) + (b - b') ^ (2 :: Int)

getNewGravityCenter :: (GravityCenter, [Pixel]) -> GravityCenter
getNewGravityCenter (c, [])     = c
getNewGravityCenter (_, pixels) = dividePixelsColors (sumPixelsColors pixels) (length pixels)

sumPixelsColors :: [Pixel] -> Color
sumPixelsColors = sumPixelsColors' (Color (Short 0) (Short 0) (Short 0))

sumPixelsColors' :: Color -> [Pixel] -> Color
sumPixelsColors' color                                 []                                                          = color
sumPixelsColors' (Color (Short r) (Short g) (Short b)) ((Pixel _ (Color (Short r') (Short g') (Short b'))) : rest) =
  sumPixelsColors' (Color (Short $ r + r') (Short $ g + g') (Short $ b + b')) rest

dividePixelsColors :: Color -> Int -> GravityCenter
dividePixelsColors (Color r g b) divisor = GravityCenter (divideShort r divisor) (divideShort g divisor) $ divideShort b divisor

divideShort :: Short -> Int -> Short
divideShort (Short a) divisor = Short (a `div` divisor)

hasReachedLimit :: [GravityCenter] -> [GravityCenter] -> Float -> Bool
hasReachedLimit (old : os) (new : ns) limit = hasReachedLimit' old new limit && hasReachedLimit os ns limit
hasReachedLimit _          _          _     = True

hasReachedLimit' :: GravityCenter -> GravityCenter -> Float -> Bool
hasReachedLimit' (GravityCenter r g b) (GravityCenter r' g' b') limit = distance (r, g, b) (r', g', b') < limit
