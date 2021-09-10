module File.Parser (parse, Point (..), Short (..), Color (..), Pixel (..)) where

import Control.Exception (throw)
import Error (CompressorException (ParsingException))
import File.Lexer (FileToken (..))
import Text.Read (readMaybe)

data Point = Point Int Int

newtype Short = Short Int deriving (Eq)

data Color = Color Short Short Short deriving (Eq)

instance Show Color where
  show (Color (Short r) (Short g) (Short b)) = '(' : show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Pixel = Pixel Point Color

instance Show Pixel where
  show (Pixel (Point x y) color) = '(' : show x ++ "," ++ show y ++ ") " ++ show color

parse :: [[FileToken]] -> [Pixel]
parse ([OPEN, Value x, COMMA, Value y, CLOSE, SPACE, OPEN, Value r, COMMA, Value g, COMMA,Value b, CLOSE] : xs)
            = Pixel (getPoint x y) (getColor r g b) : parse xs
parse []    = []
parse _     = throw $ ParsingException "Invalid Line format"

getPoint :: String -> String -> Point
getPoint x y = Point (readInt x) (readInt y)

getColor :: String -> String -> String -> Color
getColor r g b = Color (readShort r) (readShort g) (readShort b)

readInt :: String -> Int
readInt = validatePositive . readNum

readShort :: String -> Short
readShort = validateShort . readNum

readNum :: String -> Int
readNum = validateMaybe . readMaybe

validateMaybe :: Maybe a -> a
validateMaybe (Just a) = a
validateMaybe _        = throw $ ParsingException "Value was not an integer"

validatePositive :: Int -> Int
validatePositive i
  | i >= 0    = i
  | otherwise = throw $ ParsingException "Value was not positive"

validateShort :: Int -> Short
validateShort i
  | i >= 0 && i <= 255 = Short i
  | otherwise          = throw $ ParsingException "Value was not a valid short"
