module Args.Parser (readArgs, Conf (..)) where

import Control.Exception (throw)
import Error (CompressorException (ArgumentException))
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    failureCode,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    option,
    progDesc,
    short,
    strOption,
  )

data Conf = Conf
  { colors_nb :: Int,
    convergence :: Float,
    path :: String
  }

readArgs :: IO Conf
readArgs =
  checkConf . execParser $
    info
      (helper <*> parseArgs)
      ( fullDesc
          <> header "Image Compressor: pixel k-means clustering in Haskell"
          <> progDesc "Image Compressor: pixel k-means clustering in Haskell"
          <> failureCode 84
      )

parseArgs :: Parser Conf
parseArgs =
  Conf
    <$> option
      auto
      ( short 'n'
          <> metavar "N"
          <> help "Number of colors in the final image"
      )
    <*> option
      auto
      ( short 'l'
          <> metavar "L"
          <> help "convergence limit"
      )
    <*> strOption
      ( short 'f'
          <> metavar "F"
          <> help "path to the file containing the colors of the pixels"
      )

checkConf :: IO Conf -> IO Conf
checkConf conf = conf >>= \c -> return $ checkConf' c

checkConf' :: Conf -> Conf
checkConf' c@(Conf n l _)
  | n < 0 || l < 0 = throw $ ArgumentException "Negative values are forbidden"
  | otherwise      = c
