module Error (CompressorException (..), exceptionHandler, readHandler) where


import Control.Exception (Exception)
import System.Exit (ExitCode (ExitFailure), exitWith)

data CompressorException
  = ParsingException String
  | LexingException String
  | ArgumentException String
  deriving (Show)

instance Exception CompressorException

exceptionHandler :: CompressorException -> IO ()
exceptionHandler (ParsingException  text) = putStrLn text >> exitWith (ExitFailure 84)
exceptionHandler (LexingException   text) = putStrLn text >> exitWith (ExitFailure 84)
exceptionHandler (ArgumentException text) = putStrLn text >> exitWith (ExitFailure 84)

readHandler :: IOError -> IO ()
readHandler _ = putStrLn "File doesnt exist or cannot be opened" >> exitWith (ExitFailure 84)
