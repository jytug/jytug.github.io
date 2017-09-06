import Interpreter

import AbsTiny
import ErrM
import LexTiny
import ParTiny

import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity


import System.Environment
import System.IO

parser :: String -> Err Prog
parser = pProg . tokens

usageError :: IO ()
usageError = do
    hPutStrLn stderr "usage: ./interpreter <program file>"

main :: IO ()
main = do
    files <- getArgs
    case files of
        [file] -> do
            program <- readFile file
            case parser program of
                Ok prog   -> run prog
                Bad error -> hPutStrLn stderr error
        otherwise -> usageError
