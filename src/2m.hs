import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

import Crf

tomac [] = []
tomac (x:xs)= tom x xs
                where
                    tom a [] = if a==lf then [cr] else [a]                          -- *unix
                    tom a (x:xs) = case (parsing a x) of
                                   WIN                     -> cr:(tomac xs)          -- *windows
                                   MAC right               -> cr:right:(tomac xs)    -- mac
                                   UNIX left               -> left:cr:(tomac xs)     -- *unix
                                   OTHER left right        -> left:(tom right xs)    -- other


file2mac [] = return ()
file2mac (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".mac")).Bs.pack.tomac.Bs.unpack >> file2mac xs

usage xs = putStrLn (xs ++ " is an any to mac converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2mac xs
    )
