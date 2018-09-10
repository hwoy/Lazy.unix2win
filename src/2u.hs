import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

import Crf

tounix [] = []
tounix (x:xs)= tunix x xs
                where
                    tunix a [] = if a==cr then [lf] else [a]                               -- *mac
                    tunix a (x:xs) = case (parsing a x) of
                                   WIN                     -> lf:(tounix xs)                -- *windows
                                   MAC right               -> lf:right:(tounix xs)          -- *mac
                                   UNIX left               -> left:lf:(tounix xs)           -- unix
                                   OTHER left right        -> left:(tunix right xs)         -- other


file2unix [] = return ()
file2unix (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".unx")).Bs.pack.tounix.Bs.unpack >> file2unix xs

usage xs = putStrLn (xs ++ " is an any to unix converter\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2unix xs
    )
