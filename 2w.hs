import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment
import System.Directory

import CrLf

towin [] = []
towin (x:xs)= twin x xs
                where           
                    twin a [] = if (a==cr) || (a==lf) then [cr,lf] else [a]             -- *unix or *mac
                    twin a (x:xs) = case (parsing a x) of
                                   WIN              -> cr:lf:(towin xs)                 -- windows
                                   MAC              -> cr:lf:cr:lf:(towin xs)           -- *mac
                                   MACL left        -> left:(twin cr xs)                -- *mac
                                   MACR right       -> cr:lf:right:(towin xs)           -- *mac
                                   UNIX             -> cr:lf:cr:lf:(towin xs)           -- *unix
                                   UNIXL left       -> left:(twin lf xs)                -- *unix
                                   UNIXR right      -> cr:lf:right:(towin xs)           -- *unix
                                   OTHER left right -> left:(twin right xs)             -- other
                                   


file2win [] = return ()
file2win (x:xs) = let tofile = (x++".win") in Bs.readFile x >>= (Bs.writeFile tofile).Bs.pack.towin.Bs.unpack >> removeFile x >> renameFile tofile x >> file2win xs

usage xs = putStrLn (xs ++ " is an any to win converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2win xs
    )
