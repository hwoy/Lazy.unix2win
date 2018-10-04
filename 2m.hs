import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment
import System.Directory

import CrLf

tomac::[Word8]->[Word8]
tomac [] = []
tomac (byte:bytes)= tom byte bytes
                where
                    tom::Word8->[Word8]->[Word8]
                    tom a [] = if a==lf then [cr] else [a]              -- *unix
                    tom a (x:xs) = case (parsing a x) of
                      WIN              -> cr:(tomac xs)                 -- *windows
                      MAC              -> cr:cr:(tomac xs)              -- mac
                      MACL left        -> left:(tom cr xs)              -- mac
                      MACR right       -> cr:right:(tomac xs)           -- mac
                      UNIX             -> cr:cr:(tomac xs)              -- *unix
                      UNIXL left       -> left:(tom lf xs)              -- unix
                      UNIXR right      -> cr:right:(tomac xs)           -- *unix
                      OTHER left right -> left:(tom right xs)           -- other


file2mac::[String]->IO()
file2mac [] = return ()
file2mac (x:xs) = let tofile = (x++".mac") in Bs.readFile x >>= (Bs.writeFile tofile).Bs.pack.tomac.Bs.unpack >> removeFile x >> renameFile tofile x >> file2mac xs

usage::[Char]->IO()
usage xs = putStrLn (xs ++ " is an any to mac converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main::IO()
main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2mac xs
    )
