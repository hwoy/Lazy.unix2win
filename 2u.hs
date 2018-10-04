import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment
import System.Directory

import CrLf

tounix::[Word8]->[Word8]
tounix [] = []
tounix (byte:bytes)= tunix byte bytes
                where
                    tunix::Word8->[Word8]->[Word8]
                    tunix a [] = if a==cr then [lf] else [a]               -- *mac
                    tunix a (x:xs) = case (parsing a x) of
                      WIN              -> lf:(tounix xs)                    -- *windows
                      MAC              -> lf:lf:(tounix xs)                 -- *mac
                      MACL left        -> left:(tunix cr xs)                -- mac
                      MACR right       -> lf:right:(tounix xs)              -- *mac
                      UNIX             -> lf:lf:(tounix xs)                 -- unix
                      UNIXL left       -> left:(tunix lf xs)                -- unix
                      UNIXR right      -> lf:right:(tounix xs)              -- unix
                      OTHER left right -> left:(tunix right xs)             -- other


file2unix::[String]->IO()
file2unix [] = return ()
file2unix (x:xs) = let tofile = (x++".unx") in Bs.readFile x >>= (Bs.writeFile tofile).Bs.pack.tounix.Bs.unpack >> removeFile x >> renameFile tofile x >> file2unix xs

usage::[Char]->IO()
usage xs = putStrLn (xs ++ " is an any to unix converter\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main::IO()
main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2unix xs
    )
