import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


unix2mac [] = []
unix2mac (x:xs)= u2m x xs
                where
                    u2m a [] = if a==lf then [cr] else [a]                 -- *unix
                    u2m a (x:xs)
                                | a==cr && x==lf = cr:lf:(unix2mac xs)      -- windows
                                | a==cr && x/=lf = cr:x:(unix2mac xs)       -- mac
                                | a/=cr && x==lf = a:cr:(unix2mac xs)       -- *unix
                                | otherwise = a:(u2m x xs)

file2mac [] = return ()
file2mac (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.unix2mac.Bs.unpack >> file2mac xs

usage xs = putStrLn (xs ++ " is LF to CR.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2mac xs
    )
