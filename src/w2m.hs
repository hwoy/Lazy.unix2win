import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


win2mac [] = []
win2mac (x:xs)= w2m x xs
                where
                    w2m a [] = [a]
                    w2m a (x:xs)
                                | a==cr && x==lf = cr:(win2mac xs)         -- *windows
                                | a==cr && x/=lf = cr:x:(win2mac xs)      -- mac
                                | a/=cr && x==lf = a:lf:(win2mac xs)       -- unix
                                | otherwise = a:(w2m x xs)

file2mac [] = return ()
file2mac (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".unx")).Bs.pack.win2mac.Bs.unpack >> file2mac xs

usage xs = putStrLn (xs ++ " is CRLF to CR.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2mac xs
    )
