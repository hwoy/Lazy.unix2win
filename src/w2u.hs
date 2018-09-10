import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


win2unix [] = []
win2unix (x:xs)= w2u x xs
                where
                    w2u a [] = [a]
                    w2u a (x:xs)
                                | a==cr && x==lf = lf:(win2unix xs)         -- *windows
                                | a==cr && x/=lf = cr:x:(win2unix xs)       -- mac
                                | a/=cr && x==lf = a:lf:(win2unix xs)       -- unix
                                | otherwise = a:(w2u x xs)

file2unix [] = return ()
file2unix (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".unx")).Bs.pack.win2unix.Bs.unpack >> file2unix xs

usage xs = putStrLn (xs ++ " is CRLF to LF.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2unix xs
    )
