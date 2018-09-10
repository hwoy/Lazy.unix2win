import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


tounix [] = []
tounix (x:xs)= tunix x xs
                where
                    tunix a [] = if a==cr then [lf] else [a]             -- *mac
                    tunix a (x:xs)
                                | a==cr && x==lf = lf:(tounix xs)         -- *windows
                                | a==cr && x/=lf = lf:x:(tounix xs)       -- *mac
                                | a/=cr && x==lf = a:lf:(tounix xs)       -- unix
                                | otherwise = a:(tunix x xs)

file2unix [] = return ()
file2unix (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".unx")).Bs.pack.tounix.Bs.unpack >> file2unix xs

usage xs = putStrLn (xs ++ " is an any to unix converter\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2unix xs
    )
