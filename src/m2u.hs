import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


mac2unix [] = []
mac2unix (x:xs)= m2u x xs
                where
                    m2u a [] = if a==cr then [lf] else [a]                 -- *mac
                    m2u a (x:xs)
                                | a==cr && x==lf = cr:lf:(mac2unix xs)      -- windows
                                | a==cr && x/=lf = lf:x:(mac2unix xs)       -- *mac
                                | a/=cr && x==lf = a:lf:(mac2unix xs)       -- unix
                                | otherwise = a:(m2u x xs)

file2unix [] = return ()
file2unix (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.mac2unix.Bs.unpack >> file2unix xs

usage xs = putStrLn (xs ++ " is CR to LF.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2unix xs
    )
