import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


mac2win [] = []
mac2win (x:xs)= m2w x xs
                where
                    m2w a [] = if a==cr then [cr,lf] else [a]                    -- *mac
                    m2w a (x:xs)
                                | a==cr && x==lf = cr:lf:(mac2win xs)             -- windows
                                | a==cr && x/=lf = cr:lf:x:(mac2win xs)           -- *mac
                                | a/=cr && x==lf = a:lf:(mac2win xs)              -- unix
                                | otherwise = a:(m2w x xs)

file2win [] = return ()
file2win (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.mac2win.Bs.unpack >> file2win xs

usage xs = putStrLn (xs ++ " is CR to CRLF.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2win xs
    )
