import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


unix2win [] = []
unix2win (x:xs)= u2w x xs
                where
                    u2w a [] = if a==lf then [cr,lf] else [a]
                    u2w a (x:xs)
                                | a==cr && x==lf = cr:lf:(unix2win xs)      -- windows
                                | a==cr && x/=lf = cr:x:(unix2win xs)       -- mac
                                | a/=cr && x==lf = a:cr:lf:(unix2win xs)    -- *unix
                                | otherwise = a:(u2w x xs)

file2win [] = return ()
file2win (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.unix2win.Bs.unpack >> file2win xs

usage xs = putStrLn (xs ++ " is LF to CRLF.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2win xs
    )
