import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

unix2win [] = []
unix2win (x:xs)= u2w x xs
                where
                    u2w a [] = if a==10 then [13,10] else [a]
                    u2w a (x:xs)
                                | a==13 && x==10 = 13:10:(unix2win xs)
                                | a/=13 && x==10 = a:13:10:(unix2win xs)
                                | otherwise = a:(u2w x xs)

file2win [] = return ()
file2win (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.unix2win.Bs.unpack >> file2win xs

usage xs = putStrLn (xs ++ " is LF to CRLF.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2win xs
    )
