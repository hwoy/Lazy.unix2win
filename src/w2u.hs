import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

win2unix [] = []
win2unix (x:xs)= w2u x xs
                where
                    w2u a [] = [a]
                    w2u a (x:xs)
                                | a==13 && x==10 = 10:(win2unix xs)
                                | a/=13 && x==10 = a:10:(win2unix xs)
                                | otherwise = a:(w2u x xs)

file2unix [] = return ()
file2unix (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".unx")).Bs.pack.win2unix.Bs.unpack >> file2unix xs

usage xs = putStrLn (xs ++ " is CRLF to LF.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2unix xs
    )
