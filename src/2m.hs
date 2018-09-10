import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


tomac [] = []
tomac (x:xs)= tom x xs
                where
                    tom a [] = if a==lf then [cr] else [a]              -- *unix
                    tom a (x:xs)
                                | a==cr && x==lf = cr:(tomac xs)         -- *windows
                                | a==cr && x/=lf = cr:x:(tomac xs)       -- mac
                                | a/=cr && x==lf = a:cr:(tomac xs)       -- *unix
                                | otherwise = a:(tom x xs)

file2mac [] = return ()
file2mac (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".mac")).Bs.pack.tomac.Bs.unpack >> file2mac xs

usage xs = putStrLn (xs ++ " is an any to mac converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2mac xs
    )
