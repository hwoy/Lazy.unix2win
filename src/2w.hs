import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

cr::Word8
cr = 13

lf::Word8
lf = 10


towin [] = []
towin (x:xs)= twin x xs
                where
                    twin a [] = if (a==lf) || (a==cr) then [cr,lf] else [a] -- *unix or *mac
                    twin a (x:xs)
                                | a==cr && x==lf = cr:lf:(towin xs)      -- windows
                                | a==cr && x/=lf = cr:lf:x:(towin xs)    -- *mac
                                | a/=cr && x==lf = a:cr:lf:(towin xs)    -- *unix
                                | otherwise = a:(twin x xs)

file2win [] = return ()
file2win (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.towin.Bs.unpack >> file2win xs

usage xs = putStrLn (xs ++ " is an any to win converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2win xs
    )
