import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

data CRF a = WIN | MAC a | UNIX a | OTHER a a deriving(Show)

cr::Word8
cr = 13

lf::Word8
lf = 10


towin [] = []
towin (x:xs)= twin x xs
                where
                    parsing a b
                                | a==cr && b==lf = WIN
                                | a==cr && b/=lf = MAC b
                                | a/=cr && x==lf = UNIX a
                                |otherwise = OTHER a b
                                
                                
                    twin a [] = if (a==cr) || (a==lf) then [cr,lf] else [a]               -- *unix or *mac
                    twin a (x:xs) = case (parsing a x) of
                                   WIN                     -> cr:lf:(towin xs)             -- windows
                                   MAC right               -> cr:lf:right:(towin xs)       -- *mac
                                   UNIX left               -> left:cr:lf:(towin xs)        -- *unix
                                   OTHER left right        -> left:(twin right xs)         -- other
                                   


file2win [] = return ()
file2win (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".win")).Bs.pack.towin.Bs.unpack >> file2win xs

usage xs = putStrLn (xs ++ " is an any to win converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2win xs
    )
