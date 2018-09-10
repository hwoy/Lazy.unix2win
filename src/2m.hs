import qualified Data.ByteString.Lazy as Bs
import Data.Word
import System.Environment

data CRF a = WIN | MAC a | UNIX a | OTHER a a deriving(Show)

cr::Word8
cr = 13

lf::Word8
lf = 10


tomac [] = []
tomac (x:xs)= tom x xs
                where
                    parsing a b
                                | a==cr && b==lf = WIN
                                | a==cr && b/=lf = MAC b
                                | a/=cr && x==lf = UNIX a
                                |otherwise = OTHER a b
                    
                    tom a [] = if a==lf then [cr] else [a]                          -- *unix
                    tom a (x:xs) = case (parsing a x) of
                                   WIN                     -> cr:(tomac xs)          -- *windows
                                   MAC right               -> cr:right:(tomac xs)    -- mac
                                   UNIX left               -> left:cr:(tomac xs)     -- *unix
                                   OTHER left right        -> left:(tom right xs)    -- other


file2mac [] = return ()
file2mac (x:xs) = Bs.readFile x >>= (Bs.writeFile (x++".mac")).Bs.pack.tomac.Bs.unpack >> file2mac xs

usage xs = putStrLn (xs ++ " is an any to mac converter.\nUSAGE:: " ++ xs ++ " file1 file2 ...")

main =
    getArgs >>=
    (
      \xs -> if (length xs) <1 then getProgName >>= usage else file2mac xs
    )
