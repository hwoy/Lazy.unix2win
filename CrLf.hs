module CrLf where
import Data.Word

data CRLF a = WIN | MAC | MACL a | MACR a | UNIX | UNIXL a | UNIXR a | OTHER a a deriving(Show)

cr::Word8
cr = 13

lf::Word8
lf = 10

parsing::Word8->Word8->CRLF Word8
parsing a b
             | a==cr && b==lf = WIN
             | a==cr && b==cr = MAC
             | a/=lf && b==cr = MACL a
             | a==cr && b/=lf = MACR b
             | a==lf && b==lf = UNIX
             | a/=cr && b==lf = UNIXL a
             | a==lf && b/=cr = UNIXR b
             | otherwise = OTHER a b
