module Crf where
import Data.Word

data CRF a = WIN | MAC a | UNIX a | OTHER a a deriving(Show)

cr::Word8
cr = 13

lf::Word8
lf = 10

parsing a b
             | a==cr && b==lf = WIN
             | a==cr && b/=lf = MAC b
             | a/=cr && b==lf = UNIX a
             |otherwise = OTHER a b
