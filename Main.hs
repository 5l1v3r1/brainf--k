module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import           Data.Word
import           System.Environment

main = do
    s <- head <$> getArgs >>= B.readFile
    case parseOnly eat s of
        Left str -> putStrLn str
        Right  f -> void $ f (repeat 0, 0 :: Word8, repeat 0)

eat = fmap (foldl (>=>) return) . many' $ choice
    [ char '<' ~> \  (l:ls, m,   rs) -> return (  ls, l     , m:rs)
    , char '>' ~> \  (  ls, m, r:rs) -> return (m:ls, r     ,   rs)
    , char '+' ~> \  (  ls, m,   rs) -> return (  ls, succ m,   rs)
    , char '-' ~> \  (  ls, m,   rs) -> return (  ls, pred m,   rs)
    , char '.' ~> \s@(   _, m,    _) -> print m >> return s
    , char ',' ~> \  (  ls, _,   rs) -> getLine >>= \i -> return (ls, read i, rs)
    , fmap block $ char '[' *> eat <* char ']'
    , notChar ']' ~> return
    ]

block _ s@(_, 0, _) = return s
block f s           = f s >>= block f

infix 9 ~>
v ~> c = c <$ v
