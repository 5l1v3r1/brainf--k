import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import           Data.Word8
import           System.Environment

type Array = ([Word8], Word8, [Word8])

main = do
    s <- fmap head getArgs >>= B.readFile
    case parseOnly eat s of
        Left str -> putStrLn str
        Right f -> void $ f (repeat 0, 0, repeat 0)

eat :: Parser (Array -> IO Array)
eat = fmap (foldl (>=>) return) . many' $ choice
    [ char '<' ~> \(l:ls, m, rs) -> return (ls, l, m:rs)
    , char '>' ~> \(ls, m, r:rs) -> return (m:ls, r, rs)
    , char '+' ~> toMid succ
    , char '-' ~> toMid pred
    , char '.' ~> \s -> print (mid s) >> return s
    , char ',' ~> \(l, _, r) -> getLine >>= \i -> return (l, read i, r)
    , fmap block $ char '[' *> eat <* char ']'
    , anyChar ~> return
    ]

mid (_, y, _) = y
toMid f (x, y, z) = return (x, f y, z)

infix 9 ~>
v ~> c = c <$ v

block :: (Array -> IO Array) -> Array -> IO Array
block f s = case mid s of
    0 -> return s
    m -> f s >>= block f
