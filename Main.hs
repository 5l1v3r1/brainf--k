import           Data.Attoparsec.ByteString.Char8
import qualified Data.Bytestring as B

type Array = ([Word8], [Word8])
type Step = Array -> IO Array

data BF s = BF (s -> IO s)

instance Monad (BF s) where
    return = BF . return

step :: Parser (StateT Array IO Word8)
step =  aux '<' (\(l:ls, m, rs) -> (ls, l, m:rs))
    <|> aux '>' (\(ls, m, r:rs) -> (m:ls, r, rs))
    <|> aux '+' (\(ls, m, rs) -> (ls, m + 1, rs))
    <|> aux '-' (\(ls, m, rs) -> (ls, m - 1, rs))
    <|> char ',' *> \(ls, m, rs) -> (\c -> (ls, c, rs)) <$> getChar
    <|> char '.' *> \(ls, m, rs) -> putChar m >> return (ls, m, rs)
    <|> (\ss (ls, m, r) -> if m == 0 then return (ls, m, r) else compose) <$> char '[' *> many' step <* char ']'

aux = (. (return .)) . (*>) . char

compose :: [Step] -> Step
compose f g arr = f arr >>= g

block :: Step -> Step
block s (ls, m, rs) =

-- compose = foldr (\f g -> (>>= g) . f) return

-- f * g = (>>= g) . f
-- f * = (. f) . (=<<)
-- * = (. (=<<)) . flip (.)

go =
