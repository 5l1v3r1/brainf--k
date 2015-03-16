import           Data.Attoparsec.ByteString.Char8
import qualified Data.Bytestring as B

-- type Array = ([Word8], [Word8])
-- type Step = StateT Array IO Word8

-- step :: Parser (Word8 -> Step)
step = fmap (foldr (>=>) return)
     $  char '<' *> \m -> StateT $ \(l:ls, rs) -> return (l, (ls, m:rs))
    <|> char '>' *> \m -> StateT $ \(ls, r:rs) -> return (r, (m:ls, rs))
    <|> char '+' *> (return . (+ 1))
    <|> char '-' *> (return . (- 1))
    <|> char ',' *> const (liftIO getChar)
    <|> char '.' *> \m -> liftIO putChar m >> return m
    <|> block <$> char '[' *> step <* char ']'

-- block :: (Word8 -> Step) -> Word8 -> Step
block s 0 = return 0
block s m = s m >>= block s

main = do
    code <- getArgs >>= (B.readFile . head)
    case parseOnly $ B.filter (`elem` "<>+-,.") code of
        Left e -> putStrLn e
        Right f -> void . execStateT . f 0 $ (repeat 0, repeat 0)
