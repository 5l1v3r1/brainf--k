import Control.Monad.Trans.State.Lazy

-- data Instr = Left | Right | Up | Down | In | Out | Block [Instr]

-- follow :: [Instr] -> ([Word8], Word8, [Word8]) -> IO ()
-- follow (Left:xs) (l:ls, m, rs) = follow xs (ls, l, m:rs)
-- follow (Right:xs) (ls, m, r:rs) = follow xs (l:ls, r, m:rs)
-- follow [] _ = return ()

main = do
    str <- getContents >>= (void . runStateT f . (++ "]"))
    void $ runState f (str ++ "]") 0 `runStateT` (repeat 0, repeat 0)

f :: State String (Word8 -> StateT ([Word8], [Word8]) IO Word8)
f = do
    p <- pop
    case p of
        -- '<' -> <=< \m -> StateT $ \(l:ls, rs) -> return (l, (ls, m:rs))
        -- '>' -> \m -> StateT $ \(ls, r:rs) -> return (r, (m:ls, rs))
        '+' -> (return . (+ 1) >=>) <$> f
        -- '-' -> return . (- 1)
        -- '.' -> \m -> liftIO putChar m >> return m
        ',' -> (const (liftIO getChar) >=>) <$> f
        '[' -> do n <- f
                  m <- f
                  return (block n >=> m)
        ']' -> return
        

eat = foldr (>=>) return <$> choice
    [ char '<' *> \m -> StateT $ \(l:ls, rs) -> return (l, (ls, m:rs))
    , char '>' *> \m -> StateT $ \(ls, r:rs) -> return (r, (m:ls, rs))
    , char '+' *> (return . (+ 1))
    , char '-' *> (return . (- 1))
    , char ',' *> const (liftIO getChar)
    , char '.' *> \m -> liftIO putChar m >> return m
    , fmap block $ char '[' *> eat <* char ']'
    ]

pop = State \x:xs -> (x, xs)

block s 0 = return 0
block s m = s m >>= block s
