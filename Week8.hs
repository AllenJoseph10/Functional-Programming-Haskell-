fibm :: Monad m => Integer -> m Integer
fibm 0 = pure 0
fibm 1 = pure 1
fibm n = do
         x <- fibm (n-2)
         y <- fibm (n-1)
         pure (x+y)
         
