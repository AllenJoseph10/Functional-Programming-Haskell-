

(&&&) :: Bool -> Bool -> Bool
False &&& _ = False
True  &&& x = x
